
library("data.table")
library("tidyverse")
library("Matrix")
source("R/01_tidy_functions.R")

regions <- fread("inst/regions.csv")
items <- fread("inst/products.csv")
years <- 1997:2017

cbs <- readRDS("data/cbs.rds")
sup <- readRDS("data/sup.rds")
use_items <- fread("inst/use.csv")
source_use <- fread("inst/source_use.csv")

# Use ---------------------------------------------------------------------

# Split use in energy and nonenergy
cbs[, energy := 0]
cbs <- rename(cbs, "processing" = "use")
cbs[com_code %in% c("c15","c16"), `:=`(energy = processing, processing = 0)]
# cbs[com_code %in% c("c03","c17","c18"), `:=`(energy = processing * 0.5, processing = processing * 0.5)]


# Create long use table
use <- merge(
  cbs[, c("area_code", "area", "year", "com_code", "item", "production", "processing", "energy")],
  use_items[, .(proc_code, process, com_code, item, type)],
  by = c("com_code", "item"), all = TRUE, allow.cartesian = TRUE)
use[, use := NA_real_]



# TCF ---------------------------------------------------------------------

cat("Allocating part of the TCF commodities to TCF use. Applies to items:\n\t",
  paste0(unique(use[type %in% c("tcf", "tcf_cnc", "tcf_pellets"), item]), collapse = "; "),
  "\nused for processes \n\t",
  paste0(unique(use[type %in% c("tcf", "tcf_cnc", "tcf_pellets"), process]), collapse = "; "),
  ".\n", sep = "")

tcf <- fread("inst/tcf_use_tidy.csv")
tcf <- tcf[!(com_code=="c13" & unit=="m3sw/tonne") & unit!="kg/m3p"]
tcf[com_code=="c10", `:=`(source = items$item[items$com_code=="c01"], source_code = "c01")]
osb <- tcf[com_code=="c10"]
osb[, `:=`(source = items$item[items$com_code=="c02"], source_code = "c02")]
tcf <- rbind(tcf, osb)
rm(osb)

# Technical conversion factors for each input-output combination
tcf <- merge(source_use[type %in% c("tcf", "tcf_cnc"), .(proc_code, process, source_code, source, com_code, item)], 
  tcf[, .(com_code, item, source_code, source, area_code, area, unit, tcf)], 
  by = c("source_code", "source", "com_code", "item"), all.x = TRUE)

# Technical conversion factors for pellets
tcf_use <- fread("inst/tcf_use_tidy.csv")
tcf_out <- tcf_use[com_code=="c15" & unit=="m3sw/tonne"]
tcf_in <- tcf_use[com_code %in% source_use[type=="tcf_pellets", source_code] & unit %in% c("m3rw/m3p", "m3p/m3sw")]
tcf_in[unit=="m3rw/m3p", `:=`(tcf = 1 / tcf, unit = "m3p/m3rw")]
# unique(tcf_in[, .(item, source, unit)])
# unique(tcf_out[, .(item, source, unit)])
tcf_pellets <- merge(source_use[type=="tcf_pellets"], 
  tcf_in[, .(area_code,source_code=com_code,tcf_in=tcf)],
  by = c("source_code"), allow.cartesian = TRUE)
tcf_pellets <- merge(tcf_pellets, 
  tcf_out[, .(area_code,com_code,tcf_out=tcf)],
  by = c("area_code", "com_code"))
tcf_pellets[, tcf := (tcf_in * tcf_out)]
tcf_pellets[, `:=`(unit = "m3p/tonne", tcf_in = NULL, tcf_out = NULL, type = NULL,
  area = regions$area[match(tcf_pellets$area_code, regions$area_code)])]
tcf <- rbind(tcf, tcf_pellets)

tcf_codes <- list(sort(unique(cbs$area_code[cbs$area_code %in% tcf$area_code])), sort(unique(tcf$com_code)),
  sort(unique(tcf$source_code)))

# Create tcf matrices for each country
Cs <- lapply(tcf_codes[[1]], function(x) {
  out <- data.table::dcast(tcf[area_code == x], com_code ~ source_code, fill = 0,
    fun.aggregate = na_sum, value.var = "tcf")
  setkey(out, com_code)
  out <- as(out[, -1], "Matrix")
})
Cs <- lapply(Cs, `dimnames<-`, list(tcf_codes[[2]], tcf_codes[[3]]))
names(Cs) <- tcf_codes[[1]]

# Get all relevant input and output data
tcf_data <- use[area_code %in% tcf_codes[[1]] &
  (com_code %in% tcf_codes[[2]] | com_code %in% tcf_codes[[3]]),
  .(year, area_code, com_code, production, processing)]
tcf_data <- tcf_data[!duplicated(tcf_data), ] # Duplicates from proc_code
setkey(tcf_data, year, area_code, com_code)
areas <- tcf_codes[[1]]

# Production in processes
output <- tcf_data[data.table(expand.grid(year = years,
  area_code = areas, com_code = tcf_codes[[2]]))]
output[, `:=`(value = production, production = NULL, processing = NULL)]
dt_replace(output, is.na, 0, cols = "value")

# Processing of source items
input <- tcf_data[data.table(expand.grid(year = years,
  area_code = areas, com_code = tcf_codes[[3]]))]
input[, `:=`(value = processing, production = NULL, processing = NULL)]
dt_replace(input, is.na, 0, cols = "value")

# Prepare template to be filled - pocessing per process
results <- tcf_data[data.table(expand.grid(year = years, area_code = areas,
  com_code = tcf_codes[[3]], com_code_proc = tcf_codes[[2]]))]
setkey(results, com_code, com_code_proc)
results[, `:=`(value = 0, production = NULL, processing = NULL)]

for(x in years) {
  cat(paste0("Calculating processing inputs for ", x, ".\n"))
  output_x <- output[year == x, ]
  input_x <- input[year == x, ]
  for(y in areas) {
    output_y <- output_x[area_code == y, value]
    input_y <- input_x[area_code == y, value]
    # Skip if no data is available
    if(all(output_y == 0) || all(input_y == 0)) {next}
    out <- split_tcf(y = output_y, z = input_y,
      C = Cs[[as.character(y)]], cap = FALSE)
    if(length(out) == 1 && is.na(out)) {next}
    results[year == x & area_code == y &
      com_code_proc %in% out$com_code_proc, # com_code is always ordered
      value := out$value]
  }
}

results <- results[paste(com_code,com_code_proc) %in%
  paste(source_use$source_code, source_use$com_code)]

results[, `:=`(proc_code =
  source_use[match(results$com_code_proc, source_use$com_code), proc_code])]


# Redistribute c/nc use according to availability 
# for c06 veneer, c07 plywood, c10 osb
data <- merge(results[com_code %in% c("c01","c02"),], 
  source_use[, .(com_code=source_code, com_code_proc = com_code, type)], 
  by=c("com_code", "com_code_proc"))
# sum per type, i.e. tcf and tcf_cnc
data <- data[, list(value = sum(value, na.rm = TRUE)), 
  by = c("com_code", "year", "area_code", "type")]
data <- spread(data, type, value)
data <- merge(data, cbs[, .(area_code, com_code, year, processing)],
  by = c("area_code", "com_code", "year"))
data[, `:=`(rest = processing - tcf, tcf = NULL)]
# sum up remaining roundwood per country
totals <- data[, list(processing_total = sum(processing), rest_total = sum(rest)), 
  by = c("year", "area_code")]
totals[, `:=`(processing_total = ifelse(processing_total < 0, 0, processing_total), 
  rest_total = ifelse(rest_total < 0, 0, rest_total))]
data <- merge(data, totals[, .(year, area_code, processing_total, rest_total)],
  by = c("year", "area_code"))
# derive shares of remaining c & nc roundwood
data[, `:=`(rest = rest / rest_total, processing = processing / processing_total)]
data[, `:=`(rest = ifelse(!is.finite(rest), 0, rest), 
  processing = ifelse(!is.finite(processing), 0, processing))]
data[, `:=`(rest = ifelse(rest > 1, 1, rest), 
  processing = ifelse(processing > 1, 1, processing))]
data[, `:=`(rest = ifelse(rest < 0, 0, rest), 
  processing = ifelse(processing < 0, 0, processing))]
data[, `:=`(share = ifelse(rest_total==0, processing, rest))]

# merge shares into results and apply to type 'tcf_cnc'
results <- merge(results, data[, .(year, area_code, com_code, share)],
  by = c("year", "area_code", "com_code"), all.x = TRUE)
results <- merge(results, source_use[, .(com_code=source_code, com_code_proc = com_code, type)], 
  by=c("com_code", "com_code_proc"))
results[type=="tcf_cnc", `:=`(value = value * share)]
results[, share := NULL]


# Redistribute input use for pellets production
data <- results[type=="tcf_pellets", .(com_code, year, area_code, value)]
data <- merge(data, cbs[, .(area_code, com_code, year, processing)],
  by = c("area_code", "com_code", "year"))
data <- merge(data, data[com_code=="c18", .(year, area_code, share = processing / value)],
  by = c("year", "area_code"), all.x = TRUE)
data[com_code=="c18", `:=`(value = round(ifelse(share < 1, value * share, value)))]
data[com_code %in% c("c03","c17"), `:=`(value = ifelse(share < 1, value * (1 - share), 0))]

data <- merge(data, tcf_pellets[, .(area_code, com_code = source_code, tcf)],
  by = c("area_code", "com_code"))
data[, potential := processing / tcf]
data <- merge(data, cbs[com_code=="c15", .(year, area_code, production)],
  by = c("year", "area_code"))

data <- merge(data, data[com_code!="c18", list(processing_total = sum(processing, na.rm = TRUE)), 
  by = c("year", "area_code")], by = c("year", "area_code"), all.x = TRUE)
data[, share := processing / processing_total]
data[com_code %in% c("c03","c17"), value := round(value * share)]
data[!is.finite(value), value := 0]

# replace pellets rows in results
results <- rbind(results[type != "tcf_pellets"], 
  data[, .(com_code, com_code_proc = "c15", year, area_code, value, proc_code = "p15", type = "tcf_pellets")])

# Add to use (per item and process)
use <- merge(use, results[, .(year, area_code, proc_code, com_code, value)],
  by = c("year", "area_code", "proc_code", "com_code"), all.x = TRUE)
use[!is.na(value), `:=`(use = value)]
use[, value := NULL]

# Subtract from cbs processing (per item)
cbs <- merge(cbs, results[, list(value = na_sum(value)),
  by = c("year", "area_code", "com_code")],
  by = c("area_code", "year", "com_code"), all.x = TRUE)
cbs[!is.na(value), processing := round(na_sum(processing, -value))]
cbs[, value := NULL]

# Update processing in use
use[, processing := NULL]
use <- merge(use, cbs[, .(area_code, year, com_code, processing)],
  by = c("area_code", "year", "com_code"), all.x = TRUE)

rm(tcf, tcf_codes, tcf_data, areas, out, totals, data,
   results, Cs, input, output, input_x, output_x, input_y, output_y)




# 100% processes ------------------------------------------------------

cat("Allocating items going directly to a process.\n\t",
    paste0(unique(use[type %in% c("100%","tcf_fill"), item]), collapse = "; "),
    ".\n", sep = "")
use[type %in% c("100%","tcf_fill"), use := ifelse(processing > 0, processing, 0)]
use[type %in% c("100%","tcf_fill"), processing := processing - use]

# Reduce processing in CBS
cbs <- merge(cbs, use[type %in% c("100%","tcf_fill") & !is.na(use) & use > 0,
  c("area_code", "year", "com_code", "use")],
  by = c("area_code", "year", "com_code"), all.x = TRUE)
cbs[!is.na(use), processing := na_sum(processing, -use)]
cbs[, use := NULL]




# TCF fill ------------------------------------------------------
# we have allocated all remaining recovered paper to p14 paper production






#-------------#-----------#------------#--------------#-------------#-----------------#
# Hier weiter...




# Optimise  ------------------------------------------------------

# Allocate feedstocks to the production of alcoholic beverages and sweeteners
opt_in <- unique(source_use[type == "optim", .(com_code = source_code, item = source)])
opt_in <- opt_in[com_code != "c13"]   # exclude c13 (Recovered fibre pulp) since we fully allocated it to paper
opt_out <- unique(source_use[type == "optim", .(com_code, item)])
tcf <- fread("inst/tcf_use_tidy.csv")
tcf_out <- tcf[com_code %in% opt_out$com_code & unit %in% c("m3sw/m3p", "m3sw/tonne")]
tcf_in <- tcf[com_code %in% opt_in$com_code & unit %in% c("m3rw/m3p", "m3p/m3sw")]
tcf_in[unit=="m3rw/m3p", `:=`(tcf = 1 / tcf, unit = "m3p/m3rw")]
# unique(tcf_in[, .(item, source, unit)])
# unique(tcf_out[, .(item, source, unit)])
opt_tcf <- merge(source_use[type=="optim" & com_code != "c13"], 
  tcf_in[, .(area_code,source_code=com_code,tcf_in=tcf)],
  by = c("source_code"), allow.cartesian = TRUE)
opt_tcf <- merge(opt_tcf, 
  tcf_out[, .(area_code,com_code,tcf_out=tcf)],
  by = c("area_code", "com_code"))
opt_tcf[, tcf := 1 / (tcf_in * tcf_out)]
setnames(opt_tcf, "source_code", "inp_code")
setnames(opt_tcf, "com_code", "out_code")

# Add processing / production information from the balances
input <- merge(opt_in,
  cbs[year %in% years, c("area_code", "year", "com_code", "processing")],
  by = "com_code", all.x = TRUE)
input <- input[is.finite(processing) & processing > 0]
output <- merge(opt_out,
  cbs[year %in% years, c("area_code", "year", "com_code", "production")],
  by = "com_code", all.x = TRUE)
output <- output[is.finite(production) & production > 0]

# Weights are in-out ratio (to bypass e.g. high water contents of beer)
weight_out <- opt_tcf[, list(weight = mean(tcf, na.rm = TRUE)),
  by = c("out_code", "area_code")]


# Optimise allocation -----
# This takes a very long time! Six cores are working in parallel for ~20 hours.
results <- lapply(sort(unique(input$area_code)), function(x) {
  # Per area
  inp_x <- input[area_code == x, ]
  out_x <- output[area_code == x, ]
  tcf_x <- opt_tcf[area_code == x, ]
  wt_x <- weight_out[area_code == x, ]
  res <- lapply(sort(unique(input$year)), function(y) {
    # Per year
    inp_xy <- inp_x[year == y, ]
    out_xy <- out_x[year == y, ]
    # Skip optimisation if no data is available
    if(inp_xy[, .N] == 0 || out_xy[, .N] == 0) {return(NULL)}
    tcf_xy <- tcf_x[inp_code %in% inp_xy$com_code &
      out_code %in% out_xy$com_code, ]
    wt_xy <- wt_x[out_code %in% out_xy$com_code, ]
    # Optimise on country x & year y
    opt <- optim(par = rep(0, nrow(tcf_xy)), # To-do: vectorise further
      fn = function(par) {
        I <- tcf_xy[, .(inp_code, par = par)][,
          list(x = na_sum(par)), by = c("inp_code")]
        O <- tcf_xy[, .(out_code, par = par * tcf)][,
          list(x = na_sum(par)), by = c("out_code")]
        # Get absolute deviations from target (ensuring correct order, output deviations weighted)
        prod_tgt <- out_xy$production[match(O$out_code, out_xy$com_code)]
        prod_err <- abs(prod_tgt - O$x) / wt_xy$weight
        proc_tgt <- inp_xy$processing[match(I$inp_code, inp_xy$com_code)]
        proc_err <- abs(proc_tgt - I$x)
        # Get relative deviations from target weighted by maximum absolute error
        prod_err_rel <- abs(prod_tgt - O$x) / prod_tgt * max(prod_err)
        proc_err_rel <- abs(proc_tgt - I$x) / proc_tgt * max(proc_err)
        # Sum of squared absolute deviations + 50% of weighted relative deviation
        return(sum(prod_err^2) + sum(proc_err^2) + (sum(prod_err_rel^2) + sum(proc_err_rel^2)) / 2)
    }, method = "L-BFGS-B", lower = 0, upper = Inf)
    # check results
    # data <- dplyr::mutate(tcf_xy, result_in = opt$par, year = y)
    tcf_xy[, .(area_code, inp_code, out_code, tcf_in, tcf_out, tcf,
      result_in = opt$par, year = y)]
  })
  return(rbindlist(res))
})

results <- rbindlist(results)
results[, result_in := round(result_in)]
saveRDS(results, paste0("./data/optim_results_",Sys.Date(),".rds"))
# results <- readRDS("./data/optim_results_2021-02-18.rds")

# Add process information
results[, proc_code := ifelse(out_code == 2658, "p083",
  ifelse(out_code == 2657, "p082", ifelse(out_code == 2656, "p081", "p066")))]

# Add optimisation results to use (full detail) and cbs (item detail)
use <- merge(use,
  results[, .(area_code, year, com_code = inp_code, type = "optim",
    proc_code, result_in)],
  by = c("area_code", "year", "com_code", "proc_code", "type"), all.x = TRUE)
use[!is.na(result_in) & type == "optim", use := result_in]
use[, result_in := NULL]

cbs <- merge(cbs,
  results[, list(result_in = na_sum(result_in)),
    by = .(area_code, year, com_code = inp_code)],
  by = c("area_code", "year", "com_code"), all.x = TRUE)
cbs[!is.na(result_in), processing := na_sum(processing, -result_in)]
cbs[, result_in := NULL]




# Allocate final demand ------------------------------------------------------
use_fd <- cbs[, c("year", "comm_code", "area_code", "area", "com_code", "item",
  "food", "other", "losses", "stock_addition", "balancing", "unspecified")]


# Remove unneeded variables
use <- use[, c("year", "area_code", "area", "comm_code", "com_code", "item",
               "proc_code", "proc", "type", "use")]


# Save ------------------------------------------------------

saveRDS(cbs, "data/cbs_final.rds")
saveRDS(use, "data/use_final.rds")
saveRDS(use_fd, "data/use_fd_final.rds")
saveRDS(sup, "data/sup_final.rds")
