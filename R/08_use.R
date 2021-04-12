
library("data.table")
library("tidyverse")
library("Matrix")
source("R/01_tidy_functions.R")

regions <- fread("inst/regions.csv")
items <- fread("inst/products.csv")

cbs <- readRDS("data/cbs.rds")
sup <- readRDS("data/sup.rds")

#use_items <- fread("inst/use.csv")
#ZR: use_new.csv excludes item_output, com_code_output.
use_items <- fread("inst/use_new.csv")

# Use ---------------------------------------------------------------------

# Split use in energy and nonenergy
cbs[, energy := 0]
cbs <- rename(cbs, "processing" = "use")
cbs[com_code %in% c("c15","c16"), `:=`(energy = processing, processing = 0)]
cbs[com_code %in% c("c03","c17","c18"), `:=`(energy = processing * 0.5, processing = processing * 0.5)]


# ZR: Create long use table
use <- merge(
  cbs[, c("area_code", "area", "year", "com_code", "item", "production", "processing", "energy")],
  use_items[, .(proc_code, process, com_code, item, type)],
  by = c("com_code", "item"), all = TRUE, allow.cartesian = TRUE)
use[, use := NA_real_]

# OLD FORBIO 
# Create long use table
# use <- merge(
#   cbs[, c("area_code", "area", "year", "com_code", "item", "production", "processing", "energy")],
#   use_items[, .(proc_code, process, com_code, item, share, type)],
#   by = c("com_code", "item"), all = TRUE, allow.cartesian = TRUE)
# use[, use := NA_real_]

# FABIO
# use <- merge(
#   cbs[, c("area_code", "area", "year", "item_code", "item", "production", "processing")],
#   use_items[item_code %in% unique(cbs$item_code)],
#   by = c("item_code", "item"), all = TRUE, allow.cartesian = TRUE)
# use[, use := NA_real_]




# TCF ---------------------------------------------------------------------

cat("Allocating part of the TCF commodities to TCF use. Applies to items:\n\t",
  paste0(unique(use[type == "tcf", item]), collapse = "; "),
  "\n\tused for processes \n\t",
  paste0(unique(use[type == "tcf", process]), collapse = "; "),
  ".\n", sep = "")

tcf <- fread("inst/tcf_use_tidy.csv")
tcf <- tcf[!(com_code=="c13" & unit=="m3sw/tonne")]

# ZR: Basically the same old use.csv with FABIO names and without "share"
source_use <- fread("inst/source_use.csv")

# ZR: Merge tcf and estimates_use to get a similar structure as FABIO's tcf_cbs
tcf <- merge(source_use[type=="tcf", .(proc_code, process, source_code, source, com_code, item)], 
             tcf[, .(com_code, item, area_code, area, unit, tcf)], 
             by = c("com_code", "item"), all.x = TRUE)

# # OLD FORBIO
# tcf <- merge(use_items[type=="tcf", .(proc_code, process, com_code, item, com_code_output, item_output)], 
#   tcf[, .(com_code_output = com_code, item_output = item, area_code, area, unit, tcf)], 
#   by = c("com_code_output", "item_output"), all.x = TRUE)

# ZR 
tcf_codes <- list(sort(unique(cbs$area_code[cbs$area_code %in% tcf$area_code])), sort(unique(tcf$com_code)),
  sort(unique(tcf$source_code)))
Cs <- lapply(tcf_codes[[1]], function(x) {
  out <- data.table::dcast(tcf[area_code == x], com_code ~ source_code, fill = 0,
    fun.aggregate = na_sum, value.var = "tcf")
  setkey(out, com_code)
  out <- as(out[, -1], "Matrix")
})
Cs <- lapply(Cs, `dimnames<-`, list(tcf_codes[[2]], tcf_codes[[3]]))
names(Cs) <- tcf_codes[[1]]

# # OLD FORBIO
# tcf_codes <- list(sort(unique(cbs$area_code[cbs$area_code %in% tcf$area_code])), sort(unique(tcf$com_code_output)),
#   sort(unique(tcf$com_code)))
# Cs <- lapply(tcf_codes[[1]], function(x) {
#   out <- data.table::dcast(tcf[area_code == x], com_code_output ~ com_code, fill = 0,
#     fun.aggregate = na_sum, value.var = "tcf")
#   setkey(out, com_code_output)
#   out <- as(out[, -1], "Matrix")
# })
# Cs <- lapply(Cs, `dimnames<-`, list(tcf_codes[[2]], tcf_codes[[3]]))
# names(Cs) <- tcf_codes[[1]]

# FABIO
# tcf_codes <- list(sort(unique(cbs$area_code[cbs$area_code %in% tcf_cbs$area_code])), sort(unique(tcf_cbs$item_code)),
#                   sort(unique(tcf_cbs$source_code)))
# Cs <- lapply(tcf_codes[[1]], function(x) {
#   out <- data.table::dcast(tcf_cbs[area_code == x], item_code ~ source_code, fill = 0,
#                            fun.aggregate = na_sum, value.var = "tcf")
#   setkey(out, item_code)
#   out <- as(out[, -1], "Matrix")
# })
# Cs <- lapply(Cs, `dimnames<-`, list(tcf_codes[[2]], tcf_codes[[3]]))
#names(Cs) <- tcf_codes[[1]]


tcf_data <- use[area_code %in% tcf_codes[[1]] &
  (com_code %in% tcf_codes[[2]] | com_code %in% tcf_codes[[3]]),
  .(year, area_code, com_code, production, processing)]
tcf_data <- tcf_data[!duplicated(tcf_data), ] # Duplicates from proc_code
setkey(tcf_data, year, area_code, com_code)
years <- sort(unique(tcf_data$year))
areas <- tcf_codes[[1]]

# FABIO
# tcf_data <- use[area_code %in% tcf_codes[[1]] &
#                   (item_code %in% c(tcf_codes[[2]]) | item_code %in% tcf_codes[[3]]),
#                 .(year, area_code, item_code, production, processing)]
# tcf_data <- tcf_data[!duplicated(tcf_data), ] # Duplicates from proc_code
# setkey(tcf_data, year, area_code, item_code)
# years <- sort(unique(tcf_data$year))
# areas <- tcf_codes[[1]]


# Production in processes
output <- tcf_data[data.table(expand.grid(year = years,
  area_code = areas, com_code = tcf_codes[[2]]))]
output[, `:=`(value = production, production = NULL, processing = NULL)]
dt_replace(output, is.na, 0, cols = "value")

# FABIO
# # Production in processes
# output <- tcf_data[data.table(expand.grid(year = years,
#                                           area_code = areas, item_code = tcf_codes[[2]]))]
# output[, `:=`(value = production, production = NULL, processing = NULL)]
# dt_replace(output, is.na, 0, cols = "value")


# Processing of source items
input <- tcf_data[data.table(expand.grid(year = years,
  area_code = areas, com_code = tcf_codes[[3]]))]
input[, `:=`(value = processing, production = NULL, processing = NULL)]
dt_replace(input, is.na, 0, cols = "value")

# FABIO
# # Processing of source items
# input <- tcf_data[data.table(expand.grid(year = years,
#                                          area_code = areas, item_code = tcf_codes[[3]]))]
# input[, `:=`(value = processing, production = NULL, processing = NULL)]
# dt_replace(input, is.na, 0, cols = "value")


# Processing per process - to fill
results <- tcf_data[data.table(expand.grid(year = years, area_code = areas,
  com_code = tcf_codes[[2]], com_code_proc = tcf_codes[[3]]))]
setkey(results, com_code, com_code_proc)
results[, `:=`(value = 0, production = NULL, processing = NULL)]

# FABIO
# # Processing per process - to fill
# results <- tcf_data[data.table(expand.grid(year = years, area_code = areas,
#                                            item_code = tcf_codes[[3]], item_code_proc = tcf_codes[[2]]))]
# setkey(results, item_code, item_code_proc)
# results[, `:=`(value = 0, production = NULL, processing = NULL)]


for(x in years) {
  cat(paste0("Calculating processing inputs for ", x, "."))
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

# FABIO
# for(x in years) {
#   output_x <- output[year == x, ]
#   input_x <- input[year == x, ]
#   for(y in areas) {
#     output_y <- output_x[area_code == y, value]
#     input_y <- input_x[area_code == y, value]
#     # Skip if no data is available
#     if(all(output_y == 0) || all(input_y == 0)) {next}
#     out <- split_tcf(y = output_y, z = input_y,
#                      C = Cs[[as.character(y)]], cap = TRUE)
#     if(length(out) == 1 && is.na(out)) {next}
#     results[year == x & area_code == y &
#               item_code_proc %in% out$item_code_proc, # item_code is always ordered
#             value := out$value]
#   }
# }

# ZR: Starting here is not working...
results <- results[paste(com_code,com_code_proc) %in%
  paste(use_items$com_code, use_items$source_code)]

results[, `:=`(proc_code =
   use_items$proc_code[match(paste(results$com_code_proc, results$com_code),
  paste(use_items$source_code, use_items$com_code))])]


# OLD FORBIO
# results <- results[paste(com_code,com_code_proc) %in% 
#   paste(use_items$com_code_output, use_items$com_code)]
# 
# results[, `:=`(proc_code =
#   use_items$proc_code[match(paste(results$com_code_proc, results$com_code), 
#   paste(use_items$com_code, use_items$com_code_output))])]

# FABIO 
# results[, `:=`(proc_code =
#                  tcf_cbs[match(results$item_code_proc, tcf_cbs$item_code), proc_code],
#                item_code_proc = NULL)]

# Add to use (per item and process)
use <- merge(use, results[, .(year, area_code, proc_code, com_code = com_code_proc, value)],
             by = c("year", "area_code", "proc_code", "com_code"), all.x = TRUE)
use[!is.na(value), `:=`(use = value)]
use[, value := NULL]

# FABIO 
# # Add to use (per item and process)
# use <- merge(use, results,
#              by = c("year", "area_code", "proc_code", "item_code"), all.x = TRUE)
# use[!is.na(value), `:=`(use = value)]
# use[, value := NULL]

# ZR
# Subtract from cbs processing (per item)
cbs <- merge(cbs, results[, list(value = na_sum(value)),
                          by = c("year", "area_code", "com_code")],
             by = c("area_code", "year", "com_code"), all.x = TRUE)
cbs[!is.na(value), processing := round(na_sum(processing, -value))]
cbs[, value := NULL]

# # Subtract from cbs processing (per item)
# cbs <- merge(cbs, results[, list(value = na_sum(value)),
#   by = c("year", "area_code", "com_code_proc")],
#   by.x = c("area_code", "year", "com_code"), 
#   by.y = c("area_code", "year", "com_code_proc"), all.x = TRUE)
# cbs[!is.na(value), processing := round(na_sum(processing, -value))]
# cbs[, value := NULL]

# FABIO 
# # Subtract from cbs processing (per item)
# cbs <- merge(cbs, results[, list(value = na_sum(value)),
#                           by = c("year", "area_code", "item_code")],
#              by = c("area_code", "year", "item_code"), all.x = TRUE)
# cbs[!is.na(value), processing := round(na_sum(processing, -value))]
# cbs[, value := NULL]

rm(tcf, tcf_codes, tcf_data, years, areas, out,
   results, Cs, input, output, input_x, output_x, input_y, output_y)

# rm(tcf, tcf_codes, tcf_data, years, areas, out,
  # results, Cs, input, output, input_x, output_x, input_y, output_y)



# TCF c/nc ------------------------------------------------------





# 100% processes ------------------------------------------------------

cat("Allocating items going directly to a process.",
    paste0(unique(use[type == "100%", item]), collapse = "; "),
    ".\n", sep = "")
use[type == "100%", `:=`(use = processing, processing = 0)]

# Reduce processing in CBS
cbs <- merge(cbs, use[type == "100%" & !is.na(use) & use > 0,
                      c("area_code", "year", "com_code", "use")],
             by = c("area_code", "year", "com_code"), all.x = TRUE)
cbs[!is.na(use), processing := na_sum(processing, -use)]
cbs[, use := NULL]




# TCF fill ------------------------------------------------------







# Optimise  ------------------------------------------------------

# Allocate feedstocks to the production of alcoholic beverages and sweeteners
opt_tcf <- fread("inst/tcf_optim.csv")
opt_in <- fread("inst/optim_in.csv")
opt_out <- fread("inst/optim_out.csv")

# Add processing / production information from the balances
input <- merge(opt_in,
  cbs[, c("area_code", "year", "item_code", "processing")],
  by = "item_code", all.x = TRUE)

input <- input[is.finite(processing) & processing > 0]
output <- merge(opt_out,
  cbs[, c("area_code", "year", "item_code", "production")],
  by = "item_code", all.x = TRUE)
output <- output[is.finite(production) & production > 0]

# We have some NAs in the TCFs
opt_tcf <- opt_tcf[is.finite(value), ]

# Subset to needed TCFs (shold be all) and get weights (i.e. mean values)
# Weights are in-out ratio (to bypass e.g. high water contents of beer)
opt_tcf <- opt_tcf[inp_code %in% opt_in$item_code &
  out_code %in% opt_out$item_code, ]
weight_out <- opt_tcf[, list(weight = mean(value, na.rm = TRUE)),
  by = c("out_code", "area_code")]

# Subset to available TCF (should be all)
input <- input[item_code %in% opt_tcf$inp_code, ]
output <- output[item_code %in% opt_tcf$out_code, ]

# Set NAs to 0
input[is.na(processing), processing := 0]
output[is.na(production), production := 0]

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
    tcf_xy <- tcf_x[inp_code %in% inp_xy$item_code &
      out_code %in% out_xy$item_code, ]
    wt_xy <- wt_x[out_code %in% out_xy$item_code, ]
    # Optimise on country x & year y
    opt <- optim(par = rep(0, nrow(tcf_xy)), # To-do: vectorise further
      fn = function(par) {
        I <- tcf_xy[, .(inp_code, par = par)][,
          list(x = na_sum(par)), by = c("inp_code")]
        O <- tcf_xy[, .(out_code, par = par * value)][,
          list(x = na_sum(par)), by = c("out_code")]
        # Get absolute deviations from target (ensuring correct order, output deviations weighted)
        prod_tgt <- out_xy$production[match(O$out_code, out_xy$item_code)]
        prod_err <- abs(prod_tgt - O$x) / wt_xy$weight
        proc_tgt <- inp_xy$processing[match(I$inp_code, inp_xy$item_code)]
        proc_err <- abs(proc_tgt - I$x)
        # Get relative deviations from target weighted by maximum absolute error
        prod_err_rel <- abs(prod_tgt - O$x) / prod_tgt * max(prod_err)
        proc_err_rel <- abs(proc_tgt - I$x) / proc_tgt * max(proc_err)
        # Sum of squared absolute deviations + 50% of weighted relative deviation
        return(sum(prod_err^2) + sum(proc_err^2) + (sum(prod_err_rel^2) + sum(proc_err_rel^2)) / 2)
    }, method = "L-BFGS-B", lower = 0, upper = Inf)
    # check results
    # data <- dplyr::mutate(tcf_xy, result_in = opt$par, year = y)
    tcf_xy[, .(area_code, inp_code, out_code, value,
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
  results[, .(area_code, year, item_code = inp_code, type = "optim",
    proc_code, result_in)],
  by = c("area_code", "year", "item_code", "proc_code", "type"), all.x = TRUE)
use[!is.na(result_in) & type == "optim", use := result_in]
use[, result_in := NULL]

cbs <- merge(cbs,
  results[, list(result_in = na_sum(result_in)),
    by = .(area_code, year, item_code = inp_code)],
  by = c("area_code", "year", "item_code"), all.x = TRUE)
cbs[!is.na(result_in), processing := na_sum(processing, -result_in)]
cbs[, result_in := NULL]




# Allocate final demand ------------------------------------------------------
use_fd <- cbs[, c("year", "comm_code", "area_code", "area", "item_code", "item",
  "food", "other", "losses", "stock_addition", "balancing", "unspecified")]


# Remove unneeded variables
use <- use[, c("year", "area_code", "area", "comm_code", "item_code", "item",
               "proc_code", "proc", "type", "use")]


# Save ------------------------------------------------------

saveRDS(cbs, "data/cbs_final.rds")
saveRDS(use, "data/use_final.rds")
saveRDS(use_fd, "data/use_fd_final.rds")
saveRDS(sup, "data/sup_final.rds")
