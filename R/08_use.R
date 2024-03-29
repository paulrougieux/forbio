
library("data.table")
library("tidyverse")
library("Matrix")
source("R/01_tidy_functions.R")

regions <- fread("inst/regions.csv")
items <- fread("inst/products.csv")
years <- 1997:2017

cbs <- readRDS("data/cbs_bal.rds")
cbs[, processing := dom_supply]
sup <- readRDS("data/sup.rds")
use_items <- fread("inst/use.csv")
source_use <- fread("inst/source_use.csv")


# Use ---------------------------------------------------------------------

# Create long use table
use <- data.table(expand.grid(area_code = c(regions$area_code, 999), year = years, 
  com_code = unique(use_items$com_code)))
use[, `:=`(area = regions$area[match(use$area_code, regions$area_code)],
  item = items$item[match(use$com_code, items$com_code)])]
use[, area := ifelse(area_code == 999, "RoW", area)]
use <- merge(
  use[, .(area_code, area, year, com_code, item)],
  use_items[, .(proc_code, process, com_code, item, type)],
  all = TRUE, allow.cartesian = TRUE)
use[, use := NA_real_]


# CF ---------------------------------------------------------------------

cat("Allocating part of the CF commodities to CF use. Applies to items:\n\t",
  paste0(unique(use[type %in% c("cf", "cf_cnc", "cf_pellets", "cf_board", "cf_pulp"), item]), 
    collapse = "; "), "\nused for processes \n\t",
  paste0(unique(use[type %in% c("cf", "cf_cnc", "cf_pellets", "cf_board", "cf_pulp"), process]), 
    collapse = "; "), ".\n", sep = "")

# Read conversion factors
cf <- fread("inst/cf_use_tidy.csv")

# Conversion factors for each input-output combination
cf <- merge(source_use[type %in% c("cf", "cf_cnc", "cf_board", "cf_pulp"), 
                        .(proc_code, process, source_code, source, com_code, item)], 
  cf[, .(com_code, item, source_code, source, area_code, area, unit, cf)], 
  by = c("source_code", "source", "com_code", "item"), all.x = TRUE)

# Conversion factors for pellets
cf_use <- fread("inst/cf_use_tidy.csv")
cf_out <- cf_use[com_code=="c15"]
cf_in <- cf_use[com_code %in% source_use[type=="cf_pellets", source_code]]
cf_in[unit=="m3rw/m3p", `:=`(cf = 1 / cf, unit = "m3p/m3sw")]

cf_pellets <- merge(source_use[type=="cf_pellets"], 
  cf_in[, .(area_code,source_code=com_code,cf_in=cf)],
  by = c("source_code"), allow.cartesian = TRUE)
cf_pellets <- merge(cf_pellets, 
  cf_out[, .(area_code,com_code,cf_out=cf)],
  by = c("area_code", "com_code"))
cf_pellets[, cf := (cf_in * cf_out)]
cf_pellets[, `:=`(unit = "m3p/tonne", cf_in = NULL, cf_out = NULL, type = NULL,
  area = regions$area[match(cf_pellets$area_code, regions$area_code)])]
cf <- rbind(cf[!item %in% cf_pellets$item], cf_pellets)

# Conversion factors for pulp
cf_use <- fread("inst/cf_use_tidy.csv")
cf_out <- cf_use[grepl("c11", com_code)]
cf_in <- cf_use[com_code %in% source_use[type=="cf_pulp", source_code]]
cf_in[unit=="m3rw/m3p", `:=`(cf = 1 / cf, unit = "m3p/m3sw")]

cf_pulp <- merge(source_use[type=="cf_pulp"], 
  cf_in[, .(area_code, source_code=com_code, cf_in = cf)],
  by = c("source_code"), allow.cartesian = TRUE)
cf_pulp <- merge(cf_pulp, 
  cf_out[, .(area_code,com_code,cf_out=cf)],
  by = c("area_code", "com_code"))
cf_pulp[, cf := (cf_in * cf_out)]
cf_pulp[, `:=`(unit = "m3p/tonne", cf_in = NULL, cf_out = NULL, type = NULL,
  area = regions$area[match(cf_pulp$area_code, regions$area_code)])]
cf <- rbind(cf[!item %in% cf_pulp$item], cf_pulp)

# Conversion factors for boards
cf_use <- fread("inst/cf_use_tidy.csv")
cf_out <- cf_use[com_code %in% c("c08","c09")]
cf_in <- cf_use[com_code %in% source_use[type=="cf_board", source_code]]
cf_in[unit=="m3rw/m3p", `:=`(cf = 1 / cf, unit = "m3p/m3sw")]
cf_in[unit=="m3sw/tonne", `:=`(cf = 1 / cf, unit = "tonne/m3sw")]

cf_board <- merge(source_use[type=="cf_board"], 
  cf_in[, .(area_code, source_code = com_code, cf_in = cf)],
  by = c("source_code"), allow.cartesian = TRUE)
cf_board <- merge(cf_board, 
  cf_out[, .(area_code, com_code, cf_out = cf)],
  by = c("area_code", "com_code"))
cf_board[, cf := (cf_in * cf_out)]
cf_board[, `:=`(unit = "m3p/m3p", cf_in = NULL, cf_out = NULL, type = NULL,
  area = regions$area[match(cf_board$area_code, regions$area_code)])]
cf_board[source_code =="c19", `:=`(unit = "tonne/m3p")]
cf <- rbind(cf[!item %in% cf_board$item], cf_board)

cf_codes <- list(sort(unique(cbs$area_code[cbs$area_code %in% cf$area_code])), sort(unique(cf$com_code)),
  sort(unique(cf$source_code)))

# Create cf matrices for each country
Cs <- lapply(cf_codes[[1]], function(x) {
  out <- data.table::dcast(cf[area_code == x], com_code ~ source_code, fill = 0,
    fun.aggregate = na_sum, value.var = "cf")
  setkey(out, com_code)
  out <- as(out[, -1], "Matrix")
})
Cs <- lapply(Cs, `dimnames<-`, list(cf_codes[[2]], cf_codes[[3]]))
names(Cs) <- cf_codes[[1]]

# Get all relevant input and output data
cf_data <- cbs[area_code %in% cf_codes[[1]] &
  (com_code %in% cf_codes[[2]] | com_code %in% cf_codes[[3]]),
  .(year, area_code, com_code, production, processing)]
setkey(cf_data, year, area_code, com_code)
areas <- cf_codes[[1]]

# Production in processes
output <- cf_data[data.table(expand.grid(year = years,
  area_code = areas, com_code = cf_codes[[2]]))]
output[, `:=`(value = production, production = NULL, processing = NULL)]
dt_replace(output, is.na, 0, cols = "value")

# Processing of source items
input <- cf_data[data.table(expand.grid(year = years,
  area_code = areas, com_code = cf_codes[[3]]))]
input[, `:=`(value = processing, production = NULL, processing = NULL)]
dt_replace(input, is.na, 0, cols = "value")

# Prepare template to be filled - processing per process
results <- cf_data[data.table(expand.grid(year = years, area_code = areas,
  com_code = cf_codes[[3]], com_code_proc = cf_codes[[2]]))]
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
    out <- split_cf(y = output_y, z = input_y,
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

# Estimate feedstock composition for pulp production
pulp <- fread("inst/pulp_feedstock_tidy.csv")
results <- merge(results, pulp[, .(area_code, proc_code, roundwood, chips)],
  by = c("area_code", "proc_code"), all.x = TRUE)

results[grepl("p11", proc_code), `:=`(roundwood = if_else(is.na(roundwood), 0.66, roundwood),
  chips = if_else(is.na(chips), 0.34, chips))]
results[grepl("p11", proc_code), `:=`(value = if_else(com_code %in% c("c01","c02"), value * roundwood, value))]
results[grepl("p11", proc_code), `:=`(value = if_else(com_code %in% c("c17"), value * chips, value))]
results[grepl("p11", proc_code), `:=`(value = if_else(com_code %in% c("c18"), value * 0, value))]

results[, `:=`(roundwood = NULL, chips = NULL)]

# Estimate feedstock composition for fibreboard and particle board production
results[proc_code %in% c("p08", "p09"),
        value := if_else(com_code %in% c("c01","c02"), value * 0.05, value * 0.475)]

# Particle board production for the year 2017 includes c19
# Read share of c19 ("recycled_fibre") as inputs for particle board production
wbp_recyc <- fread("inst/wbp_recyc_tidy.csv")
wbp_recyc <- wbp_recyc[, `:=`(recycled_fibre = as.double(recycled_fibre))]

# Estimate feedstock composition for particle board production
results <- merge(results, wbp_recyc[, .(area_code, proc_code, recycled_fibre)],
                 by = c("area_code", "proc_code"), all.x = TRUE)
results[proc_code %in% c("p09") & year>= 2017, `:=`(recycled_fibre = if_else(is.na(recycled_fibre), 0, recycled_fibre))]
results[proc_code %in% c("p09") & com_code == "c19", value := value / 0.475 * recycled_fibre / 100]
results[proc_code %in% c("p09") & com_code == "c19" & value>0 & year == 2017]

# Downscale the other inputs (c01, c02, c17, c18)
results[, `:=`(primary = 100 - recycled_fibre)]
results[proc_code == "p09" & com_code != "c19" & year>= 2017 & !is.na(primary), `:=`(value = value*primary/100)]
results[, `:=`(recycled_fibre = NULL, primary = NULL)]
results[com_code == "c19" & year< 2017, `:=`(value = 0)]

# Redistribute c/nc use according to availability 
# for c06 veneer, c07 plywood, c08 fibreboard, c09 particle board, c10 osb, c11 pulp
data <- merge(results[com_code %in% c("c01","c02"),], 
  source_use[, .(com_code=source_code, com_code_proc = com_code, type)], 
  by=c("com_code", "com_code_proc"))

# Sum per type, i.e. cf and cf_cnc
data <- data[, list(value = sum(value, na.rm = TRUE)), 
  by = c("com_code", "year", "area_code", "type")]
data <- spread(data, type, value)
data <- merge(data, cbs[, .(area_code, com_code, year, processing = na_sum(processing, bal_byprod, bal_prod))],
  by = c("area_code", "com_code", "year"))
data[, `:=`(rest = processing - cf, demand = na_sum(cf_board, cf_cnc, cf_pulp), cf = NULL)]
data[, `:=`(rest = ifelse(rest < 0, 0, rest))]

# Sum up remaining roundwood per country
data <- merge(data, data[, list(rest_total = na_sum(rest)), by = c("year", "area_code")],
  by = c("year", "area_code"))

data[, share := rest / rest_total]
data[, share := ifelse(!is.finite(share), 0, share)]

# Merge shares into results and apply to type 'cf_cnc'
results <- merge(results, data[, .(year, area_code, com_code, share)],
  by = c("year", "area_code", "com_code"), all.x = TRUE)
results <- merge(results, source_use[, .(com_code=source_code, com_code_proc = com_code, type)], 
  by=c("com_code", "com_code_proc"))
results[type!="cf" & com_code %in% c("c01","c02"), `:=`(value = round(value * share))]
results[, share := NULL]

# Redistribute input use for pellets production
data <- results[type=="cf_pellets", .(com_code, year, area_code, value)]
data <- merge(data, cbs[, .(area_code, com_code, year, processing = na_sum(processing, bal_byprod, bal_prod))],
  by = c("area_code", "com_code", "year"))
data <- merge(data, data[com_code=="c18", .(year, area_code, share = processing / value)],
  by = c("year", "area_code"), all.x = TRUE)
data[!is.finite(share), share := 0]
data[share > 1, share := 1]
data[com_code=="c18" & share < 1, value := processing]
data[com_code=="c18" & share == 0, value := 0]
data[com_code %in% c("c03","c17"), value := value * (1 - share)]

data <- merge(data, cf_pellets[, .(area_code, com_code = source_code, cf)],
  by = c("area_code", "com_code"))
data[, potential := processing / cf]
data <- merge(data, cbs[com_code=="c15", .(year, area_code, 
  production = na_sum(production, bal_prod, bal_byprod))],
  by = c("year", "area_code"))
data <- merge(data, data[com_code=="c18", .(year, area_code, pellets_from_residues = value / cf)], 
  by = c("year", "area_code"), all.x = TRUE)
data[, demand := na_sum(production, - pellets_from_residues)]
data <- merge(data, data[com_code!="c18", list(potential_total = sum(potential, na.rm = TRUE)), 
  by = c("year", "area_code")], by = c("year", "area_code"), all.x = TRUE)

data[, share := potential / potential_total]
data[, use := round(demand * share * cf)]
data[com_code %in% c("c03","c17"), value := use]

# Replace pellets rows in results
results <- rbind(results[type != "cf_pellets"], 
  data[, .(com_code, com_code_proc = "c15", year, area_code, value, proc_code = "p15", type = "cf_pellets")])

# Add to use (per item and process)
use <- merge(use, results[, .(year, area_code, proc_code, com_code, value)],
  by = c("year", "area_code", "proc_code", "com_code"), all = TRUE)
use[!is.na(value), `:=`(use = value)]
use[, value := NULL]


# 100% processes ------------------------------------------------------

cat("Allocating items going directly to a process.\n\t",
    paste0(unique(use[type %in% c("100%","cf_fill"), item]), collapse = "; "),
    ".\n", sep = "")
use <- merge(use,
  cbs[, .(area_code, area, year, com_code, item, processing)],
  by = c("area_code", "area", "year", "com_code", "item"), all.x = TRUE)
use[type %in% c("100%","cf_fill"), use := processing]
use[, processing := NULL]
use <- use[!is.na(use) & use != 0]


# CF fill ------------------------------------------------------
# we have allocated all remaining recovered paper to p14 paper production


# Update CBS ------------------------------------------------------------

# Subtract from cbs processing (per item) 
cbs <- merge(cbs, use[, list(use = na_sum(use)), 
  by = c("area_code", "area", "year", "com_code", "item")],
  by = c("area_code", "area", "year", "com_code", "item"), all = TRUE)
cbs[, processing := na_sum(processing, -use)]
cbs[, use := NULL]

rm(cf, cf_codes, cf_data, areas, out, data, pulp, cf_board, cf_pellets, cf_pulp,
   cf_in, cf_out, cf_use, results, Cs, input, output, input_x, output_x, input_y, output_y)


# Balance supply and use ------------------------------------------------------

# Allocate energy use
cbs[, energy := 0]
cbs[com_code %in% c("c03","c15","c16","c17","c18","c19","c20") & processing > 0, `:=`(energy = processing, processing = 0)]
#cbs[com_code %in% c("c03","c15","c16","c17","c18","c20") & processing > 0, `:=`(energy = processing, processing = 0)]

# Balance processing
cbs[, bal_processing := 0]
cbs[processing < 0, `:=`(bal_processing = -processing, processing = 0)]
cbs[, `:=`(dom_supply = na_sum(dom_supply, bal_processing), total_supply = na_sum(total_supply, bal_processing))]

# Add supply from unknown sources (bal_processing)
sup_structure <- fread("inst/sup.csv")
sup_bal <- merge(
  cbs[, .(area_code, area, year, com_code, item, production = bal_processing)],
  unique(sup_structure[com_code %in% unique(cbs$com_code), 
  .(proc_code = "pxy", process = "Unknown source", com_code, item, type = NA)]),
  by = c("com_code", "item"), all.x = TRUE)
sup_bal <- sup_bal[production != 0 & !is.na(production)]
sup <- bind_rows(sup, sup_bal)
sup <- sup[, list(production = na_sum(production)), 
  by = c("com_code", "item", "area_code", "area", "year", "proc_code", "process", "type")]

# Check supply and use balances
sup_total <- merge(cbs[, .(area_code, com_code, year, 
  cbs_prod = na_sum(production, bal_prod, bal_byprod, bal_processing),
  cbs_sup = dom_supply)],
  sup[, list(sup_prod = na_sum(production)), 
  by = c("area_code", "com_code", "year")],
  by = c("area_code", "com_code", "year"), all = TRUE)
use_total <- merge(cbs[, .(area_code, com_code, year, 
  cbs_use = processing, cbs_energy = energy)],
  use[, list(use = na_sum(use)), 
  by = c("area_code", "com_code", "year")],
  by = c("area_code", "com_code", "year"), all = TRUE)
use_total[, total_use := na_sum(use, cbs_use, cbs_energy)]

totals <- merge(sup_total, use_total, all = TRUE)
totals <- totals[year %in% years]
totals[, `:=`(diff = na_sum(cbs_sup, -total_use))]
cat(paste("There are", totals[diff > 0, .N], "cases, where supply > use and", 
  totals[diff > 0, .N], "cases, where use > supply."))

# Remove unnecessary variables and rename material and energetic final use
totals[, `:=`(material_use = cbs_use, energy_use = cbs_energy)]
totals[, `:=`(cbs_prod = NULL, cbs_sup = NULL, sup_prod = NULL, diff = NULL, 
  cbs_use = NULL, use = NULL, total_use = NULL, cbs_energy = NULL)]


# Allocate final demand ------------------------------------------------------
totals[, area := regions$area[match(totals$area_code, regions$area_code)]]
totals[area_code==999, area := "RoW"]
totals[, item := items$item[match(totals$com_code, items$com_code)]]


# Re-arrange variables
use <- use[, c("year", "area_code", "area", "com_code", "item",
  "proc_code", "process", "use")]
use_fd <- totals[, c("year", "area_code", "area", "com_code", "item",
  "material_use", "energy_use")]
sup[is.na(proc_code), `:=`(proc_code = "p17", process = "Wood recovering")]
sup <- sup[, c("year", "area_code", "area", "com_code", "item",
  "proc_code", "process", "production")]


# Save ------------------------------------------------------

saveRDS(cbs, "data/cbs_final.rds")
saveRDS(use, "data/use_final.rds")
saveRDS(use_fd, "data/use_fd_final.rds")
saveRDS(sup, "data/sup_final.rds")
