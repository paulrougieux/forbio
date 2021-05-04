
library("data.table")
library(tidyverse)

regions <- fread("inst/regions.csv")
items <- fread("inst/products.csv")


# Supply ------------------------------------------------------------------

btd <- readRDS("data/btd_final.rds")
cbs <- readRDS("data/cbs.rds")
sup <- fread("inst/sup.csv")
shares <- fread("inst/mb_sup_tidy.csv")


cat("Allocate production to supplying processes.\n")

shares <- merge(shares, sup[type=="100%", c("proc_code", "com_code")],
  by = "proc_code", all.x = TRUE)

# Allocate production to supplying processes including double-counting
sup <- merge(
  cbs[, c("area_code", "area", "year", "com_code", "item", "production")],
  sup[com_code %in% unique(cbs$com_code)],
  by = c("com_code", "item"), all = TRUE, allow.cartesian = TRUE)


# Downscale double-counted production -----------------------------------------

cat("Calculate supply shares for multi-output processes.\n")

# Include CBS data of main product
shares <- merge(shares, cbs[, c("area_code", "year", "com_code", "production")],
  by = c("area_code", "com_code"), allow.cartesian = TRUE)

# Derive roundwood equivalents (rwe) and apply supply shares to get estimates
shares[, rwe := production / product]
shares[, `:=`(chips_est = round(chips * rwe), residues_est = round(residues * rwe),
  product = production, production = NULL, rwe = NULL)]

# Compare estimates totals with CBS data
shares <- merge(shares,
  shares[, list(chips_total = sum(chips_est, na.rm = TRUE),
    residues_total = sum(residues_est, na.rm = TRUE)),
      by = c("area_code","year")],
    by = c("area_code","year"), all.x = TRUE)
shares <- merge(shares, cbs[com_code=="c17", .(area_code, year, chips_cbs = production)],
  by = c("area_code","year"), all.x = TRUE)
shares <- merge(shares, cbs[com_code=="c18", .(area_code, year, residues_cbs = production)],
  by = c("area_code","year"), all.x = TRUE)

# Change units chips_cbs and residues_cbs from m3p to m3sw 
tcf <- fread("inst/tcf_use_tidy.csv")
tcf <- tcf[com_code %in% c("c17", "c18") & unit == "m3p/m3sw",
  .(com_code, area_code, tcf)]
tcf[, com_code := ifelse(com_code=="c17", "tcf_chips", "tcf_residues")]
tcf <- pivot_wider(tcf, names_from = com_code, values_from = tcf)

shares <- merge(shares, tcf, by = c("area_code"), all.x = TRUE)
shares[, `:=`(chips_cbs = round(chips_cbs / tcf_chips),
  residues_cbs = round(residues_cbs / tcf_residues))]

# Calculate and select between scaled figures and adjusted estimate
shares[, `:=`(chips_scaled = chips_cbs * chips_est / chips_total,
              residues_scaled = residues_cbs * residues_est / residues_total)]
shares[, `:=`(chips_adj = chips_scaled / chips_cbs * chips_total,
              residues_adj = residues_scaled / residues_cbs * residues_total, 
              chips_diff = chips_total - chips_scaled,
              residues_diff = residues_scaled - residues_total)]

shares[, `:=`(chips_final = ifelse(chips_diff < 0, chips_adj, chips_scaled),
              residues_final = ifelse(residues_diff > 0, residues_adj, residues_scaled)) ]

# Add residues if less chips produced than estimated
shares[, `:=`(residues_final = residues_final + 
  ifelse(chips_diff > 0 & residues_diff > 0, 
         ifelse(residues_diff > chips_diff, 
              chips_diff / residues_total * residues_est, 
              residues_diff / residues_total * residues_est), 0))]

# Calculate remainders of chips and residues
shares <- merge(shares, shares[, list(chips_rest = sum(chips_final, na.rm = TRUE), 
  residues_rest = sum(residues_final, na.rm = TRUE)), by = c("area_code", "year")], 
  all.x = TRUE, by = c("area_code", "year"))
shares[, `:=`(chips_rest = chips_cbs - chips_rest, residues_rest = residues_cbs - residues_rest)]

# Convert chips and residues into m3p
shares[, `:=`(chips_final = round(chips_final * tcf_chips), residues_final = round(residues_final * tcf_residues), 
  chips_rest = round(chips_rest * tcf_chips), residues_rest = round(residues_rest * tcf_residues))]

remainders <- shares[com_code=="c04", .(area_code, area, year, chips = chips_rest, residues = residues_rest)]


## Merge chips and residues supply ------------------------------------
sup <- merge(sup, shares[, .(area_code, proc_code, year, chips = chips_final, residues = residues_final)], 
  by = c("area_code", "proc_code", "year"), all.x = TRUE)
sup[proc_code %in% c("p04","p05","p06","p07") & com_code=="c17", production := chips]
sup[proc_code %in% c("p04","p05","p06","p07") & com_code=="c18", production := residues]
sup[, `:=`(chips = NULL, residues = NULL)]


## Merge chips and residues remainders ------------------------------------
sup <- merge(sup, remainders[, .(area_code, year, chips, residues)], 
  by = c("area_code", "year"), all.x = TRUE)
sup <- merge(sup, sup[proc_code %in% c("p01","p02"), 
  list(roundwood = sum(production, na.rm = TRUE)), by = c("area_code", "year")], 
  all.x = TRUE, by = c("area_code", "year"))
sup[, `:=`(chips = round(chips / roundwood * production), 
  residues = round(residues / roundwood * production))]
chips <- sup[proc_code %in% c("p01","p02"), 
  .(area_code, year, proc_code, com_code = "c17", item = items$item[items$com_code=="c17"], 
    area, production = chips, process, type)]
residues <- sup[proc_code %in% c("p01","p02"), 
  .(area_code, year, proc_code, com_code = "c18", item = items$item[items$com_code=="c18"], 
    area, production = residues, process, type)]
sup[, `:=`(chips = NULL, residues = NULL, roundwood = NULL)]
sup <- rbindlist(list(sup, chips, residues))


# data <- sup[proc_code %in% c("p01","p02") & com_code %in% c("c17","c18")]
# data <- merge(data, tcf, by = "area_code", all.x = TRUE)
# data[com_code=="c17", `:=`(production = production / tcf_chips)]
# data[com_code=="c18", `:=`(production = production / tcf_residues)]
# data <- data[, list(production = round(sum(production, na.rm = TRUE))), by = c("year", "area_code", "area")]
# data <- merge(data, sup[com_code=="c03", .(area_code, year, woodfuel = production)], all.x = TRUE, by = c("year", "area_code"))
# data[, diff := woodfuel - production]
# data[, diff_percentage := round(diff / woodfuel * 100)]
# data <- data[is.finite(production) & production != 0 & woodfuel != 0]


# Store results -----------------------------------------------------------

saveRDS(sup, "data/sup.rds")
