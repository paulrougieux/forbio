
library("data.table")
library(tidyverse)
source("R/01_tidy_functions.R")

regions <- fread("inst/regions.csv")
items <- fread("inst/products.csv")


# Supply ------------------------------------------------------------------

cbs <- readRDS("data/cbs.rds")
sup_structure <- fread("inst/sup.csv")
shares <- fread("inst/mb_sup_tidy.csv")


cat("Allocate production to supplying processes.\n")

shares <- merge(shares, sup_structure[type=="100%", c("proc_code", "com_code")],
  by = "proc_code", all.x = TRUE)

# Allocate production to supplying processes including double-counting
sup <- merge(
  cbs[, .(area_code, area, year, com_code, item, production)],
  sup_structure[com_code %in% unique(cbs$com_code)],
  by = c("com_code", "item"), all = TRUE, allow.cartesian = TRUE)


# Downscale double-counted production -----------------------------------------
cat("Calculate supply shares for multi-output processes.\n")

# shares <- merge(shares, cbs[, .(area_code, year, com_code, production)],
shares <- merge(shares, cbs[, .(area_code, year, com_code, production)],
  by = c("area_code", "com_code"), allow.cartesian = TRUE)

# Derive roundwood equivalents (rwe)
shares[, rwe := production / product]
shares[, `:=`(chips = round(chips * rwe), residues = round(residues * rwe),
  product = production, production = NULL, rwe = NULL)]

# Comparison with CBS data
shares <- merge(shares,
  shares[, list(chips_total = sum(chips, na.rm = TRUE),
    residues_total = sum(residues, na.rm = TRUE)),
      by = c("area_code","year")],
    by = c("area_code","year"), all.x = TRUE)
shares <- merge(shares, cbs[com_code=="c17", .(area_code, year, chips_cbs = production)],
  by = c("area_code","year"), all.x = TRUE)
shares <- merge(shares, cbs[com_code=="c18", .(area_code, year, residues_cbs = production)],
  by = c("area_code","year"), all.x = TRUE)

# Change units chips_cbs and residues_cbs from m3p to m3sw
# For this multiply instead of divide (because unit is now the other way around)
tcf <- fread("inst/tcf_use_tidy.csv")
tcf <- tcf[com_code %in% c("c17", "c18") & unit == "m3sw/m3p",
  .(com_code, area_code, tcf)]

tcf[, com_code := ifelse(com_code=="c17", "tcf_chips", "tcf_residues")]
tcf <- pivot_wider(tcf, names_from = com_code, values_from = tcf)
shares <- merge(shares, tcf, by = c("area_code"), all.x = TRUE)
shares[, `:=`(chips_cbs = round(chips_cbs * tcf_chips),
  residues_cbs = round(residues_cbs * tcf_residues))]

# Calculate supply of the unknown process pxy and convert into m3p
shares[, `:=`(chips_pxy = chips_cbs - chips_total, residues_pxy = residues_cbs - residues_total)]

# Convert chips and residues into m3p
shares[, `:=`(chips_final = round(chips / tcf_chips), 
  residues_final = round(residues / tcf_residues),
  chips_pxy = round(chips_pxy / tcf_chips), 
  residues_pxy = round(residues_pxy / tcf_residues))]


## Merge chips and residues supply ------------------------------------
sup <- merge(sup, shares[, .(area_code, proc_code, year, chips = chips_final, residues = residues_final)], 
  by = c("area_code", "proc_code", "year"), all = TRUE)
sup[proc_code %in% c("p04","p05","p06","p07") & com_code=="c17" & is.finite(chips), 
  production := chips]
sup[proc_code %in% c("p04","p05","p06","p07") & com_code=="c18" & is.finite(residues), 
  production := residues]
sup[, `:=`(chips = NULL, residues = NULL)]



## Merge chips and residues unknown supply ------------------------------------
chips <- merge(unique(shares[!is.na(chips_pxy) & chips_pxy > 0, .(area_code, proc_code = "pxy", 
  year, com_code = "c17", production = chips_pxy, process = "Unknown source", type = NA)]), 
  unique(sup[, .(area_code, area, com_code, item)]), 
  by = c("area_code", "com_code"), all.x = TRUE)
residues <- merge(unique(shares[!is.na(residues_pxy) & residues_pxy > 0, .(area_code, proc_code = "pxy", 
  year, com_code = "c18", production = residues_pxy, process = "Unknown source", type = NA)]), 
  unique(sup[, .(area_code, area, com_code, item)]), 
  by = c("area_code", "com_code"), all.x = TRUE)
sup <- rbindlist(list(sup, chips, residues), use.names = TRUE)




# Adapt and re-balance CBS -----------------------------------------------------------
cbs <- merge(cbs, sup[, list(production_new = na_sum(production)), 
  by = c("area_code", "com_code", "year")], by = c("area_code", "com_code", "year"),
  all = TRUE)
cbs[, bal_byprod := round(na_sum(production_new, -production))]
cbs[, `:=`(total_supply = na_sum(production, bal_byprod, imports), bal_prod = 0,
  dom_supply = na_sum(production, bal_byprod, imports, -exports), production_new = NULL)]
cbs[dom_supply < 0, `:=`(bal_prod = -dom_supply, dom_supply = 0, 
  total_supply = na_sum(production, bal_byprod, bal_prod, imports))]


# Add supply from unknown sources
sup_bal <- merge(
  cbs[, .(area_code, area, year, com_code, item, production = bal_prod)],
  unique(sup_structure[com_code %in% unique(cbs$com_code), 
  .(proc_code = "pxy", process = "Unknown source", com_code, item, type = NA)]),
  by = c("com_code", "item"), all.x = TRUE)
sup_bal <- sup_bal[production != 0 & !is.na(production)]
sup <- bind_rows(sup, sup_bal)


# Store results -----------------------------------------------------------

saveRDS(sup, "data/sup.rds")
saveRDS(cbs, "data/cbs_bal.rds")
