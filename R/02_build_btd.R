
library("data.table")
library(tidyverse)
source("R/01_tidy_functions.R")

regions <- fread("inst/regions.csv")
items <- fread("inst/products.csv")


# BTD ---------------------------------------------------------------------

cat("\nBuilding full BTD.\n")

btd <- readRDS("input/fore_trad_tidy.rds")
btd <- btd[unit != "usd"]

# Change from reporting & partner country to receiving & supplying country
btd[, `:=`(from = ifelse(imex == "Import", partner, reporter),
  from_code = ifelse(imex == "Import", partner_code, reporter_code),
  to = ifelse(imex == "Import", reporter, partner),
  to_code = ifelse(imex == "Import", reporter_code, partner_code),
  reporter = NULL, reporter_code = NULL,
  partner = NULL, partner_code = NULL)]

# Give preference to import flows over export flows
btd <- flow_pref(btd, pref = "Import")
btd[, imex := NULL]

# Exclude intra-regional trade flows
btd <- dt_filter(btd, from_code != to_code)

btd <- merge(btd, items[, .(com_code, item_code)],
  by = "item_code", all.x = TRUE)
btd[, item_code := NULL]


# BACI -----------------------------------------------------------------

cat("\nAdding baci trade data.\n")

baci <- readRDS("input/baci_tidy.rds")
baci <- baci[unit != "usd"]

baci <- rename(baci, from = exporter, from_code = exporter_code, to = importer, to_code = importer_code)

# Exclude intra-regional trade flows
baci <- dt_filter(baci, from_code != to_code)

conc <- fread("inst/items_baci.csv")
baci <- merge(baci, conc[, .(com_code, item, item_code = baci_code)],
  by = "item_code", all.x = TRUE)
baci[, item_code := NULL]



# Merge -------------------------------------------------------------------

btd <- rbindlist(list(btd, baci), use.names = TRUE)

# Replace negatives with 0 (except for regions "Unspecified" and "Others (adjustments)")
# (Not needed, because there are only negatives for these two regions.)
# btd[value < 0 & !from_code %in% c(252,254) & !to_code %in% c(252,254),
#     value := 0]

# Aggregate values
btd <- btd[, list(value = na_sum(value)), by = c("com_code", "item",
  "from", "from_code", "to", "to_code", "year", "unit")]


# Change units from tonnes to m3
# for wood fuel, particle board, OSB, wood residues
tcf <- fread("inst/tcf_use_tidy.csv")
tcf <- rbind(tcf[com_code == "c03" & unit == "m3sw/tonne",],
  tcf[com_code == "c09" & unit == "kg/m3p",],
  tcf[com_code == "c10" & unit == "kg/m3p",])

btd <- merge(btd, tcf[, .(com_code, unit_tcf = unit, from = area, tcf)],
  all.x = TRUE, by = c("com_code", "from"))

btd[com_code == "c03", `:=`(value = value * tcf, unit = "m3")]
btd[com_code %in% c("c09", "c10"), `:=`(value = value / tcf * 1000,
                                        unit = "m3")]
btd[com_code == "c18", `:=`(value = value * 1.5, unit = "m3")]


# Store -------------------------------------------------------------------

saveRDS(btd, "input/btd_tidy.rds")
