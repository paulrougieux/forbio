
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
# for c03 wood fuel, c09 particle board, c10 OSB, c18 wood residues

# Apply cf_btd
cf <- fread("inst/cf_btd_tidy.csv")
btd <- merge(btd, cf[, .(com_code, unit_cf = unit, from = area, cf)],
  all.x = TRUE, by = c("com_code", "from"))
btd[com_code %in% c("c03", "c09", "c10"), `:=`(value = value * cf, unit = "m3")]

# Apply factor of 1.5 to c18 wood residues (UNECE and FAO, 2010)
btd[com_code == "c18", `:=`(value = value * 1.5, unit = "m3")]

btd[, `:=`(unit_cf = NULL, cf = NULL)]


# Remove outliers ---------------------------------------------------------

btd[from_code==108 & com_code=="c02" & to_code==254 & year %in% 2001:2002, value := 0]


# Store -------------------------------------------------------------------

saveRDS(btd, "input/btd_tidy.rds")
