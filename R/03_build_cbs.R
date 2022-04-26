
library("data.table")
library("Matrix")
source("R/01_tidy_functions.R")

regions <- fread("inst/regions.csv")
items <- fread("inst/products.csv")


# Commodity balances ------------------------------------------------------

cat("\nBuilding commodity balances.\n")

fore <- readRDS("input/fore_prod_tidy.rds")
btd <- readRDS("input/btd_tidy.rds")

# Estimate black liquor production
data <- fore[grepl("sulphate", item), .(area_code, area, year, unit, production)]
data <- data[, list(production = sum(production, na.rm = TRUE)), by = c("area_code", "area", "year", "unit")]
# 1.5 tonnes of 'black liquor solids' per 1 air-dried tonne of sulphate pulp (FAO/ITTO/UNECE 2020)
data[, `:=`(item = "Black liquor", item_code = 9999, imports = 0, exports = 0, production = production * 1.5)]
fore <- rbind(fore[!grepl("sulphate", item)], data)

fore[, `:=`(com_code = items$com_code[match(fore$item_code, items$item_code)],
  item = items$item[match(fore$item_code, items$item_code)],
  item_code = NULL)]

# Remove outliers
fore[area_code==108 & com_code=="c02" & year %in% 2000:2001, exports := 0]


# Create RoW --------------------------------------------------------------

fore <- replace_RoW(fore, codes = regions[baci == TRUE, area_code])
fore <- fore[, lapply(.SD, na_sum),
  by = c("area_code", "area", "com_code", "item", "year", "unit")]

# Aggregate RoW countries in BTD
btd <- replace_RoW(btd, cols = c("from_code", "to_code"),
  codes = c(regions[baci == TRUE, area_code], 252, 254))
btd <- btd[, lapply(.SD, na_sum), by = c("from_code", "from",
  "to_code", "to", "com_code", "item", "unit", "year")]


# Save --------------------------------------------------------------------

saveRDS(fore, "data/cbs.rds")
saveRDS(btd, "data/btd.rds")
