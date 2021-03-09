
library("data.table")
library("Matrix")
source("R/01_tidy_functions.R")

regions <- fread("inst/regions.csv")
items <- fread("inst/products.csv")


# Commodity balances ------------------------------------------------------

cat("\nBuilding commodity balances.\n")

fore <- readRDS("input/fore_prod_tidy.rds")
btd <- readRDS("input/btd_tidy.rds")

fore[, `:=`(com_code = items$com_code[match(fore$item_code, items$item_code)],
  item = items$com_code[match(fore$item_code, items$item_code)],
  item_code = NULL)]

fore[, `:=`(total_supply = na_sum(production, imports),
   use = na_sum(production, imports, -exports),
   balancing = 0)]
fore[use < 0, `:=`(balancing = use, use = 0)]



# Create RoW --------------------------------------------------------------

fore <- replace_RoW(fore, codes = regions[baci == TRUE, area_code])
fore <- fore[, lapply(.SD, na_sum),
  by = c("area_code", "area", "com_code", "item", "year", "unit")]

# Aggregate RoW countries in BTD
btd <- replace_RoW(btd, cols = c("from_code", "to_code"),
  codes = c(regions[baci == TRUE, area_code], 252, 254))
btd <- btd[, lapply(.SD, na_sum), by = c("from_code", "from",
  "to_code", "to", "com_code", "item", "unit", "year")]

# # Remove ROW-internal trade from CBS
# # There are no internal trade flows within ROW
# intra <- btd[from_code==to_code, sum(value), by=c("from_code","from","com_code","item","year", "unit")]
# fore <- merge(fore, intra,
#   by.x = c("area_code", "area", "com_code", "item", "year", "unit"),
#   by.y = c("from_code", "from", "com_code", "item", "year", "unit"),
#   all.x = TRUE)
# fore[!is.na(V1), `:=`(exports = na_sum(exports,-V1),
#   imports = na_sum(imports,-V1))]
# fore[, V1 := NULL]
# rm(intra)
# # Remove ROW-internal trade from BTD
# btd <- dt_filter(btd, from_code != to_code)


# Save --------------------------------------------------------------------

saveRDS(fore, "data/cbs.rds")
saveRDS(btd, "data/btd.rds")
