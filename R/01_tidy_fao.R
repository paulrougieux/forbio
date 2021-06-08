
library("data.table")
source("R/01_tidy_functions.R")

regions <- fread("inst/regions.csv")
products <- fread("inst/products.csv")

# Colnames ----------------------------------------------------------------

rename <- c(
  "Area Code" = "area_code",
  "AreaCode" = "area_code",
  "Area" = "area",
  "AreaName" = "area",
  "Item Code" = "item_code",
  "ItemCode" = "item_code",
  "Item" = "item",
  "ItemName" = "item",
  # "Element Code" = "element_code",
  "Element" = "element",
  "ElementName" = "element",
  # "Year Code" = "year_code",
  "Year" = "year",
  "Unit" = "unit",
  # "Flag" = "flag",
  "Value" = "value",
  "Reporter Country Code" = "reporter_code",
  "Reporter Countries" = "reporter",
  "Partner Country Code" = "partner_code",
  "Partner Countries" = "partner",
  # After casting
  "Production" = "production",
  "Import Quantity" = "imports",
  "Export Quantity" = "exports",
  "Domestic supply quantity" = "total_supply",
  "Losses" = "losses",
  "Food supply quantity (tonnes)" = "food",
  "Stock Variation" = "stock_withdrawal",
  "Feed" = "feed",
  "Seed" = "seed",
  "Other uses" = "other",
  "Processing" = "processing",
  # Units
  # "1000 US$" = "k_usd",
  # "1000 Head" = "k_capita",
  "Head" = "head",
  "tonnes" = "tonnes",
  "Export" = "exports",
  "Import" = "imports",
  # Fish
  "COUNTRY" = "country",
  # "AREA" = "water_area",
  "SOURCE" = "source_code",
  "SPECIES" = "species",
  "YEAR" = "year",
  "UNIT" = "unit",
  "QUANTITY" = "value"
)


# Forestry ----------------------------------------------------------------

cat("\nTidying forestry.\n")

#
# Production
fore_prod <- readRDS("input/fore_prod.rds")

fore_prod <- dt_rename(fore_prod, rename, drop = TRUE)

# Country / Area adjustments
fore_prod <- area_kick(fore_prod, code = 351, pattern = "China", groups = TRUE)
fore_prod <- area_kick(fore_prod, code = 30, pattern = "Antarctica")
fore_prod <- area_merge(fore_prod, orig = 62, dest = 238, pattern = "Ethiopia")
fore_prod <- area_merge(fore_prod, orig = 206, dest = 276, pattern = "Sudan")
fore_prod <- area_fix(fore_prod, regions)

## Belgium-Luxembourg before 2000 together
#fore_prod[area_code==255 & area=="Belgium" & year<2000,
#     `:=`(area_code=15, area="Belgium-Luxembourg")]
#fore_prod[area_code==256 & area=="Luxembourg" & year<2000,
#     `:=`(area_code=15, area="Belgium-Luxembourg")]


# Cut down to certain products
fore_prod <- dt_filter(fore_prod, item_code %in% products$item_code |
  grepl("sulphate", item))
fore_prod <- dt_filter(fore_prod, value >= 0)
# Recode "1000 US$" to "usd"
fore_prod[unit == "1000 US$", `:=`(value = value * 1000, unit = "usd")]

fore_prod[, imex := factor(gsub("^(Import|Export) (.*)$", "\\1", element))]

# Get this in the format of CBS
fore_prod <- data.table::dcast(fore_prod[unit != "usd"],
  area_code + area + item_code + item + year + unit ~ imex, value.var = "value")
fore_prod <- dt_rename(fore_prod, rename, drop = FALSE)

# Store
saveRDS(fore_prod, "input/fore_prod_tidy.rds")
rm(fore_prod)


# Trade
fore_trad <- readRDS("input/fore_trad.rds")
items_trade <- fread("inst/items_trade.csv")

fore_trad <- dt_rename(fore_trad, rename)

# Country / Area adjustments
for(col in c("reporter_code", "partner_code")) {
  fore_trad <- area_kick(fore_trad, code = 351, pattern = "China",
    groups = TRUE, col = col)
  fore_trad <- area_kick(fore_trad, code = 30, pattern = "Antarctica",
    col = col)
  fore_trad <- area_merge(fore_trad, orig = 62, dest = 238,
    pattern = "Ethiopia", col = col)
  fore_trad <- area_merge(fore_trad, orig = 206, dest = 276,
    pattern = "Sudan", col = col)
  fore_trad <- area_fix(fore_trad, regions, col = col)
}

## Belgium-Luxembourg before 2000 together
#fore_trad[reporter_code==255 & reporter=="Belgium" & year<2000,
#     `:=`(reporter_code=15, reporter="Belgium-Luxembourg")]
#fore_trad[partner_code==255 & partner=="Belgium" & year<2000,
#     `:=`(partner_code=15, partner="Belgium-Luxembourg")]
#fore_trad[reporter_code==256 & reporter=="Luxembourg" & year<2000,
#     `:=`(reporter_code=15, reporter="Belgium-Luxembourg")]
#fore_trad[partner_code==256 & partner=="Luxembourg" & year<2000,
#     `:=`(partner_code=15, partner="Belgium-Luxembourg")]

# Cut down to certain products
fore_trad <- dt_filter(fore_trad, item_code %in% items_trade$trade_code)

# Recode "1000 US$" to "usd"
fore_trad[unit == "1000 US$", `:=`(value = value * 1000, unit = "usd")]

fore_trad[, imex := factor(gsub("^(Import|Export) (.*)$", "\\1", element))]

cat("Aggregating forestry trade items to the level of forestry production.\n")
item_match <- match(fore_trad[["item_code"]], items_trade[["trade_code"]])
fore_trad[, `:=`(item_code = items_trade$item_code[item_match],
  item = items_trade$item[item_match])]
fore_trad <- fore_trad[, list(value = na_sum(value)),
  by = .(reporter_code, reporter, partner_code, partner,
    item_code, item, year, imex, unit)]
cat("Aggregation from", length(item_match), "to",
  nrow(fore_trad), "observations.\n")

# Store
saveRDS(fore_trad, "input/fore_trad_tidy.rds")
rm(fore_trad, item_match)