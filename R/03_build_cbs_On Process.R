
library("data.table")
library("Matrix")
source("Forestry/forbio/R/01_tidy_functions.R")

regions <- fread("Forestry/forbio/inst/regions.csv")
items <- fread("Forestry/forbio/inst/products.csv")

# CBS ---------------------------------------------------------------------

#cat("\nBuilding full CBS.\n")


# Forestry ----------------------------------------------------------------

cat("\nAdding forestry production data.\n")

fore <- readRDS("Forestry/forbio/input/fore_prod_tidy.rds")

fore[, `:=`(total_supply = na_sum(production, imports),
   other = na_sum(production, imports, -exports),
   stock_withdrawal = 0, stock_addition = 0,
   feed = 0, food = 0, losses = 0, processing = 0,
   seed = 0, balancing = 0)]
 fore[other < 0, `:=`(balancing = other, other = 0)]

# cbs <- rbindlist(list(cbs, fore), use.names = TRUE)
# rm(fore)


# Estimate cbs for items/countries not included -------------------------------------

# Filter countries and items that are not yet in CBS
#addcbs <- dt_filter(crop_prod[item_code %in% items$item_code],
#                    ! paste(area_code,item_code,year) %in% paste(cbs$area_code,cbs$item_code,cbs$year))

# Technical conversion factors to impute processing ---
tcf_crop <- fread("inst/tcf_crop.csv")

C <- data.table::dcast(tcf_crop, item_code ~ source_code, fill = 0, value.var = "tcf")
tcf_codes <- list(C[, item_code], as.integer(colnames(C[, -1])))
C <- as(C[, -1], "Matrix")
dimnames(C) <- tcf_codes

tcf_data <- addcbs[item_code %in% unlist(tcf_codes),
  .(year, area_code, item_code, production = value)]
setkey(tcf_data, year, area_code, item_code) # Quick merge & ensure item-order
years <- sort(unique(tcf_data$year))
areas <- sort(unique(tcf_data$area_code))

# Base processing on production + imports - exports
tcf_data[imps[, .(year, area_code = to_code, item_code, imports = value)],
  on = c("area_code", "item_code", "year"), imports := imports]
tcf_data[exps[, .(year, area_code = from_code, item_code, exports = value)],
  on = c("area_code", "item_code", "year"), exports := exports]

# Production of items
output <- tcf_data[data.table(expand.grid(year = years,
  area_code = areas, item_code = unique(tcf_codes[[1]])))]
output[, `:=`(value = production,
  production = NULL, imports = NULL, exports = NULL)]
dt_replace(output, is.na, 0, cols = "value")
# Production of source items
input <- tcf_data[data.table(expand.grid(year = years,
  area_code = areas, item_code = unique(tcf_codes[[2]])))]
input[, `:=`(value = na_sum(production, imports, -exports),
  production = NULL, imports = NULL, exports = NULL)]
dt_replace(input, function(x) {`<`(x, 0)}, value = 0, cols = "value")
dt_replace(input, is.na, 0, cols = "value")
# Processing of source items - to fill
results <- tcf_data[data.table(expand.grid(year = years,
  area_code = areas, item_code = tcf_codes[[2]]))]
setkey(results, year, area_code, item_code)
results[, `:=`(value = NA_real_,
  production = NULL, imports = NULL, exports = NULL)]

# Fill during a loop over years and areas
for(x in years) {
  output_x <- output[year == x, ]
  input_x <- input[year == x, ]
  for(y in areas) {
    output_y <- output_x[area_code == y, value]
    input_y <- input_x[area_code == y, value]
    # Skip if no data is available
    if(all(output_y == 0) || all(input_y == 0)) {next}
    results[year == x & area_code == y,
      value := fill_tcf(y = output_y, z = input_y, C = C, cap = FALSE)]
  }
}
results <- results[!is.na(value), .(year, area_code, item_code, processing = value)]

# Allocate uses
addcbs[, `:=`(production = value, value = NULL)]
addcbs[, exports := exps$value[match(paste(addcbs$year, addcbs$area_code, addcbs$item_code),
  paste(exps$year, exps$from_code, exps$item_code))]]
addcbs[, imports := imps$value[match(paste(addcbs$year, addcbs$area_code, addcbs$item_code),
  paste(imps$year, imps$to_code, imps$item_code))]]
addcbs[, total_supply := na_sum(production,imports)]
# allocate processing inputs to 'processing'
addcbs[, processing := results$processing[match(paste(addcbs$year, addcbs$area_code, addcbs$item_code),
                                                paste(results$year, results$area_code, results$item_code))]]
# Allocate 'Seed cotton', 'Oil, palm fruit', 'Hops', 'Sugar cane' and 'Palm kernels' supply to processing
addcbs[item_code %in% c(254, 328, 677, 2536, 2562), processing := na_sum(total_supply, -exports)]
# allocate rest to 'unspecified'
addcbs[, unspecified := na_sum(total_supply,-processing,-exports)]
addcbs[, balancing := 0]
addcbs[unspecified < 0, `:=`(balancing = unspecified, unspecified = 0)]

# cat("\nFilling missing cbs seed with crop seed data.\n")
# crop_seed <- crop[element == "Seed", ]
cat("\nSkip filling cbs seed.",
  "Apparently data on seed is not reported in newer faostat versions.\n")

# Add to CBS ---
cat("\nAdding ", nrow(addcbs), " missing cbs accounts.\n", sep = "")
cbs <- dplyr::bind_rows(cbs, addcbs)

rm(crop, crop_prod, addcbs,
  tcf_crop, tcf_codes, tcf_data, input, output, results, years, areas,
  C, input_x, output_x, input_y, output_y)


# Create RoW --------------------------------------------------------------

# remove regions "Unspecified" and "Others (adjustment)"
cbs <- cbs[! area %in% c("Unspecified", "Others (adjustment)")]

# Aggregate RoW countries in CBS
cbs <- replace_RoW(cbs, codes = regions[cbs == TRUE, code])
cbs <- cbs[, lapply(.SD, na_sum),
  by = c("area_code", "area", "item_code", "item", "year")]

# Aggregate RoW countries in BTD
btd <- replace_RoW(btd, cols = c("from_code", "to_code"),
  codes = c(regions[cbs == TRUE, code], 252, 254))
btd <- btd[, lapply(.SD, na_sum), by = c("from_code", "from",
  "to_code", "to", "comm_code", "item_code", "item", "unit", "year")]

# Remove ROW-internal trade from CBS
intra <- btd[from_code==to_code & unit!="usd", sum(value), by=c("from_code","from","item_code","item","year")]
cbs <- merge(cbs, intra,
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("from_code", "from", "item_code", "item", "year"),
  all.x = TRUE)
cbs[!is.na(V1), `:=`(exports = na_sum(exports,-V1),
  imports = na_sum(imports,-V1))]
cbs[, V1 := NULL]
rm(intra)

# Remove ROW-internal trade from BTD
btd <- dt_filter(btd, from_code != to_code)


# Rebalance columns -------------------------------------------------------

cat("\nRebalance CBS.\n")

# Round to whole numbers
cols = c("imports", "exports", "feed", "food", "losses", "other",
         "processing", "production", "seed", "balancing", "unspecified")
cbs[, (cols) := lapply(.SD, round), .SDcols = cols]

# Replace negative values with '0'
cbs <- dt_replace(cbs, function(x) {`<`(x, 0)}, value = 0,
  cols = c("imports", "exports", "feed", "food", "losses",
    "other", "processing", "production", "seed"))

# Rebalance table
cbs[, balancing := na_sum(production, imports, stock_withdrawal,
        -exports, -food, -feed, -seed, -losses, -processing, -other, -unspecified)]

cat("\nAdjust 'exports' for ", cbs[balancing < 0 &
    !is.na(exports) & exports >= balancing*0.99, .N],
    " observations, where `balancing < 0` and `exports >= -balancing` to ",
    "`exports = exports - balancing`.\n", sep = "")
cbs[balancing < 0 & !is.na(exports) & exports >= -balancing*0.99,
    `:=`(exports = na_sum(exports, balancing),
         balancing = 0)]
cbs[exports < 0, `:=`(balancing = exports,
                      exports = 0)]

cat("\nAdjust 'processing' for ", cbs[balancing < 0 &
    !is.na(processing) & processing >= -balancing, .N],
    " observations, where `balancing < 0` and `processing >= -balancing` to ",
    "`processing = processing + balancing`.\n", sep = "")
cbs[balancing < 0 & !is.na(processing) & processing >= -balancing,
    `:=`(processing = na_sum(processing, balancing),
         balancing = 0)]

cat("\nAdjust 'other' for ", cbs[balancing < 0 &
    !is.na(other) & other >= -balancing, .N],
    " observations, where `balancing < 0` and `other >= -balancing` to ",
    "`other = other + balancing`.\n", sep = "")
cbs[balancing < 0 & !is.na(other) & other >= -balancing,
    `:=`(other = na_sum(other, balancing),
         balancing = 0)]

cat("\nAdjust 'unspecified' for ", cbs[balancing < 0 &
    !is.na(unspecified) & unspecified >= -balancing, .N],
    " observations, where `balancing < 0` and `unspecified >= -balancing` to ",
    "`unspecified = unspecified + balancing`.\n", sep = "")
cbs[balancing < 0 & !is.na(unspecified) & unspecified >= -balancing,
    `:=`(unspecified = na_sum(unspecified, balancing),
         balancing = 0)]

cat("\nAdjust 'stock_addition' for ", cbs[balancing < 0 &
    !is.na(stock_addition) & stock_addition >= -balancing, .N],
    " observations, where `balancing < 0` and `stock_addition >= -balancing` to ",
    "`stock_addition = stock_addition + balancing`.\n", sep = "")
cbs[balancing < 0 & !is.na(stock_addition) & stock_addition >= -balancing,
    `:=`(stock_addition = na_sum(stock_addition, balancing),
         stock_withdrawal = na_sum(stock_withdrawal, -balancing),
         balancing = 0)]

cat("\nAdjust uses proportionally for ", cbs[balancing < 0, .N],
    " observations, where `balancing < 0`", sep = "")
cbs[, divisor := na_sum(exports, other, processing, seed, food, feed, stock_addition)]
cbs[balancing < 0 & divisor >= -balancing,
    `:=`(stock_addition = round(na_sum(stock_addition, (balancing / divisor * stock_addition))),
         processing = round(na_sum(processing, (balancing / divisor * processing))),
         exports = round(na_sum(exports, (balancing / divisor * exports))),
         other = round(na_sum(other, (balancing / divisor * other))),
         seed = round(na_sum(seed, (balancing / divisor * seed))),
         food = round(na_sum(food, (balancing / divisor * food))),
         feed = round(na_sum(feed, (balancing / divisor * feed))),
         balancing = 0)]
cbs[, `:=`(stock_withdrawal = -stock_addition,
           divisor = NULL)]

# Rebalance table
cbs[, balancing := na_sum(production, imports, stock_withdrawal,
                          -exports, -food, -feed, -seed, -losses, -processing, -other, -unspecified)]

# Attribute rest (resulting from rounding differences) to stock changes
cbs[balancing < 0,
    `:=`(stock_addition = na_sum(stock_addition, balancing),
         stock_withdrawal = na_sum(stock_withdrawal, -balancing),
         balancing = 0)]

# Adjust 'total_supply' to new production values
cbs[, total_supply := na_sum(production, imports)]

cat("\nSkip capping 'exports', 'seed' and 'processing' at",
  "'total_supply + stock_withdrawal'.\n")


# # Balance CBS imports and exports -------------------------------------------------------
# # --> This balancing step was moved to script 05_balance.R
#
# # Adjust CBS to have equal export and import numbers per item per year
# # This is very helpful for the iterative proportional fitting of bilateral trade data
# cbs_bal <- cbs[, list(exp_t = sum(exports, na.rm = TRUE), imp_t = sum(imports, na.rm = TRUE)),
#                by = c("year", "item_code", "item")]
# cbs_bal[, `:=`(diff = na_sum(exp_t, -imp_t), exp_t = NULL, imp_t = NULL,
#                area_code = 999, area = "RoW")]
# # Absorb the discrepancies in "RoW"
# cbs <- merge(cbs, cbs_bal,
#              by = c("year", "item_code", "item", "area_code", "area"), all = TRUE)
# cbs[area_code == 999, `:=`(
#   exports = ifelse(diff < 0, na_sum(exports, -diff), exports),
#   imports = ifelse(diff > 0, na_sum(imports, diff), imports))]
# cbs[, diff := NULL]
#
# rm(cbs_bal); gc()
#
# # Rebalance RoW
# cbs[, balancing := na_sum(production, imports, stock_withdrawal,
#                           -exports, -food, -feed, -seed, -losses, -processing, -other, -unspecified)]
#
# cbs[, divisor := na_sum(other, processing, seed, food, feed, stock_addition, unspecified)]
# cbs[balancing < 0 & divisor >= -balancing,
#     `:=`(stock_addition = round(na_sum(stock_addition, (balancing / divisor * stock_addition))),
#          unspecified = round(na_sum(unspecified, (balancing / divisor * unspecified))),
#          processing = round(na_sum(processing, (balancing / divisor * processing))),
#          other = round(na_sum(other, (balancing / divisor * other))),
#          seed = round(na_sum(seed, (balancing / divisor * seed))),
#          food = round(na_sum(food, (balancing / divisor * food))),
#          feed = round(na_sum(feed, (balancing / divisor * feed))),
#          balancing = 0)]
# cbs[, `:=`(stock_withdrawal = -stock_addition, divisor = NULL,
#            balancing = na_sum(production, imports, stock_withdrawal,
#           -exports, -food, -feed, -seed, -losses, -processing, -other, -unspecified))]
# cbs[balancing < 0,
#     `:=`(production = na_sum(production, -balancing),
#          balancing = 0)]
# cbs[, total_supply := na_sum(production, imports)]



cat("\nAllocate remaining supply from 'unspecified' and 'balancing' to uses.\n")

cat("\nHops, oil palm fruit, palm kernels, sugar crops and live animals to 'processing'.\n")
cbs[item_code %in% c(254, 328, 677, 866, 946, 976, 1016, 1034, 2029, 1096, 1107, 1110,
  1126, 1157, 1140, 1150, 1171, 2536, 2537, 2562) & na_sum(unspecified, balancing) > 0,
  `:=`(processing = na_sum(processing, unspecified, balancing),
       unspecified = 0, balancing = 0)]

cat("\nNon-food crops to 'other'.\n")
cbs[item_code %in% c(2662, 2663, 2664, 2665, 2666, 2667, 2671, 2672, 2659,
  1864, 1866, 1867, 2661, 2746, 2748, 2747) & na_sum(unspecified, balancing) > 0,
  `:=`(other = na_sum(other, unspecified, balancing),
       unspecified = 0, balancing = 0)]

cat("\nFeed crops to 'feed'.\n")
cbs[item_code %in% c(2000, 2001, 2555, 2559, 2590, 2591, 2592, 2593, 2594,
  2595, 2596, 2597, 2598, 2749) & na_sum(unspecified, balancing) > 0,
  `:=`(feed = na_sum(feed, unspecified, balancing),
       unspecified = 0, balancing = 0)]

cat("\nRest (mostly 'food', 'feed' and 'processing') remains in 'unspecified' and 'balancing'.\n")


# Save --------------------------------------------------------------------

saveRDS(cbs, "data/cbs_full.rds")
saveRDS(btd, "data/btd_full.rds")
