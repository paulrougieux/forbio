
library("data.table")
library(tidyverse)

regions <- fread("inst/regions.csv")
items <- fread("inst/products.csv")


# Supply ------------------------------------------------------------------

btd <- readRDS("data/btd_final.rds")
cbs <- readRDS("data/cbs.rds")
sup <- fread("inst/sup.csv")
shares <- fread("inst/tcf_sup_tidy.csv")


cat("Allocate production to supplying processes.\n")

shares <- merge(shares, sup[type=="100%", c("proc_code", "com_code")],
  by = "proc_code", all.x = TRUE)

# Allocate production to supplying processes including double-counting
sup <- merge(
  cbs[, c("area_code", "area", "year", "com_code", "item", "production")],
  sup[com_code %in% unique(cbs$com_code)],
  by = c("com_code", "item"), all = TRUE, allow.cartesian = TRUE)

# Downscale double-counted production
cat("Calculate supply shares for multi-output processes.\n")

shares <- merge(shares, cbs[, c("area_code", "year", "com_code", "production")],
  by = c("area_code", "com_code"), allow.cartesian = TRUE)

# derive roundwood equivalents (rwe)
shares[, rwe := production / product]
shares[, `:=`(chips = round(chips * rwe), residues = round(residues * rwe),
  product = production, production = NULL, rwe = NULL)]

# ZR: Change units from m3rw to m3p
# for chips and residues
tcf <- fread("inst/tcf_use_tidy.csv")
tcf <- rbind(tcf[com_code == "c17" & unit == "m3p/m3sw",],
             tcf[com_code == "c18" & unit == "m3p/m3sw",])
tcf = subset(tcf, select = -c(item) )
tcf_wide <- pivot_wider(tcf, names_from = com_code, values_from = tcf)
names(tcf_wide)[names(tcf_wide) == "c17"] <- "tcf_chips"
names(tcf_wide)[names(tcf_wide) == "c18"] <- "tcf_residues"

# merge 
shares <- merge(shares, tcf_wide[, c("tcf_chips", "tcf_residues")],
                by = c("area_code"), all.x = TRUE, allow.cartesian = TRUE)
# Error: Elements listed in 'by' must be valid column names in x and y

shares <- merge(shares,
  shares[, list(chips_total = sum(chips, na.rm = TRUE),
  residues_total = sum(residues, na.rm = TRUE)),
  by = c("area_code","year")],
  by = c("area_code","year"), all.x = TRUE)

shares <- merge(shares, cbs[com_code=="c17", .(area_code, year, chips_cbs = production)],
  by = c("area_code","year"), all.x = TRUE)

shares <- merge(shares, cbs[com_code=="c18", .(area_code, year, residues_cbs = production)],
  by = c("area_code","year"), all.x = TRUE)

shares[, `:=`(chips_scale = chips_cbs / chips_total,
  residues_scale = residues_cbs / residues_total)]

# Annahme: if scale < 1 : rescale
# if scale > 1 : use own estimation, and difference is allocation to p01 and p02


## ------------------------------------


sup <- merge(sup, shares[, .(area_code, proc_code, year, product, chips, residues)], 
  by = c("area_code", "proc_code", "year"), all.x = TRUE)


# Add regions to RoW if not included in CBS
shares[, `:=`(area = ifelse(!area_code %in% regions$code[regions$cbs], "RoW", area),
              area_code = ifelse(!area_code %in% regions$code[regions$cbs], 999, area_code))]

# Aggregate values
shares <- shares[, list(value = sum(value, na.rm = TRUE)),
  by = list(area_code, area, year, proc_code, proc,
    comm_code, item_code, item)]
# Add totals
shares <- merge(
  shares, all.x = TRUE,
  shares[, list(total = sum(value, na.rm = TRUE)),
    by = list(area_code, area, year, comm_code, item_code, item)])

shares[, share := value / total]

sup <- merge(sup,
  shares[, c("area_code", "area", "year", "comm_code", "proc_code", "share")],
  by = c("area_code", "area", "year", "comm_code", "proc_code"), all.x = TRUE)

cat("Applying livestock shares to",
  sup[comm_code %in% shares$comm_code, .N], "observations.\n")
sup[is.na(share) & comm_code %in% shares$comm_code, production := 0]
sup[!is.na(share) & comm_code %in% shares$comm_code,
  production := production * share]

cat("Applying oil extraction shares to",
  sup[comm_code %in% c("c090"), .N],
  "observations of oilseed cakes.\n")
shares_o <- sup[comm_code %in% c("c079", "c080", "c081"),
  list(proc, share_o = production / sum(production, na.rm = TRUE)),
  by = list(area_code, year)]

sup <- merge(sup, shares_o, by = c("area_code", "year", "proc"), all.x = TRUE)
sup[is.na(share_o), share_o := 0]
sup[is.na(share) & comm_code %in% c("c090"),
  `:=`(production = production * share_o)]
sup[, share_o := NULL]

sup[, share := NULL]


# # Fill prices using BTD ---------------------------------------------------
# 
# prices <- as.data.table(data.table::dcast(btd, from + from_code + to + to_code +
#   item + item_code + year ~ unit, value.var = "value"))
# prices <- prices[!is.na(usd) & usd > 0,
#   list(usd = sum(usd, na.rm = TRUE), head = sum(head, na.rm = TRUE),
#     tonnes = sum(tonnes, na.rm = TRUE)),
#     by = list(from, from_code, item_code, item, year)]
# 
# prices[, price := ifelse(tonnes != 0 & !is.na(tonnes), usd / tonnes,
#   ifelse(head != 0 & !is.na(head), usd / head, NA))]
# 
# # Cap prices at 5th and 95th quantiles.
# # We might want to add a yearly element.
# caps <- prices[, list(price_q95 = quantile(price, .95, na.rm = TRUE),
#   price_q50 = quantile(price, .50, na.rm = TRUE),
#   price_q05 = quantile(price, .05, na.rm = TRUE)),
#   by = list(item)]
# prices <- merge(prices, caps, by = "item", all.x = TRUE)
# 
# cat("Capping ", prices[price > price_q95 | price < price_q05, .N],
#   " prices at the specific item's 95th and 5th quantiles.\n", sep = "")
# prices[, price := ifelse(price > price_q95, price_q95,
#   ifelse(price < price_q05, price_q05, price))]
# 
# # Get worldprices to fill gaps
# na_sum <- function(x) {ifelse(all(is.na(x)), NA_real_, sum(x, na.rm = TRUE))}
# prices_world <- prices[!is.na(usd), list(usd = na_sum(usd),
#   tonnes = na_sum(tonnes), head = na_sum(head)),
#   by = list(item, item_code, year)]
# prices_world[, price_world := ifelse(head != 0, usd / head,
#   usd / tonnes)]
# prices <- merge(
#   prices, prices_world[, c("year", "item_code", "item", "price_world")],
#   by = c("year", "item_code", "item"), all.x = TRUE)
# 
# cat("Filling ", prices[is.na(price) & !is.na(price_world), .N],
#   " missing prices with worldprices.\n", sep = "")
# prices[is.na(price), price := price_world]
# 
# cat("Filling ", prices[!is.finite(price) & !is.na(price_q50), .N],
#   " missing prices with median item prices.\n", sep = "")
# prices[!is.finite(price), price := price_q50]
# 
# sup <- merge(sup, all.x = TRUE,
#   prices[, c("from_code", "from", "item", "item_code", "year", "price")],
#   by.x = c("area_code", "area", "item", "item_code", "year"),
#   by.y = c("from_code", "from", "item", "item_code", "year"))
# 
# # apply world average price where price is NA
# sup <- merge(sup, all.x = TRUE,
#   prices_world[, c("item", "item_code", "year", "price_world")],
#   by.x = c("item", "item_code", "year"),
#   by.y = c("item", "item_code", "year"))
# sup[, `:=`(price = ifelse(is.na(price), price_world, price),
#            price_world = NULL)]


# Store results -----------------------------------------------------------

saveRDS(sup, "data/sup.rds")
