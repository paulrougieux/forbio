
library("data.table")
library("Matrix")
library("mipfp")

source("R/01_tidy_functions.R")

years <- 1997:2017


# BTD ---------------------------------------------------------------------

btd <- readRDS("data/btd.rds")
btd_est <- readRDS("data/btd_est.rds")
cbs <- readRDS("data/cbs.rds")

areas <- unique(cbs$area_code)
items <- unique(cbs$com_code)


# Prepare for creating balanced BTD sheets --------------------------------

# Subset to only keep relevant units, years and items
btd <- btd[unit %in% c("tonnes", "m3") & com_code %in% items &
             from_code %in% areas & to_code %in% areas, ]
btd_est <- btd_est[year %in% years & com_code %in% items &
                     from_code %in% areas & to_code %in% areas, ]

# Get info on target trade from CBS
target <- cbs[year %in% years, c("year", "area_code", "com_code", "exports", "imports")]

# Create a structure to map importers to exporters per item (+ targets)
mapping_templ <- data.table(
  from_code = rep(areas, each = length(areas), times = length(items)),
  to_code = rep(areas, times = length(areas) * length(items)),
  com_code = rep(items, each = length(areas) ^ 2))
constr_templ <- data.table(
  area_code = rep(areas, each = length(items)),
  com_code = rep(items, times = length(areas)))

# Fill this structure per year with (1) btd values, (2) estimated values
# Then do some iterative proportional fitting to approximate target values
# Note that we loop this over years, so memory requirements can easily be
# reduced if necessary.
btd_bal <- vector("list", length(years))
names(btd_bal) <- as.character(years)

for(i in seq_along(years)) {
  y <- years[i]
  # Add BTD values to the template
  mapping <- merge(mapping_templ,
    btd[year == y, c("from_code", "to_code", "com_code", "value")],
    by = c("from_code", "to_code", "com_code"), all.x = TRUE)
  # Add estimates
  mapping <- merge(mapping,
    btd_est[year == y, .(from_code, to_code, com_code, val_est = value)],
    by = c("from_code", "to_code", "com_code"), all.x = TRUE)
  # Prepare target-constraints for RAS
  constraint <- merge(constr_templ,
    target[year == y, c("area_code", "com_code", "imports", "exports")],
    by = c("area_code", "com_code"), all.x = TRUE)
  # Balance imports and exports
  # Adjust constraints to have equal export and import numbers per item per year
  # This is very helpful for the iterative proportional fitting of bilateral trade data
  trade_bal <- constraint[, list(exp_t = sum(exports, na.rm = TRUE),
    imp_t = sum(imports, na.rm = TRUE)), by = c("com_code")]
  trade_bal <- trade_bal[, mean_t := (exp_t + imp_t) / 2]
  constraint <- merge(constraint, trade_bal,
    by = c("com_code"), all.x = TRUE)
  constraint[, `:=`(imports = imports / imp_t * mean_t,
                    exports = exports / exp_t * mean_t)]
  # Replace NA constraints with 0
  constraint[, `:=`(imports = ifelse(is.na(imports), 0, imports),
                    exports = ifelse(is.na(exports), 0, exports))]
  
  # Eliminate estimates where data exist
  mapping[, val_est := ifelse(is.na(value), val_est, NA)]
  # Calculate totals for values and estimates per importing country and item
  mapping[, `:=`(value_sum = na_sum(value), val_est_sum = na_sum(val_est)),
          by = c("to_code","com_code")]
  # Add import target
  mapping[, val_target := constraint$imports[match(paste(mapping$to_code, mapping$com_code),
                                                   paste(constraint$area_code, constraint$com_code))]]
  # Calculate import gap
  mapping[, gap := na_sum(val_target, -value_sum)]
  # Downscale import estimates in order not to exceed the total gap between reported exports and target values
  mapping[, val_est := ifelse(gap > 0, ifelse(gap < val_est_sum, val_est / val_est_sum * gap, val_est), NA)]

  # Assign estimates to value column with a weight of 50%
  mapping[, `:=`(
    value = ifelse(is.na(value), ifelse(is.na(val_est), 0, val_est * 0.5), value),
    val_est = NULL)]


  # Restructure in a list with matrices per item
  mapping_ras <- lapply(
    split(mapping, by = "com_code", keep.by = FALSE),
    function(x) {
      out <- data.table::dcast(x, from_code ~ to_code,
        fun.aggregate = sum, value.var = "value")[, -"from_code"]
      as(out, "Matrix")})

  # Run iterative proportional fitting per item
  for(j in items) {
    mapping_ras[[j]] <- Ipfp(mapping_ras[[j]],
      target.list = list(1, 2), iter = 100, tol.margins = 1E5,
      target.data = constraint[com_code == j, .(round(exports), round(imports))])$x.hat
  }

  btd_bal[[i]] <- lapply(names(mapping_ras), function(name) {
    out <- mapping_ras[[name]]
    out <- data.table(from_code = colnames(out), as.matrix(out))
    out <- melt(out, id.vars = c("from_code"), variable.name = "to_code", variable.factor = FALSE)
    out[, .(year = y, com_code = name,
      from_code = as.integer(from_code), to_code = as.integer(to_code), value)]
  })

  cat("Calculated year ", y, ".\n", sep = "")
}

# One datatable per year
btd_bal <- lapply(btd_bal, rbindlist)
# One datatable
btd_bal <- rbindlist(btd_bal)

# Update imports and exports in CBS
imports <- btd_bal %>% group_by(year, com_code, area_code = to_code) %>% 
  summarise(value = na_sum(value))
exports <- btd_bal %>% group_by(year, com_code, area_code = from_code) %>% 
  summarise(value = na_sum(value))
cbs <- merge(cbs, imports, by = c("year", "com_code", "area_code"), all.x = TRUE)
cbs[!is.na(value), imports := value]
cbs[, value := NULL]
cbs <- merge(cbs, exports, by = c("year", "com_code", "area_code"), all.x = TRUE)
cbs[!is.na(value), exports := value]
cbs[, value := NULL]


# Store the balanced sheets -----------------------------------------------

saveRDS(btd_bal, "data/btd_bal.rds")
saveRDS(cbs, "data/cbs_trade_bal.rds")
