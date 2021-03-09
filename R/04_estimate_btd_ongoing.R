
library("data.table")
library("Matrix")
source("R/01_tidy_functions.R")



# BTD ---------------------------------------------------------------------

cat("\nEstimating BTD from CBS.\n")

cbs <- readRDS("data/cbs.rds")

# dt_replace(cbs, fun = is.na, value = 0, cols = c("imports", "exports"))

# Cast import and export columns
cbs_imp <- data.table::dcast(cbs[year %in% year, c("area_code", "year", "com_code", "imports")],
  year + com_code ~ area_code, value.var = "imports", fun.aggregate = na_sum,
  fill = 0)
cbs_exp <- data.table::dcast(cbs[year %in% year, c("area_code", "year", "com_code", "exports")],
  year + com_code ~ area_code, value.var = "exports", fun.aggregate = na_sum,
  fill = 0)

rm(cbs); gc()

# Check equivalence of year, com_code and areas.
stopifnot(all(cbs_exp[, c(1, 2)] == cbs_imp[, c(1, 2)]),
  all(names(cbs_exp) == names(cbs_imp)))
cbs_ids <- cbs_imp[, c("year", "com_code")]

# Use the Matrix package for efficient operations
cbs_imp <- as(cbs_imp[, c(-1, -2)], "Matrix")
cbs_exp <- as(cbs_exp[, c(-1, -2)], "Matrix")


spread_trade <- function(x, split_matr, inp_matr) {
  split_matr[, x] <- 0 # No internal "exports" / "imports"
  split_sums <- rowSums(split_matr, na.rm = TRUE)
  split_sums <- ifelse(split_sums == 0, NA, split_sums) # Avoid NaN
  split_matr / split_sums * inp_matr[, x]
}

build_estimates <- function(name, list, ids, kick_0 = TRUE) {
  x <- list[[name]]
  out <- melt(cbind(ids, as.matrix(x)),
              id.vars = c("year", "com_code"), na.rm = TRUE,
              variable.name = "area_code", variable.factor = FALSE)
  if(nrow(out) == 0) {return(NULL)}
  # Make sure encoding is right, to reduce memory load
  out[, `:=`(year = as.integer(year), com_code = as.integer(com_code),
    area_code = as.integer(area_code), inp_code = as.integer(area_code))]
  # Consider not carrying over 0 values
  if(kick_0) {out[value != 0, ]} else{out}
}


cat("\nPreparing to estimate trade shares. Around (16GB?) of RAM are required.\n")

# Spread exports according to import shares
est_exp <- lapply(colnames(cbs_imp), spread_trade, cbs_imp, cbs_exp)
names(est_exp) <- colnames(cbs_imp)

##ZR: starting here I got an error, see #04_Error.R;
##I then corrected sth earlier; it worked but I got warnings

est_exp <- lapply(colnames(cbs_imp), build_estimates, est_exp, cbs_ids)

est_exp <- rbindlist(est_exp)
est_exp <- est_exp[, .(year, com_code,
  to_code = area_code, from_code = inp_code, exp_spread = value)]

# Spread imports according to export shares
est_imp <- lapply(colnames(cbs_exp), spread_trade, cbs_exp, cbs_imp)
names(est_imp) <- colnames(cbs_exp)
est_imp <- lapply(colnames(cbs_exp), build_estimates, est_imp, cbs_ids)
est_imp <- rbindlist(est_imp)
est_imp <- est_imp[, .(year, com_code,
  from_code = area_code, to_code = inp_code, imp_spread = value)]

##ZR: After this I got again warning "NAs introdcuted by coercion"
## Error in eval(jsub, SDenv, parent.frame()) : object 'area' not found
#6.eval(jsub, SDenv, parent.frame())
#5.eval(jsub, SDenv, parent.frame())
#4.`[.data.table`(out, , `:=`(year = as.integer(year), com_code = as.integer(com_code), 
#                         area_code = as.integer(area_code), inp_code = as.integer(area)))
#3.out[, `:=`(year = as.integer(year), com_code = as.integer(com_code), 
#           area_code = as.integer(area_code), inp_code = as.integer(area))]
#2.FUN(X[[i]], ...)
#1.lapply(colnames(cbs_imp), build_estimates, est_exp, cbs_ids)

## I think error is in this function
# build_estimates <- function(name, list, ids, kick_0 = TRUE) {
#   x <- list[[name]]
#   out <- melt(cbind(ids, as.matrix(x)),
#      id.vars = c("year", "com_code"), na.rm = TRUE,
#      variable.name = "area_code", variable.factor = FALSE)
#   if(nrow(out) == 0) {return(NULL)}
#   # Make sure encoding is right, to reduce memory load
#    out[, `:=`(year = as.integer(year), com_code = as.integer(com_code),
#        area_code = as.integer(area_code), inp_code = as.integer(area))]
#      # Consider not carrying over 0 values
#      if(kick_0) {out[value != 0, ]} else{out}
#      }
## Exactly in this:
# area_code = as.integer(area_code), inp_code = as.integer(area_code))]

# but then I got other warning messages
#Warning messages:
# 1: In eval(jsub, SDenv, parent.frame()) : NAs introduced by coercion

rm(cbs_exp, cbs_imp, cbs_ids); gc()

# Merge import-share and export-share based estimates
btd_est <- merge(est_exp, est_imp,
  by = c("year", "com_code", "from_code", "to_code"), all = TRUE)

##ZR:Got here another error:
##Error in vecseq(f__, len__, if (allow.cartesian || notjoin || !anyDuplicated(f__,  : 
##Join results in more than 2^31 rows (internal vecseq reached physical limit).
##Very likely misspecified join.
##Check for duplicate key values in i each of which join to
##the same group in x over and over again. If that's ok, try by=.EACHI 
##to run j for each group to avoid the large allocation. 
##Otherwise, please search for this error message in the FAQ, 
##Wiki, Stack Overflow and data.table issue tracker for advice.

##I checked and est_exp and est_imp are missing com_code

rm(est_exp, est_imp); gc()

# Average the estimates - note that 0 estimates may be considered NA
btd_est[, `:=`(value = (imp_spread + exp_spread) / 2,
  exp_spread = NULL, imp_spread = NULL)]

# Store result ------------------------------------------------------------
saveRDS(btd_est, "data/btd_est.rds")
