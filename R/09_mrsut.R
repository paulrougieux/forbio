
library("data.table")
library("Matrix")
source("R/01_tidy_functions.R")

regions <- fread("inst/regions.csv")
items <- fread("inst/products.csv")

sup <- readRDS("data/sup_final.rds")

cbs <- readRDS("data/cbs_final.rds")
btd <- readRDS("data/btd_final.rds")

use <- readRDS("data/use_final.rds")
use_fd <- readRDS("data/use_fd_final.rds")

years <- seq(1997, 2017)
areas <- sort(unique(cbs$area_code))
processes <- sort(unique(sup$proc_code))
commodities <- sort(unique(sup$com_code))


# Supply ---

# Template to always get full tables
template <- data.table(expand.grid(
  proc_code = processes, com_code = commodities, stringsAsFactors = FALSE))
setkey(template, proc_code, com_code)

# List with block-diagonal supply matrices, per year
mr_sup <- lapply(years, function(x) {

  matrices <- lapply(areas, function(y, sup_y) {
    # Get supply for area y and merge with the template
    sup_x <- sup_y[area_code == y, .(proc_code, com_code, production)]
    out <- if(nrow(sup_x) == 0) {
      template[, .(proc_code, com_code, production = 0)]
    } else {merge(template, sup_x, all.x = TRUE)}

    # Cast the datatable to convert into a matrix
    out <- tryCatch(data.table::dcast(out, proc_code ~ com_code,
                                      value.var = "production", fun.aggregate = sum, na.rm = TRUE, fill = 0),
                    error = function(e) {stop("Issue at ", x, "-", y, ": ", e)})

    # Return a (sparse) matrix of supply for region y and year x
    return(Matrix(data.matrix(out[, c(-1)]), sparse = TRUE,
                  dimnames = list(out$proc_code, colnames(out)[-1])))

  }, sup_y = sup[year == x, .(area_code, proc_code, com_code, production)])

  # Return a block-diagonal matrix with all countries for year x
  return(bdiag(matrices))
})

names(mr_sup) <- years

saveRDS(mr_sup, "data/mr_sup.rds")



# Bilateral supply shares ---

# Template to always get full tables
template <- data.table(expand.grid(
  from_code = areas, to_code = areas,
  com_code = commodities, stringsAsFactors = FALSE))
setkey(template, from_code, com_code, to_code)

# Yearly list of BTD in matrix format
# Note that btd_final includes not only re-export adjusted bilateral trade flows,
# but also domestic production for domestic use, i.e. it gives the sources
# (domestic and imported) of each country's domestic use of any item.
btd_cast <- lapply(years, function(x, btd_x) {
  # Cast to convert to matrix
  out <- data.table::dcast(merge(template,
                                 btd_x[year == x, .(from_code, to_code, com_code, value)],
                                 by = c("from_code", "to_code", "com_code"), all.x = TRUE),
                           from_code + com_code ~ to_code,
                           value.var = "value", fun.aggregate = sum, na.rm = TRUE, fill = 0)

  return(Matrix(data.matrix(out[, c(-1, -2)]), sparse = TRUE,
                dimnames = list(paste0(out$from_code, "-", out$com_code),
                                colnames(out)[c(-1, -2)])))

}, btd_x = btd[, .(year, from_code, to_code, com_code, value)])

names(btd_cast) <- years

# Get commodities and their positions from total supply for domestic use
comms <- gsub("(^[0-9]+)-(c[0-9]+)", "\\2", rownames(btd_cast[[1]]))
is <- as.numeric(vapply(unique(comms), function(x) {which(comms == x)},
  numeric(length(unique(areas)))))
js <- rep(seq(unique(comms)), each = length(unique(areas)))
# Matrix used to aggregate over commodities
agg <- Matrix::sparseMatrix(i = is, j = js)

# Build supply shares, per year
supply_shares <- lapply(btd_cast, function(x, agg, js) {
  # x_agg <- colSums(crossprod(x, agg)) # Aggregate total supply (all countries)
  x_agg <- crossprod(x, agg) # Aggregate total supply (per country)
  denom <- data.table(as.matrix(t(x_agg)))
  # Calculate shares (per country)
  out <- as.matrix(x / as.matrix(denom[rep(seq(length(commodities)), length(areas)), ]))
  out[!is.finite(out)] <- 0 # See Issue #75

  return(as(out, "Matrix"))
}, agg = agg, js = js)


# Use ---

# Template to always get full tables
template <- data.table(expand.grid(
  area_code = areas, proc_code = processes, com_code = commodities,
  stringsAsFactors = FALSE))
setkey(template, area_code, proc_code, com_code)

# List with use matrices, per year
use_cast <- lapply(years, function(x, use_x) {
  # Cast use to convert to a matrix
  out <- data.table::dcast(merge(template[, .(area_code, proc_code, com_code)],
    use_x[year == x, .(area_code, proc_code, com_code, use)],
    by = c("area_code", "proc_code", "com_code"), all.x = TRUE),
    com_code ~ area_code + proc_code,
    value.var = "use", fun.aggregate = sum, na.rm = TRUE, fill = 0)

  return(Matrix(data.matrix(out[, c(-1)]), sparse = TRUE,
    dimnames = list(out$com_code, colnames(out)[-1])))

}, use_x = use[, .(year, area_code, proc_code, com_code, use)])

# Apply supply shares to the use matrix
mr_use <- mapply(function(x, y) {
  # Repeat use values, then adapted according to shares
  mr_x <- x[rep(seq_along(commodities), length(areas)), ]
  n_proc <- length(processes)

  for(j in seq_along(areas)) { # Per country j
    mr_x[, seq(1 + (j - 1) * n_proc, j * n_proc)] <-
      mr_x[, seq(1 + (j - 1) * n_proc, j * n_proc)] * y[, j]
  }

  return(mr_x)
}, use_cast, supply_shares)

names(mr_use) <- years
saveRDS(mr_use, "data/mr_use.rds")


# Final Demand ---

# Template to always get full tables
template <- data.table(expand.grid(
  area_code = areas, com_code = commodities,
  variable = c("material_use", "energy_use"),
  stringsAsFactors = FALSE))
setkey(template, area_code, com_code, variable)

use_fd <- melt(use_fd[, .(year, area_code, com_code,
  material_use, energy_use)],
  id.vars = c("year", "area_code", "com_code"))

# List with final use matrices, per year
use_fd_cast <- lapply(years, function(x, use_fd_x) {
  # Cast final use to convert to a matrix
  out <- data.table::dcast(merge(template[, .(area_code, com_code, variable)],
    use_fd_x[year == x, .(area_code, com_code, variable, value)],
    by = c("area_code", "com_code", "variable"), all.x = TRUE),
    com_code ~ area_code + variable,
    value.var = "value", fun.aggregate = sum, na.rm = TRUE, fill = 0)

  Matrix(data.matrix(out[, -1]), sparse = TRUE,
    dimnames = list(out$com_code, colnames(out)[-1]))
}, use_fd[, .(year, area_code, com_code, variable, value)])

# Apply supply shares to the final use matrix
mr_use_fd <- mapply(function(x, y) {
  mr_x <- x[rep(seq_along(commodities), length(areas)), ]
  n_var <- length(unique(use_fd[,variable]))
  for(j in seq_along(areas)) { # Could do this vectorised
    mr_x[, seq(1 + (j - 1) * n_var, j * n_var)] <-
      mr_x[, seq(1 + (j - 1) * n_var, j * n_var)] * y[, j]
  }
  return(mr_x)
}, use_fd_cast, supply_shares)

mr_use_fd <- lapply(mr_use_fd, round)
names(mr_use_fd) <- years
saveRDS(mr_use_fd, "/mnt/nfs_fineprint/tmp/forbio/Y.rds")


