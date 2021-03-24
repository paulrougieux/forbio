
library("data.table")
library("Matrix")
source("R/01_tidy_functions.R")


years <- 1997:2017


# BTD ---------------------------------------------------------------------

btd <- readRDS("data/btd_bal.rds")
cbs <- readRDS("data/cbs.rds")

areas <- sort(unique(cbs$area_code))
items <- sort(unique(cbs$com_code))



# Prepare reallocation of re-exports --------------------------------------

cbs[, dom_use := na_sum(use, balancing)]
cbs[, total_use := na_sum(dom_use, exports)]

# Create a structure to map importers to exporters per item (+ targets)
mapping_templ <- data.table(
  from_code = rep(areas, each = length(areas), times = length(items)),
  to_code = rep(areas, times = length(areas) * length(items)),
  com_code = rep(items, each = length(areas) ^ 2))

# Fill this structure per year btd values
# Then do re-export reallocation via the Leontief inverse for each item
# Note that we loop this over years, so memory requirements can easily be
# reduced if necessary.
btd_final <- vector("list", length(years))
names(btd_final) <- years

for(i in seq_along(years)) {
  y <- years[i]
  # Add BTD values to the template
  mapping <- merge(mapping_templ,
                   btd[year == y, c("from_code", "to_code", "com_code", "value")],
                   by = c("from_code", "to_code", "com_code"), all.x = TRUE)

  # Eliminate NA values
  mapping[is.na(value), value := 0]

  # Restructure in a list with matrices per item
  mapping_reex <- lapply(
    split(mapping, by = "com_code", keep.by = FALSE),
    function(x) {
      out <- data.table::dcast(x, from_code ~ to_code,
                               fun.aggregate = sum, value.var = "value")[, -"from_code"]
      as(out, "Matrix")})

  # Run re-export reallocation per item
  for(j in items) {
    data <- merge(data.table(area_code = areas),
                  cbs[year==y & com_code==j,
                      .(area_code, production, dom_use, total_use, dom_share = production / total_use)],
                  by = "area_code", all = TRUE)
    data[is.na(dom_use), dom_use := 0]
    data[is.na(total_use), total_use := 0]
    data[is.na(dom_share), dom_share := 0]

    denom <- data$total_use
    denom[denom == 0] <- 1
    mat <- mapping_reex[[j]]
    mat <- t(t(mat) / denom)
    mat <- diag(nrow(mat)) - mat
    mat <- solve(mat)
    mat <- mat * data$dom_share
    mat <- t(t(mat) * data$dom_use)

    mapping_reex[[j]] <- mat
  }

  btd_final[[i]] <- lapply(names(mapping_reex), function(name) {
    out <- mapping_reex[[name]]
    colnames(out) <- areas
    out <- data.table(from_code = areas, as.matrix(out))
    out <- melt(out, id.vars = c("from_code"), variable.name = "to_code", variable.factor = FALSE)
    out[, .(year = y, com_code = name,
            from_code = as.integer(from_code), to_code = as.integer(to_code), value)]
  })

  cat("Calculated year ", y, ".\n", sep = "")
}

# One datatable per year
btd_final <- lapply(btd_final, rbindlist)
# One datatable
btd_final <- rbindlist(btd_final)


# Store the balanced sheets -----------------------------------------------
saveRDS(btd_final, "data/btd_final.rds")
