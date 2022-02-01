library("data.table")
library("tidyverse")
library("Matrix")

years <- 1997:2017

# Read Z and Y
Z <- readRDS("/mnt/nfs_fineprint/tmp/forbio/Z.rds")

Y <- readRDS("/mnt/nfs_fineprint/tmp/forbio/Y.rds")

# Read carbon tcf
carbon_cf <- fread("inst/carbon_tidy.csv")

for (year in years) {
  Z[[as.character(year)]] <- Z[[as.character(year)]] * carbon_cf$tcf_carbon
  Y[[as.character(year)]] <- Y[[as.character(year)]] * carbon_cf$tcf_carbon
}

# Derive total output X ---------------------------------------------

X <- mapply(function(x, y) {
  rowSums(x) + rowSums(y)
}, x = Z, y = Y)



# Store X, Z variables
saveRDS(Z, "/mnt/nfs_fineprint/tmp/forbio/Z.rds")
saveRDS(X, "/mnt/nfs_fineprint/tmp/forbio/X.rds")