
library("Matrix")


# MRIO Table ---

mr_sup <- readRDS("data/mr_sup.rds")
mr_use <- readRDS("data/mr_use.rds")

# Mass
trans <- lapply(mr_sup, function(x) {
  out <- as.matrix(x / rowSums(x))
  out[!is.finite(out)] <- 0 # See Issue #75
  return(as(out, "Matrix"))
})

Z <- mapply(function(x, y) {
  x %*% y
}, x = mr_use, y = trans)

Z <- lapply(Z, round)





# Derive total output X ---------------------------------------------

X <- mapply(function(x, y) {
  rowSums(x) + rowSums(y)
}, x = Z, y = Y)



# Store X, Z variables
saveRDS(Z, "/mnt/nfs_fineprint/tmp/forbio/Z.rds")
saveRDS(X, "/mnt/nfs_fineprint/tmp/forbio/X.rds")

