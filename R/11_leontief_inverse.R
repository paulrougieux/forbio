library("data.table")
library("Matrix")


# Leontief inverse ---

prep_solve <- function(year, Z, Y, X,
                       adj_X = FALSE, adj_A = TRUE, adj_diag = FALSE) {

  if(adj_X) {X <- X + 1e-10}

  A <- Matrix(0, nrow(Z), ncol(Z))
  idx <- X != 0
  A[, idx] <- t(t(Z[, idx]) / X[idx])
  if(adj_A) {A[A < 0] <- 0}

  if(adj_diag) {diag(A)[diag(A) == 1] <- 1 - 1e-10}
  L <- .sparseDiagonal(nrow(A)) - A

  lu(L) # Computes LU decomposition and stores it in L

  L_inv <- solve(L, tol = .Machine[["double.eps"]])

  return(L_inv)
}


years <- seq(1997, 2017)
# years_singular <- c(1986,1994,2002,2009)

Z <- readRDS("/mnt/nfs_fineprint/tmp/forbio/Z.rds")
Y <- readRDS("/mnt/nfs_fineprint/tmp/forbio/Y.rds")
X <- readRDS("/mnt/nfs_fineprint/tmp/forbio/X.rds")


for(year in years){

  print(year)

  adjust <- FALSE # ifelse(year %in% years_singular, TRUE, FALSE)

  L <- prep_solve(year = year, Z = Z[[as.character(year)]],
                  Y = Y[[as.character(year)]], X = X[, as.character(year)],
                  adj_diag = adjust)
  saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/forbio/", year, "_L.rds"))

}


