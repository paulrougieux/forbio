
# Trade -------------------------------------------------------------------

library("data.table")
source("R/00_prep_functions.R")
path_trade <- "input/"
path_baci <- "~/wu_shares/WU/Projekte/GRU/04_Daten/Socio-Economic data/BACI/"


# BACI ------------------------------------------------------------------

items <- fread("inst/items_baci.csv")
files <- c(c("baci_hs92" = "BACI_HS92_V202001.zip"),
           c("baci_hs12" = "BACI_HS12_V202001.zip"))

for(file in 1:2){
  file <- files[file]
  extr <- unzip(paste0(path_baci, file), list = TRUE)[[1]]
  name <- names(file)
  col_types <- rep(list(c("integer", "integer", "integer", "integer",
    "numeric", "numeric")), length(extr))
  baci <- fa_extract(path_in = path_baci, files = file, path_out = path_trade,
    name = name, extr = extr, col_types = col_types, stack = TRUE)
  baci[, version := substr(name, 6, 9)]
  saveRDS(baci, paste0(path_trade, name))
}

baci92 <- readRDS(paste0(path_trade, names(files[1])))
baci12 <- readRDS(paste0(path_trade, names(files[2])))

# Select wood fuel, residues, charcoal, particle board, recovered paper
baci92_sel <- baci92[grep("^(440[1-2].0|441010|4707..)", baci92$k), ]

# Filter years
baci92_sel[, version12 := items$version12[match(baci92_sel$k, items$baci_code)]]
baci92_sel <- baci92_sel[(is.na(version12) & t < 2012) | !is.na(version12)]

# Select wood fuel, residues, charcoal, particle board, OSB, recovered fibre pulp
baci12_sel <- baci12[grep("^(440[1,2][3,9].|44101.|470620)", baci12$k), ]

baci_sel <- rbind(baci92_sel[, .(t,i,j,k,v,q)], 
                  baci12_sel[, .(t,i,j,k,v,q)])

saveRDS(baci_sel, paste0(path_trade, "baci_sel.rds"))

