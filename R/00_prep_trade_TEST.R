# Trade -------------------------------------------------------------------

library("data.table")
source("R/00_prep_functions.R")
path_trade <- "input/"


# BACI92 ------------------------------------------------------------------

# Links to the files - missing

file <- c("baci92_full" = "BACI_HS92_V202001.zip")

# pattern <- "(BACI_HS92_Y[0-9]+)([.]csv)"

extr <- unzip(paste0(path_trade, file), list = TRUE)[[1]]
# extr <- extr[grep(pattern, extr)]

# name <- gsub(pattern, "\\1", extr)
name <- names(file)

col_types <- rep(list(c("integer", "integer", "integer", "integer",
                        "numeric", "numeric")), length(extr))

baci92_full <- fa_extract(path_in = path_trade, files = file, path_out = path_trade,
                        name = name, extr = extr, col_types = col_types, stack = TRUE)

baci92_sel <- readRDS(baci92_full)



# Select wood fuel, residues, charcoal, particle board, recovered paper

baci92_sel <- baci92_sel[grep("^(440[1-2].0|441010|4707..)", baci92_sel$k), ]

saveRDS(baci92_sel, paste0(path_trade, "baci92_sel.rds"))



# BACI12 ------------------------------------------------------------------

# Links to the files

#link <- ??? #Where is link or which directory?

file <- c("baci12_full" = "BACI_HS12_V202001.zip")

# pattern <- "(BACI_HS12_Y[0-9]+)([.]csv)"

extr <- unzip(paste0(path_trade, file), list = TRUE)[[1]]
# extr <- extr[grep(pattern, extr)]

# name <- gsub(pattern, "\\1", extr)
name <- names(file)

col_types <- rep(list(c("integer", "integer", "integer", "integer",
                        "numeric", "numeric")), length(extr))

baci12_full <- fa_extract(path_in = path_trade, files = file, path_out = path_trade,
                          name = name, extr = extr, col_types = col_types, stack = TRUE)

baci12_sel <- readRDS(baci12_full)


# Select wood fuel, residues, charcoal, particle board, OSB, recovered fibre pulp

baci12_sel <- baci12_sel[grep("^(440[1,2][3,9].|44101.|470620)", baci12_sel$k), ]

saveRDS(baci12_sel, paste0(path_trade, "baci12_sel.rds"))






# Testing 2014 --------------------------------------------------------------

baci12_2014 <- BACI_HS12_Y2014_V202001

baci12_2014_sel <- read (baci12_2014)

baci12_2014_sel <- baci12_2014_sel[grep("^(440[1,2][3,9].|44101.|470620)", baci12_2014_sel$k), ]

