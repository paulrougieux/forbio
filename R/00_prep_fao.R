
# FAO data ----------------------------------------------------------------

library("data.table")
source("R/00_prep_functions.R")
path_fao <- "input/"


# Settings ----------------------------------------------------------------

files <- c(
  "fore_prod" = "Forestry_E_All_Data_(Normalized).zip",
  "fore_trad" = "Forestry_Trade_Flows_E_All_Data_(Normalized).zip")

# Files to extract from the ZIP archives
extr <- NA

name <- names(files)

# Links to the files
link <- "http://fenixservices.fao.org/faostat/static/bulkdownloads/"

# Column types to possibly skip some
col_types <- list(
  "fore_prod" = c("numeric", "character", "numeric", "character", "numeric",
    "character", "numeric", "numeric", "character", "numeric", "character"),
  "fore_trad" = c("numeric", "character", "numeric", "character", "numeric",
    "character", "numeric", "character", "numeric", "numeric", "character",
    "numeric", "character")
)


# Execute -----------------------------------------------------------------

fa_dl(file = files, link = link, path = path_fao)

fa_extract(path_in = path_fao, files = files,
  path_out = path_fao, name = name, extr = extr, col_types = col_types)



