
library("data.table")
source("R/01_tidy_functions.R")

regions <- fread("inst/regions.csv")


# Colnames ----------------------------------------------------------------

rename <- c(
  "t" = "year",
  "k" = "item_code",
  "i" = "exporter",
  "j" = "importer",
  "v" = "1000 US$",
  "q" = "tonnes"
)


# BACI --------------------------------------------------------------------

cat("\nTidying BACI.\n")

baci <- readRDS("input/baci_sel.rds")

baci <- dt_rename(baci, rename, drop = TRUE)

# change Fmr Sudan to Sudan
baci[importer == 736, importer := 729]
baci[exporter == 736, exporter := 729]

# Country concordance
importer_match <- match(baci[["importer"]], regions[["baci_code"]])
exporter_match <- match(baci[["exporter"]], regions[["baci_code"]])
baci[, `:=`(importer = regions$name[importer_match],
  importer_code = regions$code[importer_match],
  exporter = regions$name[exporter_match],
  exporter_code = regions$code[exporter_match])]

baci <- dt_filter(baci, !is.na(importer) & !is.na(exporter))

# Convert from 1000 US$ to usd
baci[, `:=`(usd = `1000 US$` * 1000, `1000 US$` = NULL)]

# Introduce unit variable
baci <- melt(baci, measure.vars = c("usd", "tonnes"),
  variable.name = "unit", variable.factor = FALSE)

baci <- baci[unit != "usd"]

# Store
saveRDS(baci, "input/baci_tidy.rds")
rm(baci, importer_match, exporter_match)
