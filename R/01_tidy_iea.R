
library("data.table")
source("R/01_tidy_functions.R")

regions <- fread("inst/regions.csv")
products <- fread("inst/products.csv")

# Colnames ----------------------------------------------------------------

rename <- c(
  "Country" = "iea_area",
  "Unit" = "unit",
  # "Product" = "product",
  # "Flow" = "flow",
  "Year" = "year",
  "Value" = "value"
)


# Energy ----------------------------------------------------------------

cat("\nTidying energy data.\n")

# read iea data
energy <- fread("input/iea_energy.csv")
# IEA data are available from the OECD data portal. 
# Download the flow "Total Energy Supply" for the product "PRIMSBIO"
# and convert into long format.
energy_area <- fread("inst/iea_area.csv")

energy <- dt_rename(energy, rename, drop = TRUE)
energy[value == "..", `:=`(value = NA)]
energy[, `:=`(value = as.numeric(value))]

energy <- merge(energy, energy_area, by = c("iea_area"))
energy <- energy[!is.na(energy$area_code),]

# tidy regions
# Belgium-Luxembourg before 2000 together
energy[area_code %in% 255:256 & year<2000, area_code := 15]
energy[area_code==272 & year<2006, area_code := 186]
energy <- energy[, list(value = na_sum(value)), by = c("area_code", "year", "unit")]

energy <- area_fix(energy, regions)


# Store
saveRDS(energy, "input/energy.rds")
