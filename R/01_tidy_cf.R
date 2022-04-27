
library("data.table")
library("tidyverse")
source("R/01_tidy_functions.R")
`%nin%` = Negate(`%in%`)

regions <- fread("inst/regions.csv")
products <- fread("inst/products.csv")


# MB SUP ---------------------------------------------------------------------------------------------------
cat("\nTidying material balances for supply tables.\n")

cat("\nStep 1: Tidying material balances of sawnwood C (p04) and NC (p05) production.\n")

# Read files
sup_sawnwood <- fread("inst/mb_sawnwood_raw.csv")
sup_sawnwood <- sup_sawnwood[, `:=`(product = as.numeric(product), chips = as.numeric(chips), 
                                    sawdust = as.numeric(sawdust), shavings = as.numeric(shavings),
                                    shrinkage = as.numeric(shrinkage))]

# Aggregate and rename items
sup_sawnwood[,  `:=`(residues = na_sum(sup_sawnwood$sawdust, sup_sawnwood$shavings))]
sup_sawnwood[, `:=`(sawdust = NULL, shavings = NULL)]
sup_sawnwood <- sup_sawnwood[, c("area_code", "area", "proc_code", 
                                 "process", "product","chips","residues","shrinkage")]

# Apply continental average MB where no country-specific value available
sup_cont <- sup_sawnwood[is.na(area_code)]
sup_sawnwood <- merge(sup_sawnwood, regions[, .(area_code, continent)],
                      by = "area_code", all.x = TRUE)
sup_sawnwood[continent == "EU", continent := "EUR"]
sup_sawnwood <- merge(sup_sawnwood, sup_cont[, .(continent = area, proc_code, 
  c_product = product, c_chips = chips, c_residues = residues, c_shrinkage = shrinkage)],
  by = c("continent", "proc_code"), all.x = TRUE)

# Rescale continental average values to sum up to 100
sup_sawnwood[na_sum(c_product, c_chips, c_residues, c_shrinkage) != 100,
             `:=`(c_chips = c_chips/na_sum(c_product, c_chips, c_residues, c_shrinkage)*100, 
                  c_product = c_product/na_sum(c_product, c_chips, c_residues, c_shrinkage)*100, 
                  c_residues = c_residues/na_sum(c_product, c_chips, c_residues, c_shrinkage)*100, 
                  c_shrinkage = c_shrinkage/na_sum(c_product, c_chips, c_residues, c_shrinkage)*100)]

# Fill gaps with continental values
sup_sawnwood[!is.na(product) & is.na(na_sum(chips, residues, shrinkage)), 
    `:=`(chips = (100 - product) / na_sum(c_chips, c_residues, c_shrinkage) * c_chips,
         residues = (100 - product) / na_sum(c_chips, c_residues, c_shrinkage) * c_residues,
         shrinkage = (100 - product) / na_sum(c_chips, c_residues, c_shrinkage) * c_shrinkage)]

sup_sawnwood[!is.na(product) & is.na(na_sum(chips, residues)), 
             `:=`(chips = (100 - product - shrinkage) / na_sum(c_chips, c_residues) * c_chips,
                  residues = (100 - product - shrinkage) / na_sum(c_chips, c_residues) * c_residues)]

sup_sawnwood[is.na(product), 
    `:=`(product = c_product, chips = c_chips, residues = c_residues, shrinkage = c_shrinkage)]

sup_sawnwood[, `:=`(c_product = NULL, c_chips = NULL, c_residues = NULL, c_shrinkage = NULL)]

# Apply world average MB where no continental average available
sup_wrld <- sup_cont %>% 
  group_by(proc_code) %>% 
  summarize(w_product = mean(product, na.rm = TRUE),
            w_chips = mean(chips, na.rm = TRUE),
            w_residues = mean(residues, na.rm = TRUE),
            w_shrinkage = mean(shrinkage, na.rm = TRUE)) %>% 
  ungroup()
sup_sawnwood <- merge(sup_sawnwood, sup_wrld,
  by = c("proc_code"), all.x = TRUE)
sup_sawnwood[is.na(product), 
    `:=`(product = w_product, chips = w_chips, residues = w_residues, shrinkage = w_shrinkage)]
sup_sawnwood[, `:=`(w_product = NULL, w_chips = NULL, w_residues = NULL, w_shrinkage = NULL)]

# Separate shrinkage which will be used later for cf_carbon
sup_sawnwood[is.na(shrinkage), shrinkage := 100 - product - chips - residues]

sup_sawnwood[shrinkage < 0, shrinkage := 0]

# Rescale final national values to sum up to 100
sup_sawnwood[na_sum(product, chips, residues, shrinkage) != 100,
             `:=`(chips = chips/na_sum(product, chips, residues, shrinkage)*100,  
                  product = product/na_sum(product, chips, residues, shrinkage)*100,
                  residues = residues/na_sum(product, chips, residues, shrinkage)*100,
                  shrinkage = shrinkage/na_sum(product, chips, residues, shrinkage)*100)]

shrinkage_sawnwood <- sup_sawnwood[, c("area_code", "area", "proc_code","process","shrinkage")]

# Convert into percentages ignoring shrinkage losses
sup_sawnwood[, total := product + chips + residues]
sup_sawnwood[, `:=`(product = product / total,
           chips = chips / total,
           residues = residues / total,
           total = NULL, shrinkage = NULL)]
sup_sawnwood <- sup_sawnwood[!is.na(area_code)]
rm(sup_cont, sup_wrld)


cat("\nStep 2: Tidying material balances of veneer (p06) and plywood (p07) production.\n")

# Read files
sup_venply <- fread("inst/mb_veneer,plywood_raw.csv")
share_cnc <- fread("inst/share_cnc.csv")

share_venply <- share_cnc[com_code %in% c("c06","c07")]
setnames(share_venply,c("com_code","item"),c("proc_code","process"))
share_venply[proc_code == "c06", proc_code := "p06"]
share_venply[proc_code == "c07", proc_code := "p07"]

# Rename items 
setnames(sup_venply,c("chips,peeler_cores","sanding_dust"),c("chips","residues"))

# Prepare structure to calculate weighted average of MBs
sup_venply_average <- unique(sup_venply[, .(area_code, area, proc_code, process)])
sup_venply_average[, `:=` (product = NA, chips = NA, residues = NA, shrinkage = NA)]
sup_venply_average <- merge(sup_venply_average,
                     share_venply[, .(area_code, area, proc_code, c_share, nc_share)],
                     by = c("area_code", "area", "proc_code"),all.x = TRUE)

sup_c <- sup_venply[subproc_code %in% c("p06_c","p07_c")]
sup_c <- sup_c %>%
  rename(product_c = product, chips_c = chips, residues_c = residues, shrinkage_c = shrinkage)

sup_nc <- sup_venply[subproc_code %in% c("p06_nc","p07_nc")]
sup_nc <- sup_nc %>%
  rename(product_nc = product, chips_nc = chips, residues_nc = residues, shrinkage_nc = shrinkage)

sup_venply_average <- merge(sup_venply_average,
                            sup_c[, .(area_code, area, proc_code, process, product_c, chips_c, residues_c, shrinkage_c)],
                            by = c("area_code", "area", "proc_code", "process"),
                            all.x = TRUE)
sup_venply_average <- merge(sup_venply_average,
                            sup_nc[, .(area_code, area, proc_code, process, product_nc, chips_nc, residues_nc, shrinkage_nc)],
                            by = c("area_code", "area", "proc_code", "process"),
                            all.x = TRUE)

# Calculate weighted averages
sup_venply <- sup_venply_average[, `:=`(product = (c_share * product_c + nc_share * product_nc) / 100,
                          chips = (c_share * chips_c + nc_share * chips_nc) / 100,
                          residues = (c_share * residues_c + nc_share * residues_nc) / 100,
                          shrinkage = (c_share * shrinkage_c + nc_share * shrinkage_nc) / 100)]
# Calculate averages where no weights available
sup_venply[is.na(product), 
             `:=`(product = (product_c + product_nc)/2, chips = (chips_c + chips_nc)/2,
                  residues = (residues_c + residues_nc)/2, shrinkage = (shrinkage_c + shrinkage_nc)/2)]
# Use c/nc values where only c/nc products reported
sup_venply[is.na(product) & is.na(product_c), 
           `:=`(product = product_nc, chips = chips_nc, residues = residues_nc, shrinkage = shrinkage_nc)]
sup_venply[is.na(product) & is.na(product_nc), 
           `:=`(product = product_c, chips = chips_c, residues = residues_c, shrinkage = shrinkage_c)]
sup_venply[, `:=`(c_share = NULL, product_c = NULL, nc_share = NULL, product_nc = NULL, chips_c = NULL, chips_nc = NULL,
                  residues_c = NULL, residues_nc = NULL, shrinkage_c = NULL, shrinkage_nc = NULL)]
sup_venply <- sup_venply[order(sup_venply$area_code),]

rm(share_venply, sup_venply_average, sup_c, sup_nc)

# Recalculate continental average (due to new weighted countries' averages)
sup_venply <- merge(sup_venply, regions[, .(area_code, continent)],
                    by = "area_code",
                    all.x = TRUE)
sup_venply[continent == "EU", continent := "EUR"]

sup_cont <- sup_venply %>%
  group_by(continent, proc_code, process) %>%
  summarize(product = mean(product, na.rm = TRUE),
            chips = mean(chips, na.rm = TRUE),
            residues = mean(residues, na.rm = TRUE),
            shrinkage = mean(shrinkage, na.rm = TRUE)) %>%
  ungroup()
setnames(sup_cont,c("continent"),c("area"))
setDT(sup_cont)
sup_cont <- sup_cont[! area %in% c('XXX', 'ROW') & !is.na(area)]
sup_venply <- sup_venply[!is.na(area_code)]
sup_venply <- rbind(sup_venply, sup_cont, fill=TRUE)

# Apply continental average MB where no country-specific value available
sup_venply <- merge(sup_venply, sup_cont[, .(continent = area, proc_code, c_product = product, 
                                             c_chips = chips, c_residues = residues, c_shrinkage = shrinkage)],
                      by = c("continent", "proc_code"), all.x = TRUE)

# Fill gaps with continental values
sup_venply[!is.na(product) & is.na(na_sum(chips, residues, shrinkage)), 
             `:=`(chips = (100 - product) / na_sum(c_chips, c_residues, c_shrinkage) * c_chips,
                  residues = (100 - product) / na_sum(c_chips, c_residues, c_shrinkage) * c_residues,
                  shrinkage = (100 - product) / na_sum(c_chips, c_residues, c_shrinkage) * c_shrinkage)]

sup_venply[is.na(product), 
             `:=`(product = c_product, chips = c_chips, residues = c_residues, shrinkage = c_shrinkage)]

sup_venply[, `:=`(c_product = NULL, c_chips = NULL, c_residues = NULL, c_shrinkage = NULL)]

# Apply world average MB where no continental average available
sup_wrld <- sup_cont %>% 
  group_by(proc_code) %>% 
  summarize(w_product = mean(product, na.rm = TRUE),
            w_chips = mean(chips, na.rm = TRUE),
            w_residues = mean(residues, na.rm = TRUE),
            w_shrinkage = mean(shrinkage, na.rm = TRUE)) %>% 
  ungroup()
sup_venply <- merge(sup_venply, sup_wrld,
                      by = c("proc_code"), all.x = TRUE)
sup_venply[is.na(product), 
             `:=`(product = w_product, chips = w_chips, residues = w_residues, shrinkage = w_shrinkage)]
sup_venply[, `:=`(w_product = NULL, w_chips = NULL, w_residues = NULL, w_shrinkage = NULL)]

# Write shrinkage csv
shrinkage_venply <- sup_venply[, c("area_code", "area", "proc_code","process","shrinkage")]
shrinkage <- rbind(shrinkage_sawnwood, shrinkage_venply)
fwrite(shrinkage, "inst/shrinkage_tidy.csv")

# Convert into percentages ignoring losses
sup_venply[, total := product + chips + residues]
sup_venply[, `:=`(product = product / total,
           chips = chips / total,
           residues = residues / total,
           total = NULL, shrinkage = NULL)]

# Save mb_sup_tidy
sup_venply <- sup_venply[!is.na(area_code)]
sup <- rbind(sup_sawnwood, sup_venply)
fwrite(sup, "inst/mb_sup_tidy.csv")

rm(shrinkage_venply, sup_cont, sup_wrld, sup_sawnwood, sup_venply, sup, shrinkage, shrinkage_sawnwood)


# CF USE --------------------------------------------------------------------------------------------------------------------

cat("\nTidying forestry product conversion factors (CFs) for use tables calculation.\n")

# Read main file and select specific cf
cf_raw <- fread("inst/cf_raw.csv")
cf_use_prep <- cf_raw[cf_type == "cf_use"]


cat("\nStep 1: Build weighted averages for c08 fibreboard.\n")

# Read file
share_fb <- fread("inst/share_fb.csv")

# Prepare structure to calculate weighted average of CFs
cf_average <- unique(cf_use_prep[com_code == "c08",
                               .(area_code, area, com_code, item, unit, cf_type)])
cf_average[, `:=` (cf = NA)]
cf_average <- cf_average[, `:=`(cf = as.numeric(cf))]
cf_average <- merge(cf_average,
                     share_fb[, .(area_code, area, com_code, item, c08_1_share, c08_2_share, c08_3_share)],
                     by = c("area_code", "area", "com_code", "item"),all.x = TRUE)

cf_1 <- cf_use_prep[subcom_code == "c08_1"]
cf_1 <- cf_1 %>%
  rename(cf_1 = cf)
cf_2 <- cf_use_prep[subcom_code == "c08_2"]
cf_2 <- cf_2 %>%
  rename(cf_2 = cf)
cf_3 <- cf_use_prep[subcom_code == "c08_3"]
cf_3 <- cf_3 %>%
  rename(cf_3 = cf)

cf_average <- merge(cf_average,
                     cf_1[, .(area_code, area, com_code, item, unit, cf_type, cf_1)],
                     by = c("area_code", "area", "com_code", "item", "unit", "cf_type"),
                     all.x = TRUE)
cf_average <- merge(cf_average,
                     cf_2[, .(area_code, area, com_code, item, unit, cf_type, cf_2)],
                     by = c("area_code", "area", "com_code", "item", "unit", "cf_type"),
                     all.x = TRUE)
cf_average <- merge(cf_average,
                     cf_3[, .(area_code, area, com_code, item, unit, cf_type, cf_3)],
                     by = c("area_code", "area", "com_code", "item", "unit", "cf_type"),
                     all.x = TRUE)

# Calculate weighted averages
cf_average[, `:=`(cf = (c08_1_share * cf_1 + c08_2_share * cf_2 + c08_3_share * cf_3)
                   / (c08_1_share + c08_2_share + c08_3_share))]
# Calculate averages where no weights available
cf_average[is.na(c08_1_share) & is.na(c08_2_share) & is.na(c08_3_share) & 
              !is.na(cf_1) & !is.na(cf_2) & !is.na(cf_3),
            `:=`(cf = (cf_1 + cf_2 + cf_3)/3)]
cf_average[is.na(c08_1_share) & is.na(c08_2_share) & is.na(c08_3_share) &
             !is.na(cf_1) & !is.na(cf_2),
           `:=`(cf = (cf_1 + cf_2)/2)]
# Calculate averages where only some weights/CFs available 
cf_average[is.na (cf) & !is.na(c08_1_share) & !is.na(c08_2_share) & !is.na(c08_3_share) & is.na(cf_1),
            `:=`(cf = (c08_2_share * cf_2 + c08_3_share * cf_3)/(c08_2_share + c08_3_share))]
cf_average[is.na (cf) & !is.na(c08_1_share) & !is.na(c08_2_share) & !is.na(c08_3_share) & is.na(cf_2),
            `:=`(cf = (c08_1_share * cf_1 + c08_3_share * cf_3)/(c08_1_share + c08_3_share))]
cf_average[is.na (cf) & !is.na(c08_1_share) & !is.na(c08_2_share) & !is.na(c08_3_share) & is.na(cf_3),
            `:=`(cf = (c08_1_share * cf_1 + c08_2_share * cf_2)/(c08_1_share + c08_2_share))]
# Use c08_1/2/3 values where only c08_1/2/3 products reported
cf_average[is.na(cf) & is.na(cf_2) & is.na(cf_3), `:=`(cf = cf_1)]
cf_average[is.na(cf) & is.na(cf_1) & is.na(cf_3), `:=`(cf = cf_2)]
cf_average[is.na(cf) & is.na(cf_1) & is.na(cf_2), `:=`(cf = cf_3)]

cf_average[, `:=`(c08_1_share = NULL, c08_2_share = NULL, c08_3_share = NULL, cf_1 = NULL, cf_2 = NULL, cf_3 = NULL)]

rm(share_fb, cf_1, cf_2, cf_3)

# Recalculate CF continental averages (due to new weighted countries' averages)
cf_average <- merge(cf_average, regions[, .(area_code, continent)],
                     by = "area_code",
                     all.x = TRUE)
cf_average[continent == "EU", continent := "EUR"]
cf_cont <- cf_average %>%
  group_by(continent, com_code, item, cf_type, unit) %>%
  summarize(cf = mean(cf, na.rm = TRUE)) %>%
  ungroup()
setnames(cf_cont,c("continent"),c("area"))
setDT(cf_cont)
cf_cont <- cf_cont[! area %in% c('XXX', 'ROW') & !is.na(area)]
cf_average <- cf_average[!is.na(area_code)]
cf_average <- rbind(cf_average, cf_cont, fill=TRUE)

# Apply continental average CF where no country-specific value available
cf_average <- merge(cf_average, cf_cont[, .(continent = area, com_code, cf_type, c_cf = cf)],
                     by = c("continent", "com_code", "cf_type"), all.x = TRUE)
cf_average[is.na(cf), `:=`(cf = c_cf)]
cf_average[, `:=`(c_cf = NULL)]

# Apply world average CF where no continental average available
cf_wrld <- cf_cont %>% 
  group_by(com_code, cf_type) %>% 
  summarize(w_cf = mean(cf, na.rm = TRUE)) %>% 
  ungroup()
cf_average <- merge(cf_average, cf_wrld,
                     by = c("com_code", "cf_type"), all.x = TRUE)
cf_average[is.na(cf), `:=`(cf = w_cf)]
cf_average[, `:=`(w_cf = NULL, continent = NULL)]
rm(cf_cont, cf_wrld)

# Update cf_use_prep
cf_average <- cf_average[, `:=`(description = NA, literature = NA, subcom_code = NA, subitem = NA, source = NA, source_code = NA)]
cf_use_prep <- cf_use_prep[com_code !="c08"]
cf_use_prep <- rbind(cf_use_prep, cf_average)

rm(cf_average)


cat("\nStep 2: Build average CF for c11b mechanical and semi-chemical wood pulp.\n")

# Prepare structure to calculate average of CFs (not weighted because irrelevant difference)
cf_average <- unique(cf_use_prep[com_code == "c11b",
                               .(area_code, area, com_code, item, unit, cf_type)])
cf_average[, `:=` (cf = NA)]
cf_average <- cf_average[, `:=`(cf = as.numeric(cf))]

cf_1 <- cf_use_prep[subcom_code == "c11b_1"]
cf_1 <- cf_1 %>%
  rename(cf_1 = cf)
cf_2 <- cf_use_prep[subcom_code == "c11b_2"]
cf_2 <- cf_2 %>%
  rename(cf_2 = cf)

cf_average <- merge(cf_average,
                     cf_1[, .(area_code, area, com_code, item, unit, cf_type, cf_1)],
                     by = c("area_code", "area", "com_code", "item", "unit", "cf_type"),
                     all.x = TRUE)
cf_average <- merge(cf_average,
                     cf_2[, .(area_code, area, com_code, item, unit, cf_type, cf_2)],
                     by = c("area_code", "area", "com_code", "item", "unit", "cf_type"),
                     all.x = TRUE)
rm(cf_1, cf_2)

# Calculate average
cf_average[!is.na(cf_1) & !is.na(cf_2), `:=`(cf = na_sum(cf_1, cf_2) / 2)]
# Use c11b_1/2 values where only c11b_1/2 products reported
cf_average[!is.na(cf_1) & is.na(cf_2), `:=`(cf = cf_1)]
cf_average[, `:=`(cf_1 = NULL, cf_2 = NULL)]

# Recalculate CF continental averages (due to new countries' averages)
cf_average <- merge(cf_average, regions[, .(area_code, continent)],
                     by = "area_code",
                     all.x = TRUE)
cf_average[continent == "EU", continent := "EUR"]

cf_cont <- cf_average %>%
  group_by(continent, com_code, item, cf_type, unit) %>%
  summarize(cf = mean(cf, na.rm = TRUE)) %>%
  ungroup()
setnames(cf_cont,c("continent"),c("area"))
setDT(cf_cont)
cf_cont <- cf_cont[! area %in% c('XXX','ROW') & !is.na(area)]
cf_average <- cf_average[!is.na(area_code)]
cf_average <- rbind(cf_average, cf_cont, fill=TRUE)

# Apply continental average CF where no country-specific value available
cf_average <- merge(cf_average, cf_cont[, .(continent = area, com_code, cf_type, c_cf = cf)],
                     by = c("continent", "com_code", "cf_type"), all.x = TRUE)
cf_average[is.na(cf), `:=`(cf = c_cf)]
cf_average[, `:=`(c_cf = NULL)]

# Apply world average cf where no continental average available
cf_wrld <- cf_cont %>% 
  group_by(com_code, cf_type) %>% 
  summarize(w_cf = mean(cf, na.rm = TRUE)) %>% 
  ungroup()
cf_average <- merge(cf_average, cf_wrld,
                     by = c("com_code", "cf_type"), all.x = TRUE)
cf_average[is.na(cf), `:=`(cf = w_cf)]
cf_average[, `:=`(w_cf = NULL, continent = NULL)]
rm(cf_cont, cf_wrld)

# Update cf_use_prep
cf_average <- cf_average[, `:=`(description = NA, literature = NA, subcom_code = NA, subitem = NA, 
                                  source = NA, source_code = NA)]
cf_use_prep <-cf_use_prep[com_code !="c11b"]
cf_use_prep <- rbind(cf_use_prep, cf_average)

rm(cf_average)


cat("\nStep 3: Build averages for c14 Paper and paperboard.\n") 

# Read file
share_pp <- fread("inst/share_pp.csv")

# Build averages for c14_2 and c14_3 each
# Prepare structure and build averages for c14_2 (cf_2)
cf_2a <- cf_use_prep[subcom_code == "c14_2a"]
cf_2a <- cf_2a %>%
  rename(cf_2a = cf)
cf_2b <- cf_use_prep[subcom_code == "c14_2b"]
cf_2b <- cf_2b %>%
  rename(cf_2b = cf)

cf_2 <- merge(cf_2a[, .(area_code, area, com_code, item, unit, cf_type, cf_2a)],
                     cf_2b[, .(area_code, area, com_code, item, unit, cf_type, cf_2b)],
                     by = c("area_code", "area", "com_code", "item", "unit", "cf_type"),
                     all.x = TRUE)
cf_2[, `:=` (cf_2 = NA)]
cf_2 <- cf_2[, `:=`(cf_2 = as.numeric(cf_2))]
cf_2[, `:=` (subcom_code = "c14_2", subitem = "Printing and writing papers")]
rm(cf_2a, cf_2b)

# Calculate cf average for c14_2
cf_2[!is.na(cf_2a) & !is.na(cf_2b), `:=`(cf_2 = na_sum(cf_2a, cf_2b) / 2)]
cf_2[!is.na(cf_2a) & is.na(cf_2b), `:=`(cf_2 = cf_2a)]
cf_2[!is.na(cf_2b) & is.na(cf_2a), `:=`(cf_2 = cf_2b)]
cf_2[, `:=`(cf_2a = NULL, cf_2b = NULL)]

# Build averages of cf_3
# Prepare structure and build averages for c14_3 (cf_3)
cf_3a <- cf_use_prep[subcom_code == "c14_3a"]
cf_3a <- cf_3a %>%
  rename(cf_3a = cf)
cf_3b <- cf_use_prep[subcom_code == "c14_3b"]
cf_3b <- cf_3b %>%
  rename(cf_3b = cf)
cf_3c <- cf_use_prep[subcom_code == "c14_3c"]
cf_3c <- cf_3c %>%
  rename(cf_3c = cf)
cf_3d <- cf_use_prep[subcom_code == "c14_3d"]
cf_3d <- cf_3d %>%
  rename(cf_3d = cf)
cf_3e <- cf_use_prep[subcom_code == "c14_3e"]
cf_3e <- cf_3e %>%
  rename(cf_3e = cf)
cf_3f <- cf_use_prep[subcom_code == "c14_3f"]
cf_3f <- cf_3f %>%
  rename(cf_3f = cf)
cf_3g <- cf_use_prep[subcom_code == "c14_3g"]
cf_3g <- cf_3g %>%
  rename(cf_3g = cf)

cf_3 <- merge(cf_3a[, .(area_code, area, com_code, item, unit, cf_type, cf_3a)],
               cf_3b[, .(area_code, area, com_code, item, unit, cf_type, cf_3b)],
               by = c("area_code", "area", "com_code", "item", "unit", "cf_type"),
               all.x = TRUE)
cf_3 <- merge(cf_3,
               cf_3c[, .(area_code, area, com_code, item, unit, cf_type, cf_3c)],
               by = c("area_code", "area", "com_code", "item", "unit", "cf_type"),
               all.x = TRUE)
cf_3 <- merge(cf_3,
               cf_3d[, .(area_code, area, com_code, item, unit, cf_type, cf_3d)],
               by = c("area_code", "area", "com_code", "item", "unit", "cf_type"),
               all.x = TRUE)
cf_3 <- merge(cf_3,
               cf_3e[, .(area_code, area, com_code, item, unit, cf_type, cf_3e)],
               by = c("area_code", "area", "com_code", "item", "unit", "cf_type"),
               all.x = TRUE)
cf_3 <- merge(cf_3,
               cf_3f[, .(area_code, area, com_code, item, unit, cf_type, cf_3f)],
               by = c("area_code", "area", "com_code", "item", "unit", "cf_type"),
               all.x = TRUE)
cf_3 <- merge(cf_3,
               cf_3g[, .(area_code, area, com_code, item, unit, cf_type, cf_3g)],
               by = c("area_code", "area", "com_code", "item", "unit", "cf_type"),
               all.x = TRUE)

cf_3[, `:=` (cf_3 = NA)]
cf_3 <- cf_3[, `:=`(cf_3 = as.numeric(cf_3))]
cf_3[, `:=` (subcom_code = "c14_3", subitem = "Other paper and paperboard")]
rm(cf_3a, cf_3b, cf_3c, cf_3d, cf_3e, cf_3f, cf_3g)

# Calculate cf average for c14_3
cf_3[, `:=` (cf_3 = (cf_3a + cf_3b + cf_3c + cf_3d + cf_3e + cf_3f + cf_3g)/ 7)]
cf_3[is.na(cf_3b), `:=` (cf_3 = (cf_3a + cf_3c + cf_3d + cf_3e + cf_3f + cf_3g)/ 6)]
cf_3[is.na (cf_3) & !is.na(cf_3a), `:=`(cf_3 = cf_3a)]

cf_3[, `:=`(cf_3a = NULL, cf_3b = NULL, cf_3c = NULL, cf_3d = NULL, cf_3e = NULL, cf_3f = NULL, cf_3g = NULL)]

# Prepare structure to calculate weighted average of CFs for c14
cf_average <- unique(cf_use_prep[com_code == "c14",
                               .(area_code, area, com_code, item, unit, cf_type)])
cf_average[, `:=` (cf = NA)]
cf_average <- cf_average[, `:=`(cf = as.numeric(cf))]

cf_average <- merge(cf_average,
                     share_pp[, .(area_code, area, com_code, item, c14_1_share, c14_2_share, c14_3_share)],
                     by = c("area_code", "area", "com_code", "item"),all.x = TRUE)

cf_1 <- cf_use_prep[subcom_code == "c14_1"]
cf_1 <- cf_1 %>%
  rename(cf_1 = cf)

cf_average <- merge(cf_average,
                     cf_1[, .(area_code, area, com_code, item, unit, cf_type, cf_1)],
                     by = c("area_code", "area", "com_code", "item", "unit", "cf_type"),
                     all.x = TRUE)
cf_average <- merge(cf_average,
                     cf_2[, .(area_code, area, com_code, item, unit, cf_type, cf_2)],
                     by = c("area_code", "area", "com_code", "item", "unit", "cf_type"),
                     all.x = TRUE)
cf_average <- merge(cf_average,
                     cf_3[, .(area_code, area, com_code, item, unit, cf_type, cf_3)],
                     by = c("area_code", "area", "com_code", "item", "unit", "cf_type"),
                     all.x = TRUE)

# Calculate weighted averages
cf_average[, `:=`(cf = (c14_1_share * cf_1 + c14_2_share * cf_2 + c14_3_share * cf_3)
                   / (c14_1_share + c14_2_share + c14_3_share))]
cf_average[is.na(cf_1) & is.na(cf), `:=`(cf = (c14_2_share * cf_2 + c14_3_share * cf_3)
                   / (c14_2_share + c14_3_share))]
# Calculate averages where no weights available
cf_average[is.na(c14_1_share) & is.na(c14_2_share) & is.na(c14_3_share) & 
              !is.na(cf_1) & !is.na(cf_2) & !is.na(cf_3),
            `:=`(cf = (cf_1 + cf_2 + cf_3)/3)]
cf_average[is.na(c14_1_share) & is.na(c14_2_share) & is.na(c14_3_share) &
              !is.na(cf_1) & !is.na(cf_2),
            `:=`(cf = (cf_1 + cf_2)/2)]
cf_average[is.na(c14_1_share) & is.na(c14_2_share) & is.na(c14_3_share) &
              !is.na(cf_2) & !is.na(cf_3),
            `:=`(cf = (cf_2 + cf_3)/2)]
cf_average[is.na(cf) & is.na(cf_2) & is.na(cf_3), `:=`(cf = cf_1)]

cf_average[, `:=`(c14_1_share = NULL, c14_2_share = NULL, c14_3_share = NULL, cf_1 = NULL, cf_2 = NULL, cf_3 = NULL)]
rm(share_pp, cf_1, cf_2, cf_3)

# Recalculate CF continental averages (due to new weighted countries' averages)
cf_average <- merge(cf_average, regions[, .(area_code, continent)],
                     by = "area_code",
                     all.x = TRUE)
cf_average[continent == "EU", continent := "EUR"]
cf_cont <- cf_average %>%
  group_by(continent, com_code, item, cf_type, unit) %>%
  summarize(cf = mean(cf, na.rm = TRUE)) %>%
  ungroup()
setnames(cf_cont,c("continent"),c("area"))
setDT(cf_cont)
cf_cont <- cf_cont[! area %in% c('XXX', 'ROW') & !is.na(area)]
cf_average <- cf_average[!is.na(area_code)]
cf_average <- rbind(cf_average, cf_cont, fill=TRUE)

# Apply continental average CF where no country-specific value available
cf_average <- merge(cf_average, cf_cont[, .(continent = area, com_code, cf_type, c_cf = cf)],
                     by = c("continent", "com_code", "cf_type"), all.x = TRUE)
cf_average[is.na(cf), `:=`(cf = c_cf)]
cf_average[, `:=`(c_cf = NULL)]

# Apply world average cf where no continental average available
cf_wrld <- cf_cont %>% 
  group_by(com_code, cf_type) %>% 
  summarize(w_cf = mean(cf, na.rm = TRUE)) %>% 
  ungroup()
cf_average <- merge(cf_average, cf_wrld,
                     by = c("com_code", "cf_type"), all.x = TRUE)
cf_average[is.na(cf), `:=`(cf = w_cf)]
cf_average[, `:=`(w_cf = NULL, continent = NULL)]
rm(cf_cont, cf_wrld)

# Update cf_use_prep
cf_average <- cf_average[, `:=`(description = NA, literature = NA, subcom_code = NA, subitem = NA, source = NA, source_code = NA)]
cf_use_prep <- cf_use_prep[com_code !="c14"]
cf_use_prep <- rbind(cf_use_prep, cf_average)

rm(cf_average)


cat("\nStep 4: Tidy the rest of cf_use_prep table.\n")

# Exclude items which have already been tidied
cf_use_NA <- cf_use_prep[com_code %nin% c("c08", "c11b", "c14")]

# Calculate continental averages CF 
cf_use_NA <- merge(cf_use_NA, regions[, .(area_code, continent)],
                     by = "area_code",
                     all.x = TRUE)
cf_use_NA[continent == "EU", continent := "EUR"]

cf_cont <- cf_use_NA %>%
  group_by(continent, com_code, item, cf_type, subcom_code, unit) %>%
  summarize(cf = mean(cf, na.rm = TRUE)) %>%
  ungroup()
setnames(cf_cont,c("continent"),c("area"))
setDT(cf_cont)
cf_cont <- cf_cont[! area %in% c('XXX','ROW') & !is.na(area)]

# Apply continental average CF  where no country-specific value available
cf_use_NA <- merge(cf_use_NA, cf_cont[, .(continent = area, com_code, cf_type, c_cf = cf, subcom_code)],
                     by = c("continent", "com_code", "cf_type", "subcom_code"), all.x = TRUE)
cf_use_NA[is.na(cf), `:=`(cf = c_cf)]
cf_use_NA[, `:=`(c_cf = NULL)]

# Apply world average CF where no continental average available
cf_wrld <- cf_cont %>% 
  group_by(com_code, cf_type, subcom_code) %>% 
  summarize(w_cf = mean(cf, na.rm = TRUE)) %>% 
  ungroup()
cf_use_NA <- merge(cf_use_NA, cf_wrld,
                     by = c("com_code", "cf_type", "subcom_code"), all.x = TRUE)
cf_use_NA[is.na(cf), `:=`(cf = w_cf)]
cf_use_NA[, `:=`(w_cf = NULL, continent = NULL)]
rm(cf_cont, cf_wrld)

# Update cf_use_prep
cf_use_prep <- cf_use_prep[com_code %in% c("c08", "c11b", "c14")]
cf_use_prep <- rbind(cf_use_prep, cf_use_NA)
rm(cf_use_NA)


cat("\nStep 5: Last final adjustments and write cf_use_tidy .\n")

# Give units consistent names
cf_use_prep <- cf_use_prep[com_code %in% c("c17", "c18"), `:=`(unit = "m3p/m3sw")]
cf_use_prep <- cf_use_prep[com_code == "c13", `:=`(unit = "tonne/tonne")]


# Adding missing commodities
# Create CF for c10 osb as output of c01 industrial roundwood coniferous and c02 industrial roundwood non-coniferous
cf_use_prep[com_code=="c10", `:=`(source = products$item[products$com_code=="c01"], source_code = "c01")]
cf_osb <- cf_use_prep[com_code=="c10"]
cf_osb[, `:=`(source = products$item[products$com_code=="c02"], source_code = "c02")]
cf_use_prep <- rbind(cf_use_prep, cf_osb)

# Create CF for items where CF equals 1
cf_missing <- unique(cf_raw[com_code %in% c("c01", "c02", "c03"),
                              .(area_code, area, com_code, item)])
cf_missing[, `:=` (unit = "m3rw/m3p", cf = "1", cf_type = "cf_use")]

# Create CFs for c19 from assumption CF is 2.5 m3sw/tonne
cf_postconwood <- unique(cf_raw[com_code == "c14",
                                 .(area_code, area, com_code, item, unit, cf_type)])
cf_postconwood[, `:=` (cf = 2.5)]
cf_postconwood <- cf_postconwood[, `:=`(cf = as.numeric(cf))]
cf_postconwood[, `:=` (com_code = "c19", item = products$item[products$com_code == "c19"])]
cf_postconwood <- cf_postconwood[, `:=` (literature = "Own assumption")]

# Write final cf_use_tidy as csv
cf_use <- rbind(cf_use_prep, cf_osb, cf_missing, cf_postconwood, fill = TRUE)
cf_use <- cf_use[, `:=`(subitem = NULL, subcom_code = NULL)]
cf_use <- cf_use[is.na(literature), `:=` (literature = "Own calculation")]
cf_use <- cf_use[order(cf_use$com_code, cf_use$area_code),]
cf_use <- cf_use[!is.na(area_code)]

fwrite(cf_use, "inst/cf_use_tidy.csv")

rm(cf_missing, cf_osb, cf_postconwood, cf_use_prep)


# CF CARBON -------------------------------------------------------------------------------------------------------------

cat("\nBuild CF carbon based on cf_pdensity, cf_wdensity, cf_shipping_weight and other values.\n")

# Read main file
cf_raw <- fread("inst/cf_raw.csv")
cf_carbon_prep <- cf_raw[cf_type != "cf_use"]


cat("\nStep 1: Aggregate (sub)items to build weighted averages for c03 wood fuel, c06 veneer sheets and c07 plywood.\n")

# Read file
share_cnc <- fread("inst/share_cnc.csv")

# Prepare structure to calculate weighted average of CFs
cf_average <- unique(cf_carbon_prep[com_code %in% c("c03", "c06", "c07"),
                              .(area_code, area, com_code, item, unit, cf_type)])
cf_average[, `:=` (cf = NA)]

cf_average <- merge(cf_average,
                     share_cnc[, .(area_code, area, com_code, item, c_share, nc_share)],
                     by = c("area_code", "area", "com_code", "item"),all.x = TRUE)

cf_c <- cf_carbon_prep[subcom_code %in% c("c03_c","c06_c","c07_c")]
cf_c <- cf_c %>%
  rename(cf_c = cf)
cf_nc <- cf_carbon_prep[subcom_code %in% c("c03_nc","c06_nc","c07_nc")]
cf_nc <- cf_nc %>%
  rename(cf_nc = cf)

cf_average <- merge(cf_average,
                     cf_c[, .(area_code, area, com_code, item, unit, cf_type, cf_c)],
                     by = c("area_code", "area", "com_code", "item", "unit", "cf_type"),
                     all.x = TRUE)
cf_average <- merge(cf_average,
                     cf_nc[, .(area_code, area, com_code, item, unit, cf_type, cf_nc)],
                     by = c("area_code", "area", "com_code", "item", "unit", "cf_type"),
                     all.x = TRUE)

# Calculate weighted averages
cf_average[, `:=`(cf = (c_share * cf_c + nc_share * cf_nc) / 100)]
# Calculate averages where no weights available
cf_average[is.na(c_share) & is.na(nc_share) & !is.na(cf_nc) & !is.na(cf_c),
            `:=`(cf = (cf_nc + cf_c)/2)]
# Use c/nc values where only c/nc products reported
cf_average[is.na(cf_c), `:=`(cf = cf_nc)]
cf_average[is.na(cf_nc), `:=`(cf = cf_c)]

cf_average[, `:=`(c_share = NULL, nc_share = NULL, cf_c = NULL, cf_nc = NULL)]
cf_average <- cf_average[order(cf_average$area_code),]

rm(share_cnc, cf_c, cf_nc)

# Recalculate continental average (due to new weighted countries' averages)
cf_average <- merge(cf_average, regions[, .(area_code, continent)],
                     by = "area_code",
                     all.x = TRUE)
cf_average[continent == "EU", continent := "EUR"]
cf_cont <- cf_average %>%
  group_by(continent, com_code, item, cf_type, unit) %>%
  summarize(cf = mean(cf, na.rm = TRUE)) %>%
  ungroup()
setnames(cf_cont,c("continent"),c("area"))
setDT(cf_cont)
cf_cont <- cf_cont[! area %in% c('XXX', 'ROW') & !is.na(area)]
cf_average <- cf_average[!is.na(area_code)]
cf_average <- rbind(cf_average, cf_cont, fill=TRUE)

# Apply continental average CF where no country-specific value available
cf_average <- merge(cf_average, cf_cont[, .(continent = area, com_code, cf_type, c_cf = cf)],
                     by = c("continent", "com_code", "cf_type"), all.x = TRUE)
cf_average[is.na(cf), `:=`(cf = c_cf)]
cf_average[, `:=`(c_cf = NULL)]

# Apply world average CF where no continental average available
cf_wrld <- cf_cont %>% 
  group_by(com_code, cf_type) %>% 
  summarize(w_cf = mean(cf, na.rm = TRUE)) %>% 
  ungroup()
cf_average <- merge(cf_average, cf_wrld,
                     by = c("com_code", "cf_type"), all.x = TRUE)
cf_average[is.na(cf), `:=`(cf = w_cf)]
cf_average[, `:=`(w_cf = NULL, continent = NULL)]
rm(cf_cont, cf_wrld)

# Update cf_carbon_prep
cf_carbon_prep <-cf_carbon_prep[com_code %nin% c("c03", "c06", "c07"),]
cf_carbon_prep <- rbind(cf_carbon_prep, cf_average, fill = TRUE)

rm(cf_average, cf_raw)


cat("\nStep 2: Build weighted averages for c01 industrial roundwood, coniferous.\n")

# Read file
share_irwc <- fread("inst/share_irwc.csv")

# Prepare structure to calculate weighted average of CFs
cf_average <- unique(cf_carbon_prep[com_code == "c01",
                                   .(area_code, area, com_code, item, unit, cf_type)])
cf_average[, `:=` (cf = NA)]
cf_average <- cf_average[, `:=`(cf = as.numeric(cf))]
cf_average <- merge(cf_average,
                     share_irwc[, .(area_code, area, com_code, item, c01_1_share, c01_2_share)],
                     by = c("area_code", "area", "com_code", "item"),all.x = TRUE)

cf_1 <- cf_carbon_prep[subcom_code == "c01_1"]
cf_1 <- cf_1 %>%
  rename(cf_1 = cf)
cf_2 <- cf_carbon_prep[subcom_code == "c01_2"]
cf_2 <- cf_2 %>%
  rename(cf_2 = cf)

cf_average <- merge(cf_average,
                     cf_1[, .(area_code, area, com_code, item, unit, cf_type, cf_1)],
                     by = c("area_code", "area", "com_code", "item", "unit", "cf_type"),
                     all.x = TRUE)
cf_average <- merge(cf_average,
                     cf_2[, .(area_code, area, com_code, item, unit, cf_type, cf_2)],
                     by = c("area_code", "area", "com_code", "item", "unit", "cf_type"),
                     all.x = TRUE)

# Calculate weighted averages
cf_average[!is.na(cf_1) & !is.na(cf_2)
            , `:=`(cf = ((c01_1_share * cf_1 + c01_2_share * cf_2) / 100))]
# Calculate averages where no weights available
cf_average[is.na(c01_1_share) & is.na(c01_2_share) & !is.na(cf_1) & !is.na(cf_2),
            `:=`(cf = (cf_1 + cf_2)/2)]
cf_average[c01_1_share == "0" & c01_2_share == "0" & !is.na(cf_1) & !is.na(cf_2),
            `:=`(cf = (cf_1 + cf_2)/2)]
# Use c/nc values where only c/nc products reported
cf_average[is.na(cf) & is.na(cf_1), `:=`(cf = cf_2)]
cf_average[is.na(cf) & is.na(cf_2), `:=`(cf = cf_1)]

cf_average[, `:=`(c01_1_share = NULL, c01_2_share = NULL, cf_1 = NULL, cf_2 = NULL)]
cf_average <- cf_average[order(cf_average$area_code),]

rm(share_irwc, cf_1, cf_2)

# Recalculate continental average (due to new weighted countries' averages)
cf_average <- merge(cf_average, regions[, .(area_code, continent)],
                     by = "area_code",
                     all.x = TRUE)
cf_average[continent == "EU", continent := "EUR"]
cf_cont <- cf_average %>%
  group_by(continent, com_code, item, cf_type, unit) %>%
  summarize(cf = mean(cf, na.rm = TRUE)) %>%
  ungroup()
setnames(cf_cont,c("continent"),c("area"))
setDT(cf_cont)
cf_cont <- cf_cont[! area %in% c('XXX', 'ROW') & !is.na(area)]
cf_average <- cf_average[!is.na(area_code)]
cf_average <- rbind(cf_average, cf_cont, fill=TRUE)

# Apply continental average cf where no country-specific value available
cf_average <- merge(cf_average, cf_cont[, .(continent = area, com_code, cf_type, c_cf = cf)],
                     by = c("continent", "com_code", "cf_type"), all.x = TRUE)
cf_average[is.na(cf), `:=`(cf = c_cf)]
cf_average[, `:=`(c_cf = NULL)]

# Apply world average cf where no continental average available
cf_wrld <- cf_cont %>% 
  group_by(com_code, cf_type) %>% 
  summarize(w_cf = mean(cf, na.rm = TRUE)) %>% 
  ungroup()
cf_average <- merge(cf_average, cf_wrld,
                     by = c("com_code", "cf_type"), all.x = TRUE)
cf_average[is.na(cf), `:=`(cf = w_cf)]
cf_average[, `:=`(w_cf = NULL, continent = NULL)]
rm(cf_cont, cf_wrld)

# Update cf_carbon_prep
cf_carbon_prep <-cf_carbon_prep[com_code != "c01",]
cf_carbon_prep <- rbind(cf_carbon_prep, cf_average, fill = TRUE)

rm(cf_average)


cat("\nStep 3: Build weighted averages for c02 industrial roundwood, non-coniferous.\n")

# Read file
share_irwnc <- fread("inst/share_irwnc.csv")

# Prepare structure to calculate weighted average of CFs
cf_average <- unique(cf_carbon_prep[com_code == "c02",
                                   .(area_code, area, com_code, item, unit, cf_type)])
cf_average[, `:=` (cf = NA)]
cf_average <- cf_average[, `:=`(cf = as.numeric(cf))]
cf_average <- merge(cf_average,
                     share_irwnc[, .(area_code, area, com_code, item, c02_1_share, c02_2_share)],
                     by = c("area_code", "area", "com_code", "item"), all.x = TRUE)

cf_1 <- cf_carbon_prep[subcom_code == "c02_1"]
cf_1 <- cf_1 %>%
  rename(cf_1 = cf)
cf_2 <- cf_carbon_prep[subcom_code == "c02_2"]
cf_2 <- cf_2 %>%
  rename(cf_2 = cf)

cf_average <- merge(cf_average,
                     cf_1[, .(area_code, area, com_code, item, unit, cf_type, cf_1)],
                     by = c("area_code", "area", "com_code", "item", "unit", "cf_type"),
                     all.x = TRUE)
cf_average <- merge(cf_average,
                     cf_2[, .(area_code, area, com_code, item, unit, cf_type, cf_2)],
                     by = c("area_code", "area", "com_code", "item", "unit", "cf_type"),
                     all.x = TRUE)

# Calculate weighted averages
cf_average[!is.na(cf_1) & !is.na(cf_2)
            , `:=`(cf = ((c02_1_share * cf_1 + c02_2_share * cf_2) / (c02_1_share + c02_2_share)))]
# Calculate averages where no weights available
cf_average[is.na(c02_1_share) & is.na(c02_2_share) & !is.na(cf_1) & !is.na(cf_2),
            `:=`(cf = (cf_1 + cf_2)/2)]
# Use c/nc values where only c/nc products reported
cf_average[is.na(cf_1), `:=`(cf = cf_2)]
cf_average[is.na(cf_2), `:=`(cf = cf_1)]

cf_average[, `:=`(c02_1_share = NULL, c02_2_share = NULL, cf_1 = NULL, cf_2 = NULL)]
cf_average <- cf_average[order(cf_average$area_code),]

rm(share_irwnc, cf_1, cf_2)

# Recalculate continental average (due to new weighted countries' averages)
cf_average <- merge(cf_average, regions[, .(area_code, continent)],
                     by = "area_code",
                     all.x = TRUE)
cf_average[continent == "EU", continent := "EUR"]
cf_cont <- cf_average %>%
  group_by(continent, com_code, item, cf_type, unit) %>%
  summarize(cf = mean(cf, na.rm = TRUE)) %>%
  ungroup()
setnames(cf_cont,c("continent"),c("area"))
setDT(cf_cont)
cf_cont <- cf_cont[! area %in% c('XXX', 'ROW') & !is.na(area)]
cf_average <- cf_average[!is.na(area_code)]
cf_average <- rbind(cf_average, cf_cont, fill=TRUE)

# Apply continental average cf where no country-specific value available
cf_average <- merge(cf_average, cf_cont[, .(continent = area, com_code, cf_type, c_cf = cf)],
                     by = c("continent", "com_code", "cf_type"), all.x = TRUE)
cf_average[is.na(cf), `:=`(cf = c_cf)]
cf_average[, `:=`(c_cf = NULL)]

# Apply world average cf where no continental average available
cf_wrld <- cf_cont %>% 
  group_by(com_code, cf_type) %>% 
  summarize(w_cf = mean(cf, na.rm = TRUE)) %>% 
  ungroup()
cf_average <- merge(cf_average, cf_wrld,
                     by = c("com_code", "cf_type"), all.x = TRUE)
cf_average[is.na(cf), `:=`(cf = w_cf)]
cf_average[cf == "0", `:=`(cf = w_cf)]
cf_average[, `:=`(w_cf = NULL, continent = NULL)]
rm(cf_cont, cf_wrld)

# Update cf_carbon_prep
cf_carbon_prep <-cf_carbon_prep[com_code != "c02"]
cf_carbon_prep <- rbind(cf_carbon_prep, cf_average, fill = TRUE)
rm(cf_average)


cat("\nStep 4: Build weighted averages for c08 fibreboard.\n")

# Read file
share_fb <- fread("inst/share_fb.csv")

# Prepare structure to calculate weighted average of CFs
cf_average <- unique(cf_carbon_prep[com_code == "c08",
                                   .(area_code, area, com_code, item, unit, cf_type)])
cf_average[, `:=` (cf = NA)]
cf_average <- cf_average[, `:=`(cf = as.numeric(cf))]
cf_average <- merge(cf_average,
                     share_fb[, .(area_code, area, com_code, item, c08_1_share, c08_2_share, c08_3_share)],
                     by = c("area_code", "area", "com_code", "item"),all.x = TRUE)

cf_1 <- cf_carbon_prep[subcom_code == "c08_1"]
cf_1 <- cf_1 %>%
  rename(cf_1 = cf)
cf_2 <- cf_carbon_prep[subcom_code == "c08_2"]
cf_2 <- cf_2 %>%
  rename(cf_2 = cf)
cf_3 <- cf_carbon_prep[subcom_code == "c08_3"]
cf_3 <- cf_3 %>%
  rename(cf_3 = cf)

cf_average <- merge(cf_average,
                     cf_1[, .(area_code, area, com_code, item, unit, cf_type, cf_1)],
                     by = c("area_code", "area", "com_code", "item", "unit", "cf_type"),
                     all.x = TRUE)
cf_average <- merge(cf_average,
                     cf_2[, .(area_code, area, com_code, item, unit, cf_type, cf_2)],
                     by = c("area_code", "area", "com_code", "item", "unit", "cf_type"),
                     all.x = TRUE)
cf_average <- merge(cf_average,
                     cf_3[, .(area_code, area, com_code, item, unit, cf_type, cf_3)],
                     by = c("area_code", "area", "com_code", "item", "unit", "cf_type"),
                     all.x = TRUE)

# Calculate weighted averages
cf_average[, `:=`(cf = (c08_1_share * cf_1 + c08_2_share * cf_2 + c08_3_share * cf_3)
                   / (c08_1_share + c08_2_share + c08_3_share))]
# Calculate averages where no weights available
cf_average[is.na(c08_1_share) & is.na(c08_2_share) & is.na(c08_3_share) & 
              !is.na(cf_1) & !is.na(cf_2) & !is.na(cf_3),
            `:=`(cf = (cf_1 + cf_2 + cf_3)/3)]
cf_average[is.na(c08_1_share) & is.na(c08_2_share) & is.na(c08_3_share) &
              !is.na(cf_1) & !is.na(cf_2),
            `:=`(cf = (cf_1 + cf_2)/2)]
# Calculate averages where only some weights/CFs available 
cf_average[is.na (cf) & !is.na(c08_1_share) & !is.na(c08_2_share) & !is.na(c08_3_share) & is.na(cf_1),
            `:=`(cf = (c08_2_share * cf_2 + c08_3_share * cf_3)/(c08_2_share + c08_3_share))]
cf_average[is.na (cf) & !is.na(c08_1_share) & !is.na(c08_2_share) & !is.na(c08_3_share) & is.na(cf_2),
            `:=`(cf = (c08_1_share * cf_1 + c08_3_share * cf_3)/(c08_1_share + c08_3_share))]
cf_average[is.na (cf) & !is.na(c08_1_share) & !is.na(c08_2_share) & !is.na(c08_3_share) & is.na(cf_3),
            `:=`(cf = (c08_1_share * cf_1 + c08_2_share * cf_2)/(c08_1_share + c08_2_share))]
# Use c08_1/2/3 values where only c08_1/2/3 products reported
cf_average[is.na(cf) & is.na(cf_2) & is.na(cf_3), `:=`(cf = cf_1)]
cf_average[is.na(cf) & is.na(cf_1) & is.na(cf_3), `:=`(cf = cf_2)]
cf_average[is.na(cf) & is.na(cf_1) & is.na(cf_2), `:=`(cf = cf_3)]

cf_average[, `:=`(c08_1_share = NULL, c08_2_share = NULL, c08_3_share = NULL, cf_1 = NULL, cf_2 = NULL, cf_3 = NULL)]
cf_average <- cf_average[order(cf_average$area_code),]

rm(share_fb, cf_1, cf_2, cf_3)

# Recalculate continental averages (due to new weighted countries' averages)
cf_average <- merge(cf_average, regions[, .(area_code, continent)],
                     by = "area_code",
                     all.x = TRUE)
cf_average[continent == "EU", continent := "EUR"]
cf_cont <- cf_average %>%
  group_by(continent, com_code, item, cf_type, unit) %>%
  summarize(cf = mean(cf, na.rm = TRUE)) %>%
  ungroup()
setnames(cf_cont,c("continent"),c("area"))
setDT(cf_cont)
cf_cont <- cf_cont[! area %in% c('XXX', 'ROW') & !is.na(area)]
cf_average <- cf_average[!is.na(area_code)]
cf_average <- rbind(cf_average, cf_cont, fill=TRUE)

# Apply continental average CF where no country-specific value available
cf_average <- merge(cf_average, cf_cont[, .(continent = area, com_code, cf_type, c_cf = cf)],
                     by = c("continent", "com_code", "cf_type"), all.x = TRUE)
cf_average[is.na(cf), `:=`(cf = c_cf)]
cf_average[, `:=`(c_cf = NULL)]

# Apply world average CF where no continental average available
cf_wrld <- cf_cont %>% 
  group_by(com_code, cf_type) %>% 
  summarize(w_cf = mean(cf, na.rm = TRUE)) %>% 
  ungroup()
cf_average <- merge(cf_average, cf_wrld,
                     by = c("com_code", "cf_type"), all.x = TRUE)
cf_average[is.na(cf), `:=`(cf = w_cf)]
cf_average[, `:=`(w_cf = NULL, continent = NULL)]
rm(cf_cont, cf_wrld)

# Update cf_carbon_prep
cf_carbon_prep <- cf_carbon_prep[com_code != "c08"]
cf_carbon_prep <- rbind(cf_carbon_prep, cf_average, fill = TRUE)

rm(cf_average)


cat("\nStep 5: Build  cf_wdensity [odmt/m3p] for c17 wood chips and particles and c18 wood residues.\n")

# Read file
cf_use <- fread("inst/cf_use_tidy.csv")
cf_use_byprod <- cf_use[com_code %in% c("c17", "c18")]
cf_use_byprod <- cf_use_byprod %>%
  rename(cf_use = cf)

# Build averages of CFs type "cf_use_odmt"
cf_odmt <- cf_carbon_prep[com_code %in% c("c17","c18")]

## Calculate continental averages
cf_odmt <- merge(cf_odmt, regions[, .(area_code, continent)],
                   by = "area_code",
                   all.x = TRUE)
cf_odmt[continent == "EU", continent := "EUR"]

cf_cont <- cf_odmt %>%
  group_by(continent, com_code, item, cf_type, subcom_code, unit) %>%
  summarize(cf = mean(cf, na.rm = TRUE)) %>%
  ungroup()
setnames(cf_cont,c("continent"),c("area"))
setDT(cf_cont)
cf_cont <- cf_cont[! area %in% c('XXX','ROW') & !is.na(area)]

## Apply continental average CF where no country-specific value available
cf_odmt <- merge(cf_odmt, cf_cont[, .(continent = area, com_code, cf_type, c_cf = cf, subcom_code)],
                   by = c("continent", "com_code", "cf_type", "subcom_code"), all.x = TRUE)
cf_odmt[is.na(cf), `:=`(cf = c_cf)]
cf_odmt[, `:=`(c_cf = NULL)]

## Apply world average CF where no continental average available
cf_wrld <- cf_cont %>% 
  group_by(com_code, cf_type, subcom_code) %>% 
  summarize(w_cf = mean(cf, na.rm = TRUE)) %>% 
  ungroup()
cf_odmt <- merge(cf_odmt, cf_wrld,
                   by = c("com_code", "cf_type", "subcom_code"), all.x = TRUE)
cf_odmt[is.na(cf), `:=`(cf = w_cf)]
cf_odmt[, `:=`(w_cf = NULL, continent = NULL)]
rm(cf_cont, cf_wrld)

cf_odmt <- cf_odmt %>%
  rename(cf_odmt = cf)

# Prepare structure to calculate CFs
cf_byprod <- unique(cf_carbon_prep[com_code %in% c("c17", "c18"),
                                     .(area_code, area, com_code, item)])
cf_byprod[, `:=` (cf = NA, unit = "odmt/m3p", cf_type = "cf_wdensity")]
cf_byprod <- cf_byprod[, `:=`(cf = as.numeric(cf))]
cf_byprod <- merge(cf_byprod,
                     cf_use_byprod[, .(area_code, area, com_code, item, cf_use, cf_use_unit = unit)],
                     by = c("area_code", "area", "com_code", "item"),all.x = TRUE)
cf_byprod <- merge(cf_byprod,
                    cf_odmt[, .(area_code, area, com_code, item, cf_odmt, cf_odmt_unit = unit)],
                    by = c("area_code", "area", "com_code", "item"),
                    all.x = TRUE)

rm(cf_odmt, cf_use_byprod, cf_use)

# Calculate cf_wdensity
cf_byprod[, `:=`(cf = 1 / cf_odmt / cf_use)]
cf_byprod[, `:=`(cf_use = NULL, cf_use_unit = NULL, cf_odmt = NULL, cf_odmt_unit = NULL)]
cf_byprod <- cf_byprod[!is.na(area_code)]

# Update cf_carbon_prep
cf_carbon_prep <- cf_carbon_prep[com_code %nin% c("c17", "c18")]
cf_carbon_prep <- rbind(cf_carbon_prep, cf_byprod, fill = TRUE)

rm(cf_byprod)


cat("\nStep 6: Tidy the rest of cf_carbon_prep.\n")

cf_carbon_NA <- cf_carbon_prep[com_code %in% c("c04", "c05", "c09", "c10")]

# Calculate continental averages
cf_carbon_NA <- merge(cf_carbon_NA, regions[, .(area_code, continent)],
                   by = "area_code",
                   all.x = TRUE)
cf_carbon_NA[continent == "EU", continent := "EUR"]

cf_cont <- cf_carbon_NA %>%
  group_by(continent, com_code, item, cf_type, subcom_code, unit) %>%
  summarize(cf = mean(cf, na.rm = TRUE)) %>%
  ungroup()
setnames(cf_cont,c("continent"),c("area"))
setDT(cf_cont)
cf_cont <- cf_cont[! area %in% c('XXX','ROW') & !is.na(area)]

# Apply continental average CF where no country-specific value available
cf_carbon_NA <- merge(cf_carbon_NA, cf_cont[, .(continent = area, com_code, cf_type, c_cf = cf, subcom_code)],
                   by = c("continent", "com_code", "cf_type", "subcom_code"), all.x = TRUE)
cf_carbon_NA[is.na(cf), `:=`(cf = c_cf)]
cf_carbon_NA[, `:=`(c_cf = NULL)]

# Apply world average CF where no continental average available
cf_wrld <- cf_cont %>% 
  group_by(com_code, cf_type, subcom_code) %>% 
  summarize(w_cf = mean(cf, na.rm = TRUE)) %>% 
  ungroup()
cf_carbon_NA <- merge(cf_carbon_NA, cf_wrld,
                   by = c("com_code", "cf_type", "subcom_code"), all.x = TRUE)
cf_carbon_NA[is.na(cf), `:=`(cf = w_cf)]
cf_carbon_NA[, `:=`(w_cf = NULL, continent = NULL)]
rm(cf_cont, cf_wrld)

# Update cf_carbon_prep
cf_carbon_prep <- cf_carbon_prep[com_code %nin% c("c04", "c05", "c09", "c10")]
cf_carbon_prep <- rbind(cf_carbon_prep, cf_carbon_NA)
rm(cf_carbon_NA)
cf_carbon_prep <- cf_carbon_prep[order(cf_carbon_prep$com_code, cf_carbon_prep$area_code),]

# Make units consistent
cf_carbon_prep[unit %in% c("kg/m3", "kg/m3sw"), cf := cf / 1000]
cf_carbon_prep[unit == "kg/m3", `:=` (unit = "tonne/m3")]
cf_carbon_prep[unit == "kg/m3sw", `:=` (unit = "tonne/m3sw")]

# Write cf file for converting (bilateral data) items from tonnes to volume to be use in other script
cf_btd <- cf_carbon_prep[com_code %in% c("c03", "c09", "c10") & unit %in% c("tonne/m3", "m3rw/tonne")]

cf_btd <- cf_btd[com_code %in% c("c09", "c10"), `:=` (cf = 1 / cf)]
cf_btd <- cf_btd[com_code %in% c("c09", "c10"), `:=` (unit = "m3/tonne")]
cf_btd <- cf_btd[, `:=`(subitem = NULL, subcom_code = NULL)]
cf_btd <- cf_btd[is.na(literature), `:=` (literature = "Own calculation")]

# Apply factor of 1.5 to c18 wood residues (UNECE and FAO, 2010)
cf_residues <- unique(cf_carbon_prep[com_code == "c18",
                      .(area_code, area, com_code, item)])
cf_residues[, `:=`(cf = 1.5, unit = "m3/tonne", cf_type = "cf_density", description = "volumen to weight",
                   literature = "UNECE and FAO (2010)")]
cf_btd <- rbind(cf_btd, cf_residues, fill = TRUE)
cf_btd <- cf_btd[!is.na(area_code)]

fwrite(cf_btd, "inst/cf_btd_tidy.csv")

rm(cf_residues, cf_btd)


cat("\nStep 7: Tidying wood percentage content in wood-based panels.\n")

# Read file
wbp <- fread("inst/wbp_raw.csv")
wbp[wood == 0, wood := NA]

# Build averages for c08 fiberboard, not weighted because not enough data
wbp_fb <- wbp[com_code == "c08"]

# Prepare structure to calculate average for c08 fiberboard
wbp_fb <- unique(wbp_fb[com_code == "c08",
                                 .(area_code, area, com_code, item)])
wbp_fb[, `:=` (wood = NA)]
wbp_fb <- wbp_fb[, `:=`(wood = as.numeric(wood))]
wood_1 <- wbp[subcom_code == "c08_1"]
wood_1 <- wood_1 %>%
  rename(wood_1 = wood)
wood_2 <- wbp[subcom_code == "c08_2"]
wood_2 <- wood_2 %>%
  rename(wood_2 = wood)
wood_3 <- wbp[subcom_code == "c08_3"]
wood_3 <- wood_3 %>%
  rename(wood_3 = wood)

wbp_fb <- merge(wbp_fb,
                    wood_1[, .(area_code, area, com_code, item, wood_1)],
                    by = c("area_code", "area", "com_code", "item"),
                    all.x = TRUE)
wbp_fb <- merge(wbp_fb,
                    wood_2[, .(area_code, area, com_code, item, wood_2)],
                    by = c("area_code", "area", "com_code", "item"),
                    all.x = TRUE)
wbp_fb <- merge(wbp_fb,
                    wood_3[, .(area_code, area, com_code, item, wood_3)],
                    by = c("area_code", "area", "com_code", "item"),
                    all.x = TRUE)

rm(wood_1, wood_2, wood_3)

# Calculate average
wbp_fb[!is.na(wood_1) & !is.na(wood_2) & !is.na(wood_3), `:=`(wood = na_sum(wood_1, wood_2, wood_3) / 3)]
wbp_fb[is.na(wood) & is.na(wood_3) & !is.na(wood_1) & !is.na(wood_2), `:=`(wood = na_sum(wood_1, wood_2) / 2)]
wbp_fb[is.na(wood) & is.na(wood_3) & is.na(wood_1) & !is.na(wood_2), `:=`(wood = wood_2)]

wbp_fb[, `:=` (wood_1 = NULL, wood_2 = NULL, wood_3 = NULL)]

# Recalculate continental averages (due to new countries' averages)
wbp_fb <- merge(wbp_fb, regions[, .(area_code, continent)],
                    by = "area_code",
                    all.x = TRUE)
wbp_fb[continent == "EU", continent := "EUR"]
wood_cont <- wbp_fb %>%
  group_by(continent, com_code, item) %>%
  summarize(wood = mean(wood, na.rm = TRUE)) %>%
  ungroup()
setnames(wood_cont,c("continent"),c("area"))
setDT(wood_cont)
wood_cont <- wood_cont[! area %in% c('XXX','ROW') & !is.na(area)]
wbp_fb <- wbp_fb[!is.na(area_code)]
wbp_fb <- rbind(wbp_fb, wood_cont, fill=TRUE)

## Apply continental average "wood" where no country-specific value available
wbp_fb <- merge(wbp_fb, wood_cont[, .(continent = area, com_code, c_wood = wood)],
                    by = c("continent", "com_code"), all.x = TRUE)
wbp_fb[is.na(wood), `:=`(wood = c_wood)]
wbp_fb[, `:=`(c_wood = NULL)]

# Apply world average cf where no continental average available
wood_wrld <- wood_cont %>% 
  group_by(com_code) %>% 
  summarize(w_wood = mean(wood, na.rm = TRUE)) %>% 
  ungroup()
wbp_fb <- merge(wbp_fb, wood_wrld,
                    by = c("com_code"), all.x = TRUE)
wbp_fb[is.na(wood), `:=`(wood = w_wood)]
wbp_fb[, `:=`(w_wood = NULL, continent = NULL)]
rm(wood_cont, wood_wrld)

# Update wbp
wbp_fb <- wbp_fb[, `:=`(literature = "Na")]
wbp <- wbp[com_code !="c08"]
wbp <- rbind(wbp, wbp_fb, fill = TRUE)

rm(wbp_fb)

# Build continental and world average for the rest of commodities (c09 and c10)
wbp_cont <- wbp[is.na(area_code)]
wbp <- merge(wbp, regions[, .(area_code, continent)],
             by = "area_code")
wbp[continent == "EU", continent := "EUR"]

wbp_wrld <- wbp_cont %>% 
  group_by(com_code) %>% 
  summarize(w_wood = mean(wood, na.rm = TRUE)) %>% 
  ungroup()

wbp <- merge(wbp, wbp_cont[, .(continent = area, com_code, c_wood = wood)], 
             by = c("continent", "com_code"), all.x = TRUE)
wbp <- merge(wbp, wbp_wrld,
             by = c("com_code"), all.x = TRUE)
wbp[, `:=`(wood = ifelse(!is.na(wood), wood, 
                         ifelse(!is.na(c_wood), c_wood, w_wood)), 
           c_wood = NULL, w_wood = NULL)]
rm(wbp_cont, wbp_wrld)
wbp <- wbp[!is.na(area_code)]
wbp <- wbp[, `:=` (subitem = NULL, subcom_code = NULL)]

# Write wood content file
wbp_wood <- wbp[, c("area_code", "area", "com_code","item","wood","literature_wood")]
wbp_wood <- wbp_wood[is.na(literature_wood), `:=`(literature_wood = "Own calculations")]
wbp_wood<- wbp_wood %>%
  rename(literature = literature_wood)
fwrite(wbp_wood, "inst/wbp_wood_tidy.csv")

# Write percentage of recycled fiber used
wbp_recyc <- wbp[com_code == "c09", c("area_code", "area", "com_code","item", "recycled_fibre", "literature_recycledfibre")]
wbp_recyc <- wbp_recyc %>%
  rename(literature = literature_recycledfibre,
         proc_code = com_code,
         process = item)
wbp_recyc <- wbp_recyc[, `:=`(process = "Particle board production",
                                              proc_code = "p09")]
fwrite(wbp_recyc, "inst/wbp_recyc_tidy.csv")

rm(wbp, wbp_recyc)


cat("\nStep 8: Build wood basic density for wood-based panels (c08, c09, c10).\n")

# Select data
cf_wbd <- cf_carbon_prep[com_code %in% c("c08", "c09", "c10")]
cf_wbd <- cf_wbd[!is.na(area_code)]
cf_wbd <- cf_wbd[,`:=`(literature = NULL)]
cf_wbd <- merge(cf_wbd, wbp_wood,
            by = c("area_code", "area", "com_code", "item"), all = TRUE)

# Calculate wood basic density
cf_wbd[, cf := cf * (wood / 100)]
cf_wbd[, `:=`(wood = NULL)]
cf_wbd[, `:=`(cf_type = "cf_wdensity", description = "wood basic density")]

# Update cf_carbon_prep
cf_carbon_prep <- cf_carbon_prep[com_code %nin% c("c08", "c09", "c10")]
cf_carbon_prep <- rbind(cf_carbon_prep, cf_wbd, fill = TRUE)
cf_carbon_prep <- cf_carbon_prep[order(cf_carbon_prep$com_code, cf_carbon_prep$area_code),]

rm(cf_wbd, wbp_wood)


cat("\nStep 9: Build basic density of sawnwood C, sawnwood NC, veneer and plywood .\n")

# Read file
shrinkage <- fread ("inst/shrinkage_tidy.csv")

# Re-structure shrinkage file
# replace processes by commodities

## I AM HERE ## 

# Merge shrinkage data
bd <- merge(bd, shrinkage,
            by = c("continent", "area_code", "area", "com_code", "item", "source_code"), all.x = TRUE)

# calculate basic density for c04 (standard moisture content 15%)
bd[com_code %in% c("c04"),
   `:=`(cf_bd = cf * (100 - shrinkage) / (100 + 15))]

# calculate basic density for c05 (standard moisture content 12%)
bd[com_code %in% c("c05"),
   `:=`(cf_bd = cf * (100 - shrinkage) / (100 + 12))]

# calculate basic density for c06 and c07 (standard moisture content 8%)
bd[com_code %in% c("c06", "c07"),
   `:=`(cf_bd = cf * (100 - shrinkage) / (100 + 8))]

bd[, `:=`(shrinkage = NULL)]


cat("\nLast step: Building cf_carbon.\n")

# Calculate carbon content
bd[is.na(cf_bd), cf_bd := cf]
bd[, `:=`(cf = NULL)]
# Assumption carbon content for all is 0.5
# Except charcoal which is 0.85 (IPCC 2019, Ch. 12) and black liquor 0.35 (own assumption)
bd[, cf_carbon := cf_bd * 0.5]
bd[com_code %in% c("c16"),
   cf_carbon := cf_bd * 0.85]
bd[com_code %in% c("c20"),
   cf_carbon := cf_bd * 0.35]

carbon <- bd[, c("continent", "area_code", "area", "com_code","item","cf_carbon")]
carbon <- carbon[area_code %in% regions$area_code[regions$baci == TRUE]]
carbon <- carbon[with(carbon, order(area_code, com_code))]

# Write final cf_carbon_tidy
fwrite(carbon, "inst/carbon_tidy.csv")

rm(bd, carbon, carbon2, density, shrinkage)

# FEEDSTOCK ESTIMATE -------------------------------------------------------------

# Tidying estimates for feedstock composition for pulp production
pulp <- fread("inst/pulp_feedstock_raw.csv")
pulp[, `:=`(continent = regions$continent[match(pulp$area_code, regions$area_code)])]
pulp[continent == "EU", `:=`(continent = "EUR")]
pulp[, `:=`(roundwood = if_else(!is.na(roundwood), roundwood, 
                                roundwood[match(paste(pulp$continent, pulp$proc_code), paste(pulp$area, pulp$proc_code))]),
            chips = if_else(!is.na(chips), chips, 
                            chips[match(paste(pulp$continent, pulp$proc_code), paste(pulp$area, pulp$proc_code))]))]
# Write file
fwrite(pulp, "inst/pulp_feedstock_tidy.csv")


# CF SWE (?) ---------------------------------------------------------------------

#cat("\nTidying swe technical conversion factors.\n")

# provisorisch:
# used "cf_use_tidy" as basis
# divided 1/cf for c17 and c18 to obtain the right unit (m3sw/m3p)
# I used this: cf_in[unit=="m3rw/m3p", `:=`(cf = 1 / cf, unit = "m3p/m3rw")]
# calculated mean of cf from c04-c10 and multiplied by 2 (average of m3p/tonne) to obtain cf for c19
# divided cf of c11a to obtain cf for c20
