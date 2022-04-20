
library("data.table")
library("tidyverse")
source("R/01_tidy_functions.R")
`%nin%` = Negate(`%in%`)

regions <- fread("inst/regions.csv")
products <- fread("inst/products.csv")


# MB SUP ---------------------------------------------------------------------------------------------------
cat("\nTidying material balances for supply tables.\n")

# Begin with p04 and p05
cat("\nStep 1: Tidying material balances of sawnwood C (p04) and NC (p05) production.\n")

# Read file
sup_sawnwood <- fread("inst/mb_sawnwood_raw.csv")
sup_sawnwood <- sup_sawnwood[, `:=`(product = as.numeric(product), chips = as.numeric(chips), 
                                    sawdust = as.numeric(sawdust), shavings = as.numeric(shavings),
                                    shrinkage = as.numeric(shrinkage))]

# Aggregation and renaming items
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

# Separate shrinkage which will be used later
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

## Continue with p06 and p07 ##
cat("\nStep 2: Tidying material balances of veneer (p06) and plywood (p07) production.\n")

# Read file
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

# Create shrinkage csv
shrinkage_venply <- sup_venply[, c("area_code", "area", "proc_code","process","shrinkage")]
shrinkage <- rbind(shrinkage_sawnwood, shrinkage_venply)
# Write shrinkage file
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


# TCF USE --------------------------------------------------------------------------------------------------------------------

# Read main file
tcf_raw <- fread("inst/tcf_raw.csv")
tcf_use_prep <- tcf_raw[tcf_type == "tcf_use"]

cat("\nStep 1: Build weighted averages for c08 fibreboard.\n")

# Read file
share_fb <- fread("inst/share_fb.csv")

# Prepare structure to calculate weighted average of TCFs
tcf_average <- unique(tcf_use_prep[com_code == "c08",
                               .(area_code, area, com_code, item, unit, tcf_type)])
tcf_average[, `:=` (tcf = NA)]
tcf_average <- tcf_average[, `:=`(tcf = as.numeric(tcf))]
tcf_average <- merge(tcf_average,
                     share_fb[, .(area_code, area, com_code, item, c08_1_share, c08_2_share, c08_3_share)],
                     by = c("area_code", "area", "com_code", "item"),all.x = TRUE)

tcf_1 <- tcf_use_prep[subcom_code == "c08_1"]
tcf_1 <- tcf_1 %>%
  rename(tcf_1 = tcf)
tcf_2 <- tcf_use_prep[subcom_code == "c08_2"]
tcf_2 <- tcf_2 %>%
  rename(tcf_2 = tcf)
tcf_3 <- tcf_use_prep[subcom_code == "c08_3"]
tcf_3 <- tcf_3 %>%
  rename(tcf_3 = tcf)

tcf_average <- merge(tcf_average,
                     tcf_1[, .(area_code, area, com_code, item, unit, tcf_type, tcf_1)],
                     by = c("area_code", "area", "com_code", "item", "unit", "tcf_type"),
                     all.x = TRUE)
tcf_average <- merge(tcf_average,
                     tcf_2[, .(area_code, area, com_code, item, unit, tcf_type, tcf_2)],
                     by = c("area_code", "area", "com_code", "item", "unit", "tcf_type"),
                     all.x = TRUE)
tcf_average <- merge(tcf_average,
                     tcf_3[, .(area_code, area, com_code, item, unit, tcf_type, tcf_3)],
                     by = c("area_code", "area", "com_code", "item", "unit", "tcf_type"),
                     all.x = TRUE)

# Calculate weighted averages
tcf_average[, `:=`(tcf = (c08_1_share * tcf_1 + c08_2_share * tcf_2 + c08_3_share * tcf_3)
                   / (c08_1_share + c08_2_share + c08_3_share))]
# Calculate averages where no weights available
tcf_average[is.na(c08_1_share) & is.na(c08_2_share) & is.na(c08_3_share) & 
              !is.na(tcf_1) & !is.na(tcf_2) & !is.na(tcf_3),
            `:=`(tcf = (tcf_1 + tcf_2 + tcf_3)/3)]
tcf_average[is.na(c08_1_share) & is.na(c08_2_share) & is.na(c08_3_share) &
             !is.na(tcf_1) & !is.na(tcf_2),
           `:=`(tcf = (tcf_1 + tcf_2)/2)]
# Calculate averages where only some weights/TCFs available 
tcf_average[is.na (tcf) & !is.na(c08_1_share) & !is.na(c08_2_share) & !is.na(c08_3_share) & is.na(tcf_1),
            `:=`(tcf = (c08_2_share * tcf_2 + c08_3_share * tcf_3)/(c08_2_share + c08_3_share))]
tcf_average[is.na (tcf) & !is.na(c08_1_share) & !is.na(c08_2_share) & !is.na(c08_3_share) & is.na(tcf_2),
            `:=`(tcf = (c08_1_share * tcf_1 + c08_3_share * tcf_3)/(c08_1_share + c08_3_share))]
tcf_average[is.na (tcf) & !is.na(c08_1_share) & !is.na(c08_2_share) & !is.na(c08_3_share) & is.na(tcf_3),
            `:=`(tcf = (c08_1_share * tcf_1 + c08_2_share * tcf_2)/(c08_1_share + c08_2_share))]
# Use c08_1/2/3 values where only c08_1/2/3 products reported
tcf_average[is.na(tcf) & is.na(tcf_2) & is.na(tcf_3), `:=`(tcf = tcf_1)]
tcf_average[is.na(tcf) & is.na(tcf_1) & is.na(tcf_3), `:=`(tcf = tcf_2)]
tcf_average[is.na(tcf) & is.na(tcf_1) & is.na(tcf_2), `:=`(tcf = tcf_3)]

tcf_average[, `:=`(c08_1_share = NULL, c08_2_share = NULL, c08_3_share = NULL, tcf_1 = NULL, tcf_2 = NULL, tcf_3 = NULL)]
tcf_average <- tcf_average[order(tcf_average$area_code),]

rm(share_fb, tcf_1, tcf_2, tcf_3)

# Recalculate continental averages (due to new weighted countries' averages)
tcf_average <- merge(tcf_average, regions[, .(area_code, continent)],
                     by = "area_code",
                     all.x = TRUE)
tcf_average[continent == "EU", continent := "EUR"]
tcf_cont <- tcf_average %>%
  group_by(continent, com_code, item, tcf_type, unit) %>%
  summarize(tcf = mean(tcf, na.rm = TRUE)) %>%
  ungroup()
setnames(tcf_cont,c("continent"),c("area"))
setDT(tcf_cont)
tcf_cont <- tcf_cont[! area %in% c('XXX', 'ROW') & !is.na(area)]
tcf_average <- tcf_average[!is.na(area_code)]
tcf_average <- rbind(tcf_average, tcf_cont, fill=TRUE)

# Apply continental average TCF where no country-specific value available
tcf_average <- merge(tcf_average, tcf_cont[, .(continent = area, com_code, tcf_type, c_tcf = tcf)],
                     by = c("continent", "com_code", "tcf_type"), all.x = TRUE)
tcf_average[is.na(tcf), `:=`(tcf = c_tcf)]
tcf_average[, `:=`(c_tcf = NULL)]

# Apply world average TCF where no continental average available
tcf_wrld <- tcf_cont %>% 
  group_by(com_code, tcf_type) %>% 
  summarize(w_tcf = mean(tcf, na.rm = TRUE)) %>% 
  ungroup()
tcf_average <- merge(tcf_average, tcf_wrld,
                     by = c("com_code", "tcf_type"), all.x = TRUE)
tcf_average[is.na(tcf), `:=`(tcf = w_tcf)]
tcf_average[, `:=`(w_tcf = NULL, continent = NULL)]
rm(tcf_cont, tcf_wrld)

# Update tcf_use_prep
tcf_average <- tcf_average[, `:=`(description = NA, literature = NA, subcom_code = NA, subitem = NA, source = NA, source_code = NA)]
tcf_use_prep <- tcf_use_prep[com_code !="c08"]
tcf_use_prep <- rbind(tcf_use_prep, tcf_average)

rm(tcf_average)


cat("\nStep 2: Building average TCF for c11b mechanical and semi-chemical wood pulp.\n")

# Prepare structure to calculate average of TCFs (not weighted because no big difference)
tcf_average <- unique(tcf_use_prep[com_code == "c11b",
                               .(area_code, area, com_code, item, unit, tcf_type)])
tcf_average[, `:=` (tcf = NA)]
tcf_average <- tcf_average[, `:=`(tcf = as.numeric(tcf))]

tcf_1 <- tcf_use_prep[subcom_code == "c11b_1"]
tcf_1 <- tcf_1 %>%
  rename(tcf_1 = tcf)
tcf_2 <- tcf_use_prep[subcom_code == "c11b_2"]
tcf_2 <- tcf_2 %>%
  rename(tcf_2 = tcf)

tcf_average <- merge(tcf_average,
                     tcf_1[, .(area_code, area, com_code, item, unit, tcf_type, tcf_1)],
                     by = c("area_code", "area", "com_code", "item", "unit", "tcf_type"),
                     all.x = TRUE)
tcf_average <- merge(tcf_average,
                     tcf_2[, .(area_code, area, com_code, item, unit, tcf_type, tcf_2)],
                     by = c("area_code", "area", "com_code", "item", "unit", "tcf_type"),
                     all.x = TRUE)
rm(tcf_1, tcf_2)

# Calculate average
tcf_average[!is.na(tcf_1) & !is.na(tcf_2), `:=`(tcf = na_sum(tcf_1, tcf_2) / 2)]
# Use c11b_1/2 values where only c11b_1/2 products reported
tcf_average[!is.na(tcf_1) & is.na(tcf_2), `:=`(tcf = tcf_1)]
tcf_average[, `:=`(tcf_1 = NULL, tcf_2 = NULL)]

tcf_average <- tcf_average[order(tcf_average$area_code),]

# Recalculate continental averages (due to new countries' averages)
tcf_average <- merge(tcf_average, regions[, .(area_code, continent)],
                     by = "area_code",
                     all.x = TRUE)
tcf_average[continent == "EU", continent := "EUR"]

tcf_cont <- tcf_average %>%
  group_by(continent, com_code, item, tcf_type, unit) %>%
  summarize(tcf = mean(tcf, na.rm = TRUE)) %>%
  ungroup()
setnames(tcf_cont,c("continent"),c("area"))
setDT(tcf_cont)
tcf_cont <- tcf_cont[! area %in% c('XXX','ROW') & !is.na(area)]
tcf_average <- tcf_average[!is.na(area_code)]
tcf_average <- rbind(tcf_average, tcf_cont, fill=TRUE)

# Apply continental average TCF where no country-specific value available
tcf_average <- merge(tcf_average, tcf_cont[, .(continent = area, com_code, tcf_type, c_tcf = tcf)],
                     by = c("continent", "com_code", "tcf_type"), all.x = TRUE)
tcf_average[is.na(tcf), `:=`(tcf = c_tcf)]
tcf_average[, `:=`(c_tcf = NULL)]

# Apply world average TCF where no continental average available
tcf_wrld <- tcf_cont %>% 
  group_by(com_code, tcf_type) %>% 
  summarize(w_tcf = mean(tcf, na.rm = TRUE)) %>% 
  ungroup()
tcf_average <- merge(tcf_average, tcf_wrld,
                     by = c("com_code", "tcf_type"), all.x = TRUE)
tcf_average[is.na(tcf), `:=`(tcf = w_tcf)]
tcf_average[, `:=`(w_tcf = NULL, continent = NULL)]
rm(tcf_cont, tcf_wrld)

# Update tcf_use_prep
tcf_average <- tcf_average[, `:=`(description = NA, literature = NA, subcom_code = NA, subitem = NA, 
                                  source = NA, source_code = NA)]
tcf_use_prep <-tcf_use_prep[com_code !="c11b"]
tcf_use_prep <- rbind(tcf_use_prep, tcf_average)

rm(tcf_average)


cat("\nStep 3: Build averages for c14 Paper and paperboard.\n") 

# Read file
share_pp <- fread("inst/share_pp.csv")

## Build averages for c14_2 and c14_3 each

# Prepare structure and build averages for c14_2 (tcf_2)
tcf_2a <- tcf_use_prep[subcom_code == "c14_2a"]
tcf_2a <- tcf_2a %>%
  rename(tcf_2a = tcf)
tcf_2b <- tcf_use_prep[subcom_code == "c14_2b"]
tcf_2b <- tcf_2b %>%
  rename(tcf_2b = tcf)

tcf_2 <- merge(tcf_2a[, .(area_code, area, com_code, item, unit, tcf_type, tcf_2a)],
                     tcf_2b[, .(area_code, area, com_code, item, unit, tcf_type, tcf_2b)],
                     by = c("area_code", "area", "com_code", "item", "unit", "tcf_type"),
                     all.x = TRUE)
tcf_2[, `:=` (tcf_2 = NA)]
tcf_2 <- tcf_2[, `:=`(tcf_2 = as.numeric(tcf_2))]
tcf_2[, `:=` (subcom_code = "c14_2", subitem = "Printing and writing papers")]
rm(tcf_2a, tcf_2b)

# Calculate TCF average for c14_2
tcf_2[!is.na(tcf_2a) & !is.na(tcf_2b), `:=`(tcf_2 = na_sum(tcf_2a, tcf_2b) / 2)]
tcf_2[!is.na(tcf_2a) & is.na(tcf_2b), `:=`(tcf_2 = tcf_2a)]
tcf_2[!is.na(tcf_2b) & is.na(tcf_2a), `:=`(tcf_2 = tcf_2b)]
tcf_2[, `:=`(tcf_2a = NULL, tcf_2b = NULL)]

#tcf_2 <- tcf_2[order(tcf_2$area_code),]

# Build averages of tcf_3
#tcf_3 <- tcf_use_prep[subcom_code %in% c("c14_3a", "c14_3b", "c14_3c", "c14_3d", "c14_3e", "c14_3f", "c14_3g")]

# Prepare structure and build averages for c14_3 (tcf_3)
tcf_3a <- tcf_use_prep[subcom_code == "c14_3a"]
tcf_3a <- tcf_3a %>%
  rename(tcf_3a = tcf)
tcf_3b <- tcf_use_prep[subcom_code == "c14_3b"]
tcf_3b <- tcf_3b %>%
  rename(tcf_3b = tcf)
tcf_3c <- tcf_use_prep[subcom_code == "c14_3c"]
tcf_3c <- tcf_3c %>%
  rename(tcf_3c = tcf)
tcf_3d <- tcf_use_prep[subcom_code == "c14_3d"]
tcf_3d <- tcf_3d %>%
  rename(tcf_3d = tcf)
tcf_3e <- tcf_use_prep[subcom_code == "c14_3e"]
tcf_3e <- tcf_3e %>%
  rename(tcf_3e = tcf)
tcf_3f <- tcf_use_prep[subcom_code == "c14_3f"]
tcf_3f <- tcf_3f %>%
  rename(tcf_3f = tcf)
tcf_3g <- tcf_use_prep[subcom_code == "c14_3g"]
tcf_3g <- tcf_3g %>%
  rename(tcf_3g = tcf)

tcf_3 <- merge(tcf_3a[, .(area_code, area, com_code, item, unit, tcf_type, tcf_3a)],
               tcf_3b[, .(area_code, area, com_code, item, unit, tcf_type, tcf_3b)],
               by = c("area_code", "area", "com_code", "item", "unit", "tcf_type"),
               all.x = TRUE)
tcf_3 <- merge(tcf_3,
               tcf_3c[, .(area_code, area, com_code, item, unit, tcf_type, tcf_3c)],
               by = c("area_code", "area", "com_code", "item", "unit", "tcf_type"),
               all.x = TRUE)
tcf_3 <- merge(tcf_3,
               tcf_3d[, .(area_code, area, com_code, item, unit, tcf_type, tcf_3d)],
               by = c("area_code", "area", "com_code", "item", "unit", "tcf_type"),
               all.x = TRUE)
tcf_3 <- merge(tcf_3,
               tcf_3e[, .(area_code, area, com_code, item, unit, tcf_type, tcf_3e)],
               by = c("area_code", "area", "com_code", "item", "unit", "tcf_type"),
               all.x = TRUE)
tcf_3 <- merge(tcf_3,
               tcf_3f[, .(area_code, area, com_code, item, unit, tcf_type, tcf_3f)],
               by = c("area_code", "area", "com_code", "item", "unit", "tcf_type"),
               all.x = TRUE)
tcf_3 <- merge(tcf_3,
               tcf_3g[, .(area_code, area, com_code, item, unit, tcf_type, tcf_3g)],
               by = c("area_code", "area", "com_code", "item", "unit", "tcf_type"),
               all.x = TRUE)

tcf_3[, `:=` (tcf_3 = NA)]
tcf_3 <- tcf_3[, `:=`(tcf_3 = as.numeric(tcf_3))]
tcf_3[, `:=` (subcom_code = "c14_3", subitem = "Other paper and paperboard")]
rm(tcf_3a, tcf_3b, tcf_3c, tcf_3d, tcf_3e, tcf_3f, tcf_3g)

# Calculate TCF average for c14_3
tcf_3[, `:=` (tcf_3 = (tcf_3a + tcf_3b + tcf_3c + tcf_3d + tcf_3e + tcf_3f + tcf_3g)/ 7)]
tcf_3[is.na(tcf_3b), `:=` (tcf_3 = (tcf_3a + tcf_3c + tcf_3d + tcf_3e + tcf_3f + tcf_3g)/ 6)]
tcf_3[is.na (tcf_3) & !is.na(tcf_3a), `:=`(tcf_3 = tcf_3a)]

tcf_3[, `:=`(tcf_3a = NULL, tcf_3b = NULL, tcf_3c = NULL, tcf_3d = NULL, tcf_3e = NULL, tcf_3f = NULL, tcf_3g = NULL)]
tcf_3 <- tcf_3[order(tcf_3$area_code),]


# Prepare structure to calculate weighted average of TCFs for c14
tcf_average <- unique(tcf_use_prep[com_code == "c14",
                               .(area_code, area, com_code, item, unit, tcf_type)])
tcf_average[, `:=` (tcf = NA)]
tcf_average <- tcf_average[, `:=`(tcf = as.numeric(tcf))]

tcf_average <- merge(tcf_average,
                     share_pp[, .(area_code, area, com_code, item, c14_1_share, c14_2_share, c14_3_share)],
                     by = c("area_code", "area", "com_code", "item"),all.x = TRUE)

tcf_1 <- tcf_use_prep[subcom_code == "c14_1"]
tcf_1 <- tcf_1 %>%
  rename(tcf_1 = tcf)
# tcf_2 <- tcf_use_prep[subcom_code == "c14_2"]
# tcf_2 <- tcf_2 %>%
#   rename(tcf_2 = tcf)
# tcf_3 <- tcf_use_prep[subcom_code == "c14_3"]
# tcf_3 <- tcf_3 %>%
#   rename(tcf_3 = tcf)

tcf_average <- merge(tcf_average,
                     tcf_1[, .(area_code, area, com_code, item, unit, tcf_type, tcf_1)],
                     by = c("area_code", "area", "com_code", "item", "unit", "tcf_type"),
                     all.x = TRUE)
tcf_average <- merge(tcf_average,
                     tcf_2[, .(area_code, area, com_code, item, unit, tcf_type, tcf_2)],
                     by = c("area_code", "area", "com_code", "item", "unit", "tcf_type"),
                     all.x = TRUE)
tcf_average <- merge(tcf_average,
                     tcf_3[, .(area_code, area, com_code, item, unit, tcf_type, tcf_3)],
                     by = c("area_code", "area", "com_code", "item", "unit", "tcf_type"),
                     all.x = TRUE)

# Calculate weighted averages
tcf_average[, `:=`(tcf = (c14_1_share * tcf_1 + c14_2_share * tcf_2 + c14_3_share * tcf_3)
                   / (c14_1_share + c14_2_share + c14_3_share))]
tcf_average[is.na(tcf_1) & is.na(tcf), `:=`(tcf = (c14_2_share * tcf_2 + c14_3_share * tcf_3)
                   / (c14_2_share + c14_3_share))]
# Calculate averages where no weights available
tcf_average[is.na(c14_1_share) & is.na(c14_2_share) & is.na(c14_3_share) & 
              !is.na(tcf_1) & !is.na(tcf_2) & !is.na(tcf_3),
            `:=`(tcf = (tcf_1 + tcf_2 + tcf_3)/3)]
tcf_average[is.na(c14_1_share) & is.na(c14_2_share) & is.na(c14_3_share) &
              !is.na(tcf_1) & !is.na(tcf_2),
            `:=`(tcf = (tcf_1 + tcf_2)/2)]
tcf_average[is.na(c14_1_share) & is.na(c14_2_share) & is.na(c14_3_share) &
              !is.na(tcf_2) & !is.na(tcf_3),
            `:=`(tcf = (tcf_2 + tcf_3)/2)]
tcf_average[is.na(tcf) & is.na(tcf_2) & is.na(tcf_3), `:=`(tcf = tcf_1)]

tcf_average[, `:=`(c14_1_share = NULL, c14_2_share = NULL, c14_3_share = NULL, tcf_1 = NULL, tcf_2 = NULL, tcf_3 = NULL)]
tcf_average <- tcf_average[order(tcf_average$area_code),]

rm(share_fb, tcf_1, tcf_2, tcf_3)

# Recalculate continental averages (due to new weighted countries' averages)
tcf_average <- merge(tcf_average, regions[, .(area_code, continent)],
                     by = "area_code",
                     all.x = TRUE)
tcf_average[continent == "EU", continent := "EUR"]
tcf_cont <- tcf_average %>%
  group_by(continent, com_code, item, tcf_type, unit) %>%
  summarize(tcf = mean(tcf, na.rm = TRUE)) %>%
  ungroup()
setnames(tcf_cont,c("continent"),c("area"))
setDT(tcf_cont)
tcf_cont <- tcf_cont[! area %in% c('XXX', 'ROW') & !is.na(area)]
tcf_average <- tcf_average[!is.na(area_code)]
tcf_average <- rbind(tcf_average, tcf_cont, fill=TRUE)

# Apply continental average TCF where no country-specific value available
tcf_average <- merge(tcf_average, tcf_cont[, .(continent = area, com_code, tcf_type, c_tcf = tcf)],
                     by = c("continent", "com_code", "tcf_type"), all.x = TRUE)
tcf_average[is.na(tcf), `:=`(tcf = c_tcf)]
tcf_average[, `:=`(c_tcf = NULL)]

# Apply world average TCF where no continental average available
tcf_wrld <- tcf_cont %>% 
  group_by(com_code, tcf_type) %>% 
  summarize(w_tcf = mean(tcf, na.rm = TRUE)) %>% 
  ungroup()
tcf_average <- merge(tcf_average, tcf_wrld,
                     by = c("com_code", "tcf_type"), all.x = TRUE)
tcf_average[is.na(tcf), `:=`(tcf = w_tcf)]
tcf_average[, `:=`(w_tcf = NULL, continent = NULL)]
rm(tcf_cont, tcf_wrld)

# Update tcf_use_prep
tcf_average <- tcf_average[, `:=`(description = NA, literature = NA, subcom_code = NA, subitem = NA, source = NA, source_code = NA)]
tcf_use_prep <- tcf_use_prep[com_code !="c14"]
tcf_use_prep <- rbind(tcf_use_prep, tcf_average)

rm(tcf_average)


cat("\nStep 4: Tidying the rest of tcf_use_prep table.\n")

tcf_use_NA <- tcf_use_prep[com_code %nin% c("c08", "c11b", "c14")]

# Calculate continental averages
tcf_use_NA <- merge(tcf_use_NA, regions[, .(area_code, continent)],
                     by = "area_code",
                     all.x = TRUE)
tcf_use_NA[continent == "EU", continent := "EUR"]

tcf_cont <- tcf_use_NA %>%
  group_by(continent, com_code, item, tcf_type, unit) %>%
  summarize(tcf = mean(tcf, na.rm = TRUE)) %>%
  ungroup()
setnames(tcf_cont,c("continent"),c("area"))
setDT(tcf_cont)
tcf_cont <- tcf_cont[! area %in% c('XXX','ROW') & !is.na(area)]

# Apply continental average TCF where no country-specific value available
tcf_use_NA <- merge(tcf_use_NA, tcf_cont[, .(continent = area, com_code, tcf_type, c_tcf = tcf)],
                     by = c("continent", "com_code", "tcf_type"), all.x = TRUE)
tcf_use_NA[is.na(tcf), `:=`(tcf = c_tcf)]
tcf_use_NA[, `:=`(c_tcf = NULL)]

# Apply world average TCF where no continental average available
tcf_wrld <- tcf_cont %>% 
  group_by(com_code, tcf_type) %>% 
  summarize(w_tcf = mean(tcf, na.rm = TRUE)) %>% 
  ungroup()
tcf_use_NA <- merge(tcf_use_NA, tcf_wrld,
                     by = c("com_code", "tcf_type"), all.x = TRUE)
tcf_use_NA[is.na(tcf), `:=`(tcf = w_tcf)]
tcf_use_NA[, `:=`(w_tcf = NULL, continent = NULL)]
rm(tcf_cont, tcf_wrld)

# Update tcf_use_prep
tcf_use_prep <- tcf_use_prep[com_code %in% c("c08", "c11b", "c14")]
tcf_use_prep <- rbind(tcf_use_prep, tcf_use_NA)
rm(tcf_use_NA)


cat("\nStep 5: Giving units uniform names.\n")

tcf_use_prep <- tcf_use_prep[com_code %in% c("c17", "c18"), `:=`(unit = "m3p/m3sw")]
tcf_use_prep <- tcf_use_prep[com_code == "c13", `:=`(unit = "tonne/tonne")]


cat("\nStep 6: Adding missing commodities .\n")

# Create tcf for c10 as output of c01 and c02 each
tcf_use_prep[com_code=="c10", `:=`(source = products$item[products$com_code=="c01"], source_code = "c01")]
osb <- tcf_use_prep[com_code=="c10"]
osb[, `:=`(source = products$item[products$com_code=="c02"], source_code = "c02")]
tcf_use_prep <- rbind(tcf_use_prep, osb)
rm(osb)

# Create tcf for items where this equals 1
tcf_missing <- unique(tcf_raw[com_code %in% c("c01", "c02", "c03"),
                              .(area_code, area, com_code, item)])
tcf_missing[, `:=` (unit = "m3rw/m3p", tcf = "1", tcf_type = "tcf_use")]

# Write final tcf_use_tidy as csv
tcf_use <- rbind(tcf_use_prep, tcf_missing, fill = TRUE)
tcf_use <- tcf_use[, `:=`(subitem = NULL, subcom_code = NULL)]
tcf_use <- tcf_use[is.na(literature), `:=` (literature = "Own calculation")]
tcf_use <- tcf_use[order(tcf_use$com_code, tcf_use$area_code),]
#tcf_use <- tcf_use[, c("continent", "area_code", "area", "com_code","item","source_code","source","unit","tcf")]

fwrite(tcf_use, "inst/tcf_use_tidy.csv")

rm(tcf_missing, tcf_use_prep, tcf_use)

# TCF CARBON -----------------------------------------------------------------------------------

# Read main file
tcf_carbon_prep <- tcf_raw[tcf_type != "tcf_use"]


cat("\nStep 1: Aggregation of (sub)items to build weighted averages for c03 wood fuel, c06 veneer sheets and c07 plywood.\n")

# Read files
share_cnc <- fread("inst/share_cnc.csv")

# Prepare structure to calculate weighted average of TCFs
tcf_average <- unique(tcf_carbon_prep[com_code %in% c("c03", "c06", "c07"),
                              .(area_code, area, com_code, item, unit, tcf_type)])
tcf_average[, `:=` (tcf = NA)]

tcf_average <- merge(tcf_average,
                     share_cnc[, .(area_code, area, com_code, item, c_share, nc_share)],
                     by = c("area_code", "area", "com_code", "item"),all.x = TRUE)

tcf_c <- tcf_carbon_prep[subcom_code %in% c("c03_c","c06_c","c07_c")]
tcf_c <- tcf_c %>%
  rename(tcf_c = tcf)
tcf_nc <- tcf_carbon_prep[subcom_code %in% c("c03_nc","c06_nc","c07_nc")]
tcf_nc <- tcf_nc %>%
  rename(tcf_nc = tcf)

tcf_average <- merge(tcf_average,
                     tcf_c[, .(area_code, area, com_code, item, unit, tcf_type, tcf_c)],
                     by = c("area_code", "area", "com_code", "item", "unit", "tcf_type"),
                     all.x = TRUE)
tcf_average <- merge(tcf_average,
                     tcf_nc[, .(area_code, area, com_code, item, unit, tcf_type, tcf_nc)],
                     by = c("area_code", "area", "com_code", "item", "unit", "tcf_type"),
                     all.x = TRUE)

# Calculate weighted averages
tcf_average[, `:=`(tcf = (c_share * tcf_c + nc_share * tcf_nc) / 100)]
# Calculate averages where no weights available
tcf_average[is.na(c_share) & is.na(nc_share) & !is.na(tcf_nc) & !is.na(tcf_c),
            `:=`(tcf = (tcf_nc + tcf_c)/2)]
# Use c/nc values where only c/nc products reported
tcf_average[is.na(tcf_c), `:=`(tcf = tcf_nc)]
tcf_average[is.na(tcf_nc), `:=`(tcf = tcf_c)]

tcf_average[, `:=`(c_share = NULL, nc_share = NULL, tcf_c = NULL, tcf_nc = NULL)]
tcf_average <- tcf_average[order(tcf_average$area_code),]

rm(share_cnc, tcf_c, tcf_nc)

# Recalculate continental average (due to new weighted countries' averages)
tcf_average <- merge(tcf_average, regions[, .(area_code, continent)],
                     by = "area_code",
                     all.x = TRUE)
tcf_average[continent == "EU", continent := "EUR"]
tcf_cont <- tcf_average %>%
  group_by(continent, com_code, item, tcf_type, unit) %>%
  summarize(tcf = mean(tcf, na.rm = TRUE)) %>%
  ungroup()
setnames(tcf_cont,c("continent"),c("area"))
setDT(tcf_cont)
tcf_cont <- tcf_cont[! area %in% c('XXX', 'ROW') & !is.na(area)]
tcf_average <- tcf_average[!is.na(area_code)]
tcf_average <- rbind(tcf_average, tcf_cont, fill=TRUE)

# Apply continental average tcf where no country-specific value available
tcf_average <- merge(tcf_average, tcf_cont[, .(continent = area, com_code, tcf_type, c_tcf = tcf)],
                     by = c("continent", "com_code", "tcf_type"), all.x = TRUE)
tcf_average[is.na(tcf), `:=`(tcf = c_tcf)]
tcf_average[, `:=`(c_tcf = NULL)]

# Apply world average TCF where no continental average available
tcf_wrld <- tcf_cont %>% 
  group_by(com_code, tcf_type) %>% 
  summarize(w_tcf = mean(tcf, na.rm = TRUE)) %>% 
  ungroup()
tcf_average <- merge(tcf_average, tcf_wrld,
                     by = c("com_code", "tcf_type"), all.x = TRUE)
tcf_average[is.na(tcf), `:=`(tcf = w_tcf)]
tcf_average[, `:=`(w_tcf = NULL, continent = NULL)]
rm(tcf_cont, tcf_wrld)

# Update "tcf_carbon_prep"
tcf_average <- tcf_average[, `:=`(description = NA, literature = NA, subcom_code = NA, subitem = NA, source = NA, source_code = NA)]
tcf_carbon_prep <-tcf_carbon_prep[com_code %nin% c("c03", "c06", "c07"),]
tcf_carbon_prep <- rbind(tcf_carbon_prep, tcf_average)

rm(tcf_average, tcf_raw)


cat("\nStep 2: Calculate weighted averages for c01 industrial roundwood, coniferous.\n")

# Read file
share_irwc <- fread("inst/share_irwc.csv")

# Prepare structure to calculate weighted average of TCFs
tcf_average <- unique(tcf_carbon_prep[com_code == "c01",
                                   .(area_code, area, com_code, item, unit, tcf_type)])
tcf_average[, `:=` (tcf = NA)]
tcf_average <- tcf_average[, `:=`(tcf = as.numeric(tcf))]
tcf_average <- merge(tcf_average,
                     share_irwc[, .(area_code, area, com_code, item, c01_1_share, c01_2_share)],
                     by = c("area_code", "area", "com_code", "item"),all.x = TRUE)

tcf_1 <- tcf_carbon_prep[subcom_code == "c01_1"]
tcf_1 <- tcf_1 %>%
  rename(tcf_1 = tcf)
tcf_2 <- tcf_carbon_prep[subcom_code == "c01_2"]
tcf_2 <- tcf_2 %>%
  rename(tcf_2 = tcf)

tcf_average <- merge(tcf_average,
                     tcf_1[, .(area_code, area, com_code, item, unit, tcf_type, tcf_1)],
                     by = c("area_code", "area", "com_code", "item", "unit", "tcf_type"),
                     all.x = TRUE)
tcf_average <- merge(tcf_average,
                     tcf_2[, .(area_code, area, com_code, item, unit, tcf_type, tcf_2)],
                     by = c("area_code", "area", "com_code", "item", "unit", "tcf_type"),
                     all.x = TRUE)

# Calculate weighted averages
tcf_average[!is.na(tcf_1) & !is.na(tcf_2)
            , `:=`(tcf = ((c01_1_share * tcf_1 + c01_2_share * tcf_2) / 100))]
# Calculate averages where no weights available
tcf_average[is.na(c01_1_share) & is.na(c01_2_share) & !is.na(tcf_1) & !is.na(tcf_2),
            `:=`(tcf = (tcf_1 + tcf_2)/2)]
tcf_average[c01_1_share == "0" & c01_2_share == "0" & !is.na(tcf_1) & !is.na(tcf_2),
            `:=`(tcf = (tcf_1 + tcf_2)/2)]
# Use c/nc values where only c/nc products reported
tcf_average[is.na(tcf) & is.na(tcf_1), `:=`(tcf = tcf_2)]
tcf_average[is.na(tcf) & is.na(tcf_2), `:=`(tcf = tcf_1)]

tcf_average[, `:=`(c01_1_share = NULL, c01_2_share = NULL, tcf_1 = NULL, tcf_2 = NULL)]
tcf_average <- tcf_average[order(tcf_average$area_code),]

rm(share_irwc, tcf_1, tcf_2)

# Recalculate continental average (due to new weighted countries' averages)
tcf_average <- merge(tcf_average, regions[, .(area_code, continent)],
                     by = "area_code",
                     all.x = TRUE)
tcf_average[continent == "EU", continent := "EUR"]
tcf_cont <- tcf_average %>%
  group_by(continent, com_code, item, tcf_type, unit) %>%
  summarize(tcf = mean(tcf, na.rm = TRUE)) %>%
  ungroup()
setnames(tcf_cont,c("continent"),c("area"))
setDT(tcf_cont)
tcf_cont <- tcf_cont[! area %in% c('XXX', 'ROW') & !is.na(area)]
tcf_average <- tcf_average[!is.na(area_code)]
tcf_average <- rbind(tcf_average, tcf_cont, fill=TRUE)

# Apply continental average TCF where no country-specific value available
tcf_average <- merge(tcf_average, tcf_cont[, .(continent = area, com_code, tcf_type, c_tcf = tcf)],
                     by = c("continent", "com_code", "tcf_type"), all.x = TRUE)
tcf_average[is.na(tcf), `:=`(tcf = c_tcf)]
tcf_average[, `:=`(c_tcf = NULL)]

# Apply world average TCF where no continental average available
tcf_wrld <- tcf_cont %>% 
  group_by(com_code, tcf_type) %>% 
  summarize(w_tcf = mean(tcf, na.rm = TRUE)) %>% 
  ungroup()
tcf_average <- merge(tcf_average, tcf_wrld,
                     by = c("com_code", "tcf_type"), all.x = TRUE)
tcf_average[is.na(tcf), `:=`(tcf = w_tcf)]
tcf_average[, `:=`(w_tcf = NULL, continent = NULL)]
rm(tcf_cont, tcf_wrld)

# Update tcf_carbon_prep
tcf_average <- tcf_average[, `:=`(description = NA, literature = NA,
                                  subcom_code = NA, subitem = NA, source = NA, source_code = NA)]
tcf_carbon_prep <-tcf_carbon_prep[com_code != "c01",]
tcf_carbon_prep <- rbind(tcf_carbon_prep, tcf_average)

rm(tcf_average)


cat("\nStep 3: Build weighted averages for c02 industrial roundwood, non-coniferous.\n")

# Read file
share_irwnc <- fread("inst/share_irwnc.csv")

# Prepare structure to calculate weighted average of TCFs
tcf_average <- unique(tcf_carbon_prep[com_code == "c02",
                                   .(area_code, area, com_code, item, unit, tcf_type)])
tcf_average[, `:=` (tcf = NA)]
tcf_average <- tcf_average[, `:=`(tcf = as.numeric(tcf))]
tcf_average <- merge(tcf_average,
                     share_irwnc[, .(area_code, area, com_code, item, c02_1_share, c02_2_share)],
                     by = c("area_code", "area", "com_code", "item"), all.x = TRUE)

tcf_1 <- tcf_carbon_prep[subcom_code == "c02_1"]
tcf_1 <- tcf_1 %>%
  rename(tcf_1 = tcf)
tcf_2 <- tcf_carbon_prep[subcom_code == "c02_2"]
tcf_2 <- tcf_2 %>%
  rename(tcf_2 = tcf)

tcf_average <- merge(tcf_average,
                     tcf_1[, .(area_code, area, com_code, item, unit, tcf_type, tcf_1)],
                     by = c("area_code", "area", "com_code", "item", "unit", "tcf_type"),
                     all.x = TRUE)
tcf_average <- merge(tcf_average,
                     tcf_2[, .(area_code, area, com_code, item, unit, tcf_type, tcf_2)],
                     by = c("area_code", "area", "com_code", "item", "unit", "tcf_type"),
                     all.x = TRUE)

# Calculate weighted averages
tcf_average[!is.na(tcf_1) & !is.na(tcf_2)
            , `:=`(tcf = ((c02_1_share * tcf_1 + c02_2_share * tcf_2) / (c02_1_share + c02_2_share)))]
# Calculate averages where no weights available
tcf_average[is.na(c02_1_share) & is.na(c02_2_share) & !is.na(tcf_1) & !is.na(tcf_2),
            `:=`(tcf = (tcf_1 + tcf_2)/2)]
# Use c/nc values where only c/nc products reported
tcf_average[is.na(tcf_1), `:=`(tcf = tcf_2)]
tcf_average[is.na(tcf_2), `:=`(tcf = tcf_1)]

tcf_average[, `:=`(c02_1_share = NULL, c02_2_share = NULL, tcf_1 = NULL, tcf_2 = NULL)]
tcf_average <- tcf_average[order(tcf_average$area_code),]

rm(share_irwnc, tcf_1, tcf_2)

# Recalculate continental average (due to new weighted countries' averages)
tcf_average <- merge(tcf_average, regions[, .(area_code, continent)],
                     by = "area_code",
                     all.x = TRUE)
tcf_average[continent == "EU", continent := "EUR"]
tcf_cont <- tcf_average %>%
  group_by(continent, com_code, item, tcf_type, unit) %>%
  summarize(tcf = mean(tcf, na.rm = TRUE)) %>%
  ungroup()
setnames(tcf_cont,c("continent"),c("area"))
setDT(tcf_cont)
tcf_cont <- tcf_cont[! area %in% c('XXX', 'ROW') & !is.na(area)]
tcf_average <- tcf_average[!is.na(area_code)]
tcf_average <- rbind(tcf_average, tcf_cont, fill=TRUE)

# Apply continental average TCF where no country-specific value available
tcf_average <- merge(tcf_average, tcf_cont[, .(continent = area, com_code, tcf_type, c_tcf = tcf)],
                     by = c("continent", "com_code", "tcf_type"), all.x = TRUE)
tcf_average[is.na(tcf), `:=`(tcf = c_tcf)]
tcf_average[, `:=`(c_tcf = NULL)]

# Apply world average TCF where no continental average available
tcf_wrld <- tcf_cont %>% 
  group_by(com_code, tcf_type) %>% 
  summarize(w_tcf = mean(tcf, na.rm = TRUE)) %>% 
  ungroup()
tcf_average <- merge(tcf_average, tcf_wrld,
                     by = c("com_code", "tcf_type"), all.x = TRUE)
tcf_average[is.na(tcf), `:=`(tcf = w_tcf)]
tcf_average[tcf == "0", `:=`(tcf = w_tcf)]
tcf_average[, `:=`(w_tcf = NULL, continent = NULL)]
rm(tcf_cont, tcf_wrld)

# Update tcf_carbon_prep
tcf_average <- tcf_average[, `:=`(description = NA, literature = NA, subcom_code = NA,
                                  subitem = NA, source = NA, source_code = NA)]
tcf_carbon_prep <-tcf_carbon_prep[com_code != "c02"]
tcf_carbon_prep <- rbind(tcf_carbon_prep, tcf_average)
rm(tcf_average)


### I STAYED HERE: control this step since it's copy paste ###
cat("\nStep 4: Build weighted averages for c08 fibreboard.\n")

# Read file
share_fb <- fread("inst/share_fb.csv")

# Prepare structure to calculate weighted average of TCFs
tcf_average <- unique(tcf_carbon_prep[com_code == "c08",
                                   .(area_code, area, com_code, item, unit, tcf_type)])
tcf_average[, `:=` (tcf = NA)]
tcf_average <- tcf_average[, `:=`(tcf = as.numeric(tcf))]
tcf_average <- merge(tcf_average,
                     share_fb[, .(area_code, area, com_code, item, c08_1_share, c08_2_share, c08_3_share)],
                     by = c("area_code", "area", "com_code", "item"),all.x = TRUE)

tcf_1 <- tcf_use_prep[subcom_code == "c08_1"]
tcf_1 <- tcf_1 %>%
  rename(tcf_1 = tcf)
tcf_2 <- tcf_use_prep[subcom_code == "c08_2"]
tcf_2 <- tcf_2 %>%
  rename(tcf_2 = tcf)
tcf_3 <- tcf_use_prep[subcom_code == "c08_3"]
tcf_3 <- tcf_3 %>%
  rename(tcf_3 = tcf)

tcf_average <- merge(tcf_average,
                     tcf_1[, .(area_code, area, com_code, item, unit, tcf_type, tcf_1)],
                     by = c("area_code", "area", "com_code", "item", "unit", "tcf_type"),
                     all.x = TRUE)
tcf_average <- merge(tcf_average,
                     tcf_2[, .(area_code, area, com_code, item, unit, tcf_type, tcf_2)],
                     by = c("area_code", "area", "com_code", "item", "unit", "tcf_type"),
                     all.x = TRUE)
tcf_average <- merge(tcf_average,
                     tcf_3[, .(area_code, area, com_code, item, unit, tcf_type, tcf_3)],
                     by = c("area_code", "area", "com_code", "item", "unit", "tcf_type"),
                     all.x = TRUE)

# Calculate weighted averages
tcf_average[, `:=`(tcf = (c08_1_share * tcf_1 + c08_2_share * tcf_2 + c08_3_share * tcf_3)
                   / (c08_1_share + c08_2_share + c08_3_share))]
# Calculate averages where no weights available
tcf_average[is.na(c08_1_share) & is.na(c08_2_share) & is.na(c08_3_share) & 
              !is.na(tcf_1) & !is.na(tcf_2) & !is.na(tcf_3),
            `:=`(tcf = (tcf_1 + tcf_2 + tcf_3)/3)]
tcf_average[is.na(c08_1_share) & is.na(c08_2_share) & is.na(c08_3_share) &
              !is.na(tcf_1) & !is.na(tcf_2),
            `:=`(tcf = (tcf_1 + tcf_2)/2)]
# Calculate averages where only some weights/TCFs available 
tcf_average[is.na (tcf) & !is.na(c08_1_share) & !is.na(c08_2_share) & !is.na(c08_3_share) & is.na(tcf_1),
            `:=`(tcf = (c08_2_share * tcf_2 + c08_3_share * tcf_3)/(c08_2_share + c08_3_share))]
tcf_average[is.na (tcf) & !is.na(c08_1_share) & !is.na(c08_2_share) & !is.na(c08_3_share) & is.na(tcf_2),
            `:=`(tcf = (c08_1_share * tcf_1 + c08_3_share * tcf_3)/(c08_1_share + c08_3_share))]
tcf_average[is.na (tcf) & !is.na(c08_1_share) & !is.na(c08_2_share) & !is.na(c08_3_share) & is.na(tcf_3),
            `:=`(tcf = (c08_1_share * tcf_1 + c08_2_share * tcf_2)/(c08_1_share + c08_2_share))]
# Use c08_1/2/3 values where only c08_1/2/3 products reported
tcf_average[is.na(tcf) & is.na(tcf_2) & is.na(tcf_3), `:=`(tcf = tcf_1)]
tcf_average[is.na(tcf) & is.na(tcf_1) & is.na(tcf_3), `:=`(tcf = tcf_2)]
tcf_average[is.na(tcf) & is.na(tcf_1) & is.na(tcf_2), `:=`(tcf = tcf_3)]

tcf_average[, `:=`(c08_1_share = NULL, c08_2_share = NULL, c08_3_share = NULL, tcf_1 = NULL, tcf_2 = NULL, tcf_3 = NULL)]
tcf_average <- tcf_average[order(tcf_average$area_code),]

rm(share_fb, tcf_1, tcf_2, tcf_3)

# Recalculate continental averages (due to new weighted countries' averages)
tcf_average <- merge(tcf_average, regions[, .(area_code, continent)],
                     by = "area_code",
                     all.x = TRUE)
tcf_average[continent == "EU", continent := "EUR"]
tcf_cont <- tcf_average %>%
  group_by(continent, com_code, item, tcf_type, unit) %>%
  summarize(tcf = mean(tcf, na.rm = TRUE)) %>%
  ungroup()
setnames(tcf_cont,c("continent"),c("area"))
setDT(tcf_cont)
tcf_cont <- tcf_cont[! area %in% c('XXX', 'ROW') & !is.na(area)]
tcf_average <- tcf_average[!is.na(area_code)]
tcf_average <- rbind(tcf_average, tcf_cont, fill=TRUE)

# Apply continental average TCF where no country-specific value available
tcf_average <- merge(tcf_average, tcf_cont[, .(continent = area, com_code, tcf_type, c_tcf = tcf)],
                     by = c("continent", "com_code", "tcf_type"), all.x = TRUE)
tcf_average[is.na(tcf), `:=`(tcf = c_tcf)]
tcf_average[, `:=`(c_tcf = NULL)]

# Apply world average TCF where no continental average available
tcf_wrld <- tcf_cont %>% 
  group_by(com_code, tcf_type) %>% 
  summarize(w_tcf = mean(tcf, na.rm = TRUE)) %>% 
  ungroup()
tcf_average <- merge(tcf_average, tcf_wrld,
                     by = c("com_code", "tcf_type"), all.x = TRUE)
tcf_average[is.na(tcf), `:=`(tcf = w_tcf)]
tcf_average[, `:=`(w_tcf = NULL, continent = NULL)]
rm(tcf_cont, tcf_wrld)

# Update tcf_use_prep
tcf_average <- tcf_average[, `:=`(description = NA, literature = NA, subcom_code = NA, subitem = NA, source = NA, source_code = NA)]
tcf_use_prep <- tcf_carbon_prep[com_code !="c08"]
tcf_carbon_prep <- rbind(tcf_carbon_prep, tcf_average)

rm(tcf_average)



# cat("\nStep X: c17 and c18.\n")
# # build  "oven-dry tonne/m³ loose" or odmt/m3p 
# # by: 1/(green swe to oven-dry tonne)/(Oven-dry tonne/m³ loose)
# # be careful with unit (which will later be turn around or not, in the Use Script)

# cat("\nStep XX: Tidying the rest (c04, c05, c09, c10).\n")

# tcf of c01, c02, c03, c17 and c18 already refer to the basic density
# that is (oven) dry weight to green volume (c01-c03) or m3 product (c17,c18)


## Old calculations for TCF_carbon ## ----------------------
# Step 1
cat("\nStep 1: Tidying density-related factors.\n")

# Select tcf_density & tcf_shipping_weight from tcf_raw
tcf_raw

#density[tcf == 0, tcf :=NA]

# convert kg into tonne
density[unit %in% c("kg/m3"),
        `:=`(tcf = tcf / 1000)]
density[unit == "kg/m3", unit := "tonne/m3"]

density_cont <- density[is.na(area_code)]
density <- merge(density, regions[, .(area_code, continent)],
                 by = "area_code")
density[continent == "EU", continent :="EUR"]

# calculate world averages
density_wrld <- density_cont %>%
  group_by(com_code, source_code, unit) %>%
  summarize(w_tcf = mean(tcf, na.rm = TRUE)) %>%
  ungroup()

# apply continental average tcf where no country-specific value available
density <- merge(density, density_cont[, .(continent = area, com_code, source_code, unit, c_tcf = tcf)],
            by = c ("continent", "com_code", "source_code", "unit"), all.x = TRUE)

# apply world average tcf where no continental average available
density <- merge(density, density_wrld,
            by = c("com_code", "source_code", "unit"), all.x = TRUE)

density[, `:=`(tcf = ifelse(!is.na(tcf), tcf, 
                         ifelse(!is.na(tcf), c_tcf, w_tcf)), 
           c_tcf = NULL, w_tcf = NULL)]


density <- density[, c("continent", "area_code", "area", "com_code","item","source_code", "source", "unit", "tcf")]

fwrite(density, "inst/density_tidy.csv")
rm(density, density_cont, density_wrld)


## step 2
cat("\nStep 2: Tidying wood percetange value in wood-based panels.\n")

# tidy wood percentage in wood-based panels
wbp <- fread("inst/wood_wbp.csv")

wbp[wood == 0, wood := NA]

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

wbp <- wbp[, c("continent", "area_code", "area", "com_code","item","wood")]

fwrite(wbp, "inst/wbp_tidy.csv")
rm(wbp, wbp_cont, wbp_wrld)

## step 3: basic density for wood-based panels (c08,c09,c10)
cat("\nStep 3: Calculate basic density of wood-based panels.\n")

# upload basic density and wood percentage data
density <- fread("inst/density_tidy.csv")
wbp <- fread("inst/wbp_tidy.csv")

bd <- merge(density, wbp,
             by = c("continent", "area_code", "area", "com_code", "item"), all.x = TRUE)

# calculate wood basic density
bd[, tcf_bd := tcf * (wood / 100)]

bd[, `:=`(wood = NULL)]

## step 4: basic density for sawnwood C, sawnwood NC, veneer and plywood (c04-c07)
cat("\nStep 4: Calculate basic density of sawnwood C, sawnwood NC, veneer and plywood .\n")

# upload & tidy shrinkage data
shrinkage <- fread ("inst/shrinkage.csv")
shrinkage[shrinkage == 0, shrinkage := NA]

shrinkage_cont <- shrinkage[is.na(area_code)]
shrinkage <- merge(shrinkage, regions[, .(area_code, continent)],
             by = "area_code")
shrinkage[continent == "EU", continent := "EUR"]

shrinkage_wrld <- shrinkage_cont %>% 
  group_by(com_code, source_code) %>% 
  summarize(w_shrinkage = mean(shrinkage, na.rm = TRUE)) %>% 
  ungroup()

shrinkage <- merge(shrinkage, shrinkage_cont[, .(continent = area, com_code, source_code, c_shrinkage = shrinkage)], 
             by = c("continent", "com_code", "source_code"), all.x = TRUE)

shrinkage <- merge(shrinkage, shrinkage_wrld,
             by = c("com_code", "source_code"), all.x = TRUE)

shrinkage[, `:=`(shrinkage = ifelse(!is.na(shrinkage), shrinkage, 
                        ifelse(!is.na(c_shrinkage), c_shrinkage, w_shrinkage)), 
           c_shrinkage = NULL, w_shrinkage = NULL)]


shrinkage <- shrinkage[, c("continent", "area_code", "area", "com_code","item","source_code","shrinkage")]

fwrite(shrinkage, "inst/shrinkage_tidy.csv")
rm(wbp, shrinkage_cont, shrinkage_wrld)

# merge shrinkage data
shrinkage <- fread ("inst/shrinkage_tidy.csv")
bd <- merge(bd, shrinkage,
            by = c("continent", "area_code", "area", "com_code", "item", "source_code"), all.x = TRUE)

# calculate basic density for c04 (standard moisture content 15%)
bd[com_code %in% c("c04"),
   `:=`(tcf_bd = tcf * (100 - shrinkage) / (100 + 15))]

# calculate basic density for c05 (standard moisture content 12%)
bd[com_code %in% c("c05"),
   `:=`(tcf_bd = tcf * (100 - shrinkage) / (100 + 12))]

# calculate basic density for c06 and c07 (standard moisture content 8%)
bd[com_code %in% c("c06", "c07"),
   `:=`(tcf_bd = tcf * (100 - shrinkage) / (100 + 8))]

bd[, `:=`(shrinkage = NULL)]

## step 6: Building carbon TCF
cat("\nStep 6: Building carbon TCF.\n")

bd[is.na(tcf_bd), tcf_bd := tcf]

bd[, `:=`(tcf = NULL)]


# Multiply by 0.5 all
bd[, tcf_carbon := tcf_bd * 0.5]

# Except for charcoal, which has 0.85 carbon content
bd[com_code %in% c("c16"),
      tcf_carbon := tcf_bd * 0.85]

# Except for black liquor, which has 0.35 carbon content
bd[com_code %in% c("c20"),
   tcf_carbon := tcf_bd * 0.35]

carbon <- bd[, c("continent", "area_code", "area", "com_code","item","tcf_carbon")]

carbon <- carbon[area_code %in% regions$area_code[regions$baci == TRUE]]
carbon <- carbon[with(carbon, order(area_code, com_code))]


fwrite(carbon, "inst/carbon_tidy.csv")

# build average for c06 and c07
# quick fix in excel
# but ideally it should be here 
#
# carbon2 <- fread("inst/carbon_tidy.csv")
# 
# c2 <- carbon2[tcf_carbon, area_code, source_code %in% c("c02"),
#                by = c("com_code"), all.x = TRUE]
# # 
# carbon3 <- carbon2[com_code %in% c("c06", "c07"),
#                    source_code %in% c("c01"),
#                    tc_c02 := tcf_carbon]

                   
                   
fwrite(carbon, "inst/carbon_tidy.csv")

rm(bd, carbon, carbon2, density, shrinkage)

# TCF SWE (?) ---------------------------------------------------------------------

#cat("\nTidying swe technical conversion factors.\n")

# provisorisch:
# used "tcf_use_tidy" as basis
# divided 1/tcf for c17 and c18 to obtain the right unit (m3sw/m3p)
# I used this: tcf_in[unit=="m3rw/m3p", `:=`(tcf = 1 / tcf, unit = "m3p/m3rw")]
# calculated mean of tcf from c04-c10 and multiplied by 2 (average of m3p/tonne) to obtain tcf for c19
# divided tcf of c11a to obtain tcf for c20
