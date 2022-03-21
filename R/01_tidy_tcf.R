
library("data.table")
library("tidyverse")
source("R/01_tidy_functions.R")

regions <- fread("inst/regions.csv")
products <- fread("inst/products.csv")


# MB SUP ----------------------------------------------------------------
cat("\nTidying material balances for supply tables.\n")

# Begin with c04 and c05
cat("\nStep 1: Tidying material balances of sawnwood c04 and c05.\n")

# Read file
sup_sawnwood <- fread("inst/mb_sawnwood_raw.csv")

# Aggregation and renaming items
sup_sawnwood[,  `:=`(residues = na_sum(sup_sawnwood$sawdust, sup_sawnwood$shavings))]
sup_sawnwood[, `:=`(sawdust = NULL, shavings = NULL)]
sup_sawnwood <- sup_sawnwood[, c("area_code", "area", "proc_code", 
                                 "process", "product","chips","residues","shrinkage")]

# Apply continental average MB where no country-specific value available
sup_cont <- sup_sawnwood[is.na(area_code)]
sup_sawnwood <- merge(sup_sawnwood, regions[, .(area_code, continent)],
                      by = "area_code")
sup_sawnwood[continent == "EU", continent := "EUR"]
sup_sawnwood <- merge(sup_sawnwood, sup_cont[, .(continent = area, proc_code, 
  c_product = product, c_chips = chips, c_residues = residues)],
  by = c("continent", "proc_code"), all.x = TRUE)

# Calculate new "losses"(shrinkage)
sup_sawnwood[, c_losses := 100 - (c_product + c_chips + c_residues)]

# Correct c_losses to 0 when this is -1 and allocate -1 to chips
sup_sawnwood[c_losses == -1,
             c_losses := 0]
sup_sawnwood[c_losses == 0,
             c_chips := c_chips - 1]

# Or allocation in both but this does not seem to work
# sup_sawnwood[c_losses == 0,
#              c_chips := c_chips - 0.5,
#              c_residues := c_residues - 0.5]

sup_sawnwood[!is.na(product) & is.na(chips), 
    `:=`(chips = (100 - product - c_losses) / (c_chips + c_residues) * c_chips,
         residues = (100 - product - c_losses) / (c_chips + c_residues) * c_residues)]
sup_sawnwood[is.na(product), 
    `:=`(product = c_product, chips = c_chips, residues = c_residues)]

sup_sawnwood[, `:=`(c_product = NULL, c_chips = NULL, c_residues = NULL, c_losses = NULL)]

# Apply world average MB where no continental average available
sup_wrld <- sup_cont %>% 
  group_by(proc_code) %>% 
  summarize(w_product = mean(product, na.rm = TRUE),
            w_chips = mean(chips, na.rm = TRUE),
            w_residues = mean(residues, na.rm = TRUE)) %>% 
  ungroup()
sup_sawnwood <- merge(sup_sawnwood, sup_wrld,
  by = c("proc_code"), all.x = TRUE)
sup_sawnwood[is.na(product), 
    `:=`(product = w_product, chips = w_chips, residues = w_residues)]
sup_sawnwood[, `:=`(w_product = NULL, w_chips = NULL, w_residues = NULL)]

# Create shrinkage csv
sup_sawnwood[is.na(shrinkage),
             `:=`(shrinkage = 100 - product - chips - residues)]
shrinkage_sawnwood <- sup_sawnwood[, c("area_code", "area", "proc_code","process","shrinkage")]
fwrite(shrinkage_sawnwood, "inst/shrinkage_sawnwood.csv")

# Convert into percentages ignoring losses
sup_sawnwood[, total := product + chips + residues]
sup_sawnwood[, `:=`(product = product / total,
           chips = chips / total,
           residues = residues / total,
           total = NULL)]

rm(sup_cont, sup_wrld, shrinkage_sawnwood)


## Continue with c06 and c07 ##
cat("\nStep 2: Tidying material balances of veneer and plywood.\n")

# Read file
sup_venply <- fread("inst/mb_veneer,plywood_raw.csv")
share_cnc <- fread("inst/share_cnc.csv")

share_venply <- share_cnc[com_code %in% c("c06","c07")]
setnames(share_venply,c("com_code","item"),c("proc_code","process"))
share_venply[proc_code == "c06", proc_code := "p06"]
share_venply[proc_code == "c07", proc_code := "p07"]

# Rename items 
setnames(sup_venply,c("chips,peeler_cores","sanding_dust"),c("chips","residues"))

# Prepare structure to calculate weighted average of TCFs
sup_venply_average <- sup_venply[subproc_code %in% c("p06_c", "p07_c")]
sup_venply_average[, `:=` (subproc_code = NULL, subprocess = NULL,
                           product = NA, chips = NA, residues = NA, shrinkage = NA)]
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
sup_venply[is.na(product), 
             `:=`(product = (product_c + product_nc)/2, chips = (chips_c + chips_nc)/2,
                  residues = (residues_c + residues_nc)/2, shrinkage = (shrinkage_c + shrinkage_nc)/2)]
sup_venply[is.na(product) & is.na(product_c), 
           `:=`(product = product_nc, chips = chips_nc, residues = residues_nc, shrinkage = shrinkage_nc)]
sup_venply[is.na(product) & is.na(product_nc), 
           `:=`(product = product_c, chips = chips_c, residues = residues_c, shrinkage = shrinkage_c)]

sup_venply[, `:=`(c_share = NULL, product_c = NULL, nc_share = NULL, product_nc = NULL, chips_c = NULL, chips_nc = NULL,
                  residues_c = NULL, residues_nc = NULL, shrinkage_c = NULL, shrinkage_nc = NULL, literature = NULL)]

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
sup_cont <- sup_cont[ area != 'XXX']
sup_cont <- sup_cont[ area != 'ROW']
sup_venply <- sup_venply[!is.na(area_code)]
sup_venply <- rbind(sup_venply, sup_cont, fill=TRUE)

# Apply continental average MB where no country-specific value available
sup_cont <- sup_venply[is.na(area_code)]
sup_venply <- merge(sup_venply, sup_cont[, .(continent = area, proc_code, 
                                                 c_product = product, c_chips = chips, c_residues = residues)],
                      by = c("continent", "proc_code"), all.x = TRUE)

# Calculate new "losses" ("shrinkage")
sup_venply[, c_losses := 100 - (c_product + c_chips + c_residues)]
sup_venply[!is.na(product) & is.na(chips), 
             `:=`(chips = (100 - product - c_losses) / (c_chips + c_residues) * c_chips,
                  residues = (100 - product - c_losses) / (c_chips + c_residues) * c_residues)]
sup_venply[is.na(product), 
             `:=`(product = c_product, chips = c_chips, residues = c_residues)]

sup_venply[, `:=`(c_product = NULL, c_chips = NULL, c_residues = NULL, c_losses = NULL)]

# Apply world average MB where no continental average available
sup_wrld <- sup_cont %>% 
  group_by(proc_code) %>% 
  summarize(w_product = mean(product, na.rm = TRUE),
            w_chips = mean(chips, na.rm = TRUE),
            w_residues = mean(residues, na.rm = TRUE)) %>% 
  ungroup()
sup_venply <- merge(sup_venply, sup_wrld,
                      by = c("proc_code"), all.x = TRUE)
sup_venply[is.na(product), 
             `:=`(product = w_product, chips = w_chips, residues = w_residues)]
sup_venply[, `:=`(w_product = NULL, w_chips = NULL, w_residues = NULL)]

# Shift 20% of chips to product and residues (original category was "chips, peeler cores, etc.")
sup_venply[, `:=`(product = product + chips * 0.2, residues = residues + chips * 0.2, chips = chips * 0.6)]

# Create shrinkage csv
sup_venply[is.na(shrinkage),
             `:=`(shrinkage = 100 - product - chips - residues)]
shrinkage_venply <- sup_venply[, c("area_code", "area", "proc_code","process","shrinkage")]
fwrite(shrinkage_venply, "inst/shrinkage_venply.csv")

# Convert into percentages ignoring losses
sup_venply[, total := product + chips + residues]
sup_venply[, `:=`(product = product / total,
           chips = chips / total,
           residues = residues / total,
           total = NULL)]

# Save mb_sup_tidy
sup_venply <- sup_venply[!is.na(area_code)]
sup <- rbind(sup_sawnwood, sup_venply)
fwrite(sup, "inst/mb_sup_tidy.csv")

rm(shrinkage_venply, sup_cont, sup_wrld, sup_sawnwood, sup_venply, sup)


# TCF -----------------------------------------------------------------------------------------

cat("\nStep 1: Aggregation of (sub)items to build weighted averages for c03 wood fuel, c06 veneer sheets, c07 plywood.\n")

# Read files
tcf_raw <- fread("inst/tcf_raw.csv")
share_cnc <- fread("inst/share_cnc.csv")

# Preparing structure of table
tcf_average <- tcf_raw[subcom_code %in% c("c03_c", "c06_c", "c07_c")]
tcf_average[, `:=` (subcom_code = NA, subitem = NA, source_code = NA, source = NA, tcf = NA)]
tcf_average <- merge(tcf_average,
                     share_cnc[, .(area_code, area, com_code, item, c_share, nc_share)],
                     by = c("area_code", "area", "com_code", "item"),all.x = TRUE)

tcf_c <- tcf_raw[subcom_code %in% c("c03_c","c06_c","c07_c")]
tcf_c <- tcf_c %>%
  rename(tcf_c = tcf)
tcf_nc <- tcf_raw[subcom_code %in% c("c03_nc","c06_nc","c07_nc")]
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

# Calculation of (weighted) averages
tcf_average[, `:=`(tcf = (c_share * tcf_c + nc_share * tcf_nc) / 100)]
tcf_average[is.na(tcf_c), `:=`(tcf = tcf_nc)]
tcf_average[is.na(tcf_nc), `:=`(tcf = tcf_c)]
tcf_average[is.na(c_share) & is.na(nc_share) & !is.na(tcf_nc) & !is.na(tcf_c),
                                                      `:=`(tcf = (tcf_nc + tcf_c)/2)]
tcf_average[, `:=`(c_share = NULL, nc_share = NULL, tcf_c = NULL, tcf_nc = NULL, 
                   subcom_code = NULL, subitem = NULL, source_code = NULL, source = NULL,
                   literature = NULL, description = NULL)]
tcf_average <- tcf_average[order(tcf_average$area_code),]

rm(share_cnc, tcf_c, tcf_nc)

# Recalculate continental averages
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
tcf_cont <- tcf_cont[ area != 'XXX']
tcf_cont <- tcf_cont[ area != 'ROW']
tcf_cont <- tcf_cont[, "continent":= NA]
tcf_cont <- tcf_cont[, "area_code":= NA]

tcf_average <- rbind((tcf_average[!is.na(continent)]), tcf_cont, fill=TRUE)
#tcf_average <- tcf_average[continent != 'XXX']

# Apply continental average tcf where no country-specific value available
tcf_average <- merge(tcf_average, tcf_cont[, .(continent = area, com_code, tcf_type, c_tcf = tcf)],
                    by = c("continent", "com_code", "tcf_type"), all.x = TRUE)
tcf_average[is.na(tcf), `:=`(tcf = c_tcf)]
tcf_average[, `:=`(c_tcf = NULL)]

# Apply world average tcf where no continental average available
tcf_wrld <- tcf_cont %>% 
  group_by(com_code, tcf_type) %>% 
  summarize(w_tcf = mean(tcf, na.rm = TRUE)) %>% 
  ungroup()
tcf_average <- merge(tcf_average, tcf_wrld,
                    by = c("com_code", "tcf_type"), all.x = TRUE)
tcf_average[is.na(tcf), `:=`(tcf = w_tcf)]
tcf_average[, `:=`(w_tcf = NULL, continent = NULL)]
rm(tcf_cont, tcf_wrld)

# Create "tcf_prep" which is the updated version of tcf_raw
tcf_average <- tcf_average[, `:=`(description = NA, literature = NA, subcom_code = NA, subitem = NA, source = NA, source_code = NA)]
`%nin%` = Negate(`%in%`)
tcf_raw <-tcf_raw[com_code %nin% c("c03", "c06", "c07"),]
tcf_prep <- rbind(tcf_raw, tcf_average)

rm(tcf_average, tcf_raw)


cat("\nNow calculate weighted averages for c01 industrial roundwood, coniferous.\n")

# Read file
share_irwc <- fread("inst/share_irwc.csv")

# Prepare structure of table
tcf_average <- tcf_prep[subcom_code == "c01_1"]
tcf_average[, `:=` (subcom_code = NA, subitem = NA, source_code = NA, source = NA, tcf = NA)]
tcf_average <- merge(tcf_average,
                     share_irwc[, .(area_code, area, com_code, item, c01_1_share, c01_2_share)],
                     by = c("area_code", "area", "com_code", "item"),all.x = TRUE)

tcf_1 <- tcf_prep[subcom_code == "c01_1"]
tcf_1 <- tcf_1 %>%
  rename(tcf_1 = tcf)
tcf_2 <- tcf_prep[subcom_code == "c01_2"]
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

# Calculation of (weighted) averages
tcf_average[!is.na(tcf_1) & !is.na(tcf_2)
            , `:=`(tcf = ((c01_1_share * tcf_1 + c01_2_share * tcf_2) / 100))]
tcf_average[is.na(tcf) & is.na(tcf_1), `:=`(tcf = tcf_2)]
tcf_average[is.na(tcf) & is.na(tcf_2), `:=`(tcf = tcf_1)]
tcf_average[is.na(c01_1_share) & is.na(c01_2_share) & !is.na(tcf_1) & !is.na(tcf_2),
            `:=`(tcf = (tcf_1 + tcf_2)/2)]
tcf_average[c01_1_share == "0" & c01_2_share == "0" & !is.na(tcf_1) & !is.na(tcf_2),
            `:=`(tcf = (tcf_1 + tcf_2)/2)]

tcf_average[, `:=`(c01_1_share = NULL, c01_2_share = NULL, tcf_1 = NULL, tcf_2 = NULL, 
                   subcom_code = NULL, subitem = NULL, source_code = NULL, source = NULL,
                   literature = NULL, description = NULL)]
tcf_average <- tcf_average[order(tcf_average$area_code),]

rm(share_irwc, tcf_1, tcf_2)

# Recalculate continental averages
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
tcf_cont <- tcf_cont[ area != 'XXX']
tcf_cont <- tcf_cont[ area != 'ROW']
tcf_cont <- tcf_cont[, "continent":= NA]
tcf_cont <- tcf_cont[, "area_code":= NA]
tcf_average <- tcf_average[!is.na(continent)]
tcf_average <- rbind(tcf_average, tcf_cont, fill=TRUE)

# Apply continental average tcf where no country-specific value available
tcf_average <- merge(tcf_average, tcf_cont[, .(continent = area, com_code, tcf_type, c_tcf = tcf)],
                     by = c("continent", "com_code", "tcf_type"), all.x = TRUE)
tcf_average[is.na(tcf), `:=`(tcf = c_tcf)]
tcf_average[, `:=`(c_tcf = NULL)]

# Apply world average tcf where no continental average available
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

# Update tcf_prep
tcf_average <- tcf_average[, `:=`(description = NA, literature = NA,
                                  subcom_code = NA, subitem = NA, source = NA, source_code = NA)]
tcf_prep <-tcf_prep[com_code != "c01",]
tcf_prep <- rbind(tcf_prep, tcf_average)

rm(tcf_average)


cat("\nNow calculate weighted averages for c02 industrial roundwood, non-coniferous.\n")

# Read file
share_irwnc <- fread("inst/share_irwnc.csv")

# Prepare structure of table
tcf_average <- tcf_prep[subcom_code == "c02_1"]
tcf_average[, `:=` (subcom_code = NA, subitem = NA, source_code = NA, source = NA, tcf = NA)]
tcf_average <- merge(tcf_average,
                     share_irwnc[, .(area_code, area, com_code, item, c02_1_share, c02_2_share)],
                     by = c("area_code", "area", "com_code", "item"),all.x = TRUE)

tcf_1 <- tcf_prep[subcom_code == "c02_1"]
tcf_1 <- tcf_1 %>%
  rename(tcf_1 = tcf)
tcf_2 <- tcf_prep[subcom_code == "c02_2"]
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

# Calculation of (weighted) averages
tcf_average[!is.na(tcf_1) & !is.na(tcf_2)
            , `:=`(tcf = ((c02_1_share * tcf_1 + c02_2_share * tcf_2) / (c02_1_share + c02_2_share)))]
tcf_average[is.na(tcf_1), `:=`(tcf = tcf_2)]
tcf_average[is.na(tcf_2), `:=`(tcf = tcf_1)]
tcf_average[is.na(c02_1_share) & is.na(c02_2_share) & !is.na(tcf_1) & !is.na(tcf_2),
            `:=`(tcf = (tcf_1 + tcf_2)/2)]
tcf_average[, `:=`(c02_1_share = NULL, c02_2_share = NULL, tcf_1 = NULL, tcf_2 = NULL, 
                   subcom_code = NULL, subitem = NULL, source_code = NULL, source = NULL,
                   literature = NULL, description = NULL)]
tcf_average <- tcf_average[order(tcf_average$area_code),]

rm(share_irwnc, tcf_1, tcf_2)

# Recalculate continental averages
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
tcf_cont <- tcf_cont[ area != 'XXX']
tcf_cont <- tcf_cont[ area != 'ROW']
tcf_cont <- tcf_cont[, "continent":= NA]
tcf_cont <- tcf_cont[, "area_code":= NA]
tcf_average <- tcf_average[!is.na(continent)]
tcf_average <- rbind(tcf_average, tcf_cont, fill=TRUE)

# Apply continental average tcf where no country-specific value available
tcf_average <- merge(tcf_average, tcf_cont[, .(continent = area, com_code, tcf_type, c_tcf = tcf)],
                     by = c("continent", "com_code", "tcf_type"), all.x = TRUE)
tcf_average[is.na(tcf), `:=`(tcf = c_tcf)]
tcf_average[, `:=`(c_tcf = NULL)]

# Apply world average tcf where no continental average available
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

# Update tcf_prep
tcf_average <- tcf_average[, `:=`(description = NA, literature = NA, subcom_code = NA, subitem = NA, source = NA, source_code = NA)]
tcf_prep <-tcf_prep[com_code != "c02"]
tcf_prep <- rbind(tcf_prep, tcf_average)
rm(tcf_average)


cat("\nCalculate weighted averages for c08 fibreboard.\n")

# Read file
share_fb <- fread("inst/share_fb.csv")

# Preparing structure of table
tcf_average <- tcf_prep[subcom_code == "c08_1"]
tcf_average[, `:=` (subcom_code = NA, subitem = NA, source_code = NA, source = NA, tcf = NA)]
tcf_average <- merge(tcf_average,
                     share_fb[, .(area_code, area, com_code, item, c08_1_share, c08_2_share, c08_3_share)],
                     by = c("area_code", "area", "com_code", "item"),all.x = TRUE)

tcf_1 <- tcf_prep[subcom_code == "c08_1"]
tcf_1 <- tcf_1 %>%
  rename(tcf_1 = tcf)
tcf_2 <- tcf_prep[subcom_code == "c08_2"]
tcf_2 <- tcf_2 %>%
  rename(tcf_2 = tcf)
tcf_3 <- tcf_prep[subcom_code == "c08_3"]
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

# Calculation of (weighted) averages
tcf_average[, `:=`(tcf = (c08_1_share * tcf_1 + c08_2_share * tcf_2 + c08_3_share * tcf_3)
                   / (c08_1_share + c08_2_share + c08_3_share))]
tcf_average[is.na(tcf) & is.na(tcf_2) & is.na(tcf_3), `:=`(tcf = tcf_1)]
tcf_average[is.na(tcf) & is.na(tcf_1) & is.na(tcf_3), `:=`(tcf = tcf_2)]
tcf_average[is.na(tcf) & is.na(tcf_1) & is.na(tcf_2), `:=`(tcf = tcf_3)]
tcf_average[is.na(tcf) & is.na(tcf_1) & is.na(tcf_2) & is.na(tcf_3), `:=`(tcf = NA)]
tcf_average[is.na(c08_1_share) & is.na(c08_2_share) & is.na(c08_3_share) & 
              !is.na(tcf_1) & !is.na(tcf_2) & !is.na(tcf_3),
            `:=`(tcf = (tcf_1 + tcf_2 + tcf_3)/3)]
tcf_average[is.na(c08_1_share) & is.na(c08_2_share) & is.na(c08_3_share) &
             !is.na(tcf_1) & !is.na(tcf_2),
           `:=`(tcf = (tcf_1 + tcf_2)/2)]
tcf_average[is.na (tcf) & !is.na(c08_1_share) & !is.na(c08_2_share) & !is.na(c08_3_share) & is.na(tcf_1),
            `:=`(tcf = (c08_2_share * tcf_2 + c08_3_share * tcf_3)/(c08_2_share + c08_3_share))]
tcf_average[is.na (tcf) & !is.na(c08_1_share) & !is.na(c08_2_share) & !is.na(c08_3_share) & is.na(tcf_2),
            `:=`(tcf = (c08_1_share * tcf_1 + c08_3_share * tcf_3)/(c08_1_share + c08_3_share))]
tcf_average[is.na (tcf) & !is.na(c08_1_share) & !is.na(c08_2_share) & !is.na(c08_3_share) & is.na(tcf_3),
            `:=`(tcf = (c08_1_share * tcf_1 + c08_2_share * tcf_2)/(c08_1_share + c08_2_share))]

tcf_average[, `:=`(c08_1_share = NULL, c08_2_share = NULL, c08_3_share = NULL, tcf_1 = NULL, tcf_2 = NULL, tcf_3 = NULL,
                   subcom_code = NULL, subitem = NULL, source_code = NULL, source = NULL,
                   literature = NULL, description = NULL)]
tcf_average <- tcf_average[order(tcf_average$area_code),]

rm(share_fb, tcf_1, tcf_2, tcf_3)

# Recalculate continental averages
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
tcf_cont <- tcf_cont[ area != 'XXX']
tcf_cont <- tcf_cont[ area != 'ROW']
tcf_cont <- tcf_cont[, "continent":= NA]
tcf_cont <- tcf_cont[, "area_code":= NA]
tcf_average <- tcf_average[!is.na(continent)]
tcf_average <- rbind(tcf_average, tcf_cont, fill=TRUE)

# Apply continental average tcf where no country-specific value available
tcf_average <- merge(tcf_average, tcf_cont[, .(continent = area, com_code, tcf_type, c_tcf = tcf)],
                     by = c("continent", "com_code", "tcf_type"), all.x = TRUE)
tcf_average[is.na(tcf), `:=`(tcf = c_tcf)]
tcf_average[, `:=`(c_tcf = NULL)]

# Apply world average tcf where no continental average available
tcf_wrld <- tcf_cont %>% 
  group_by(com_code, tcf_type) %>% 
  summarize(w_tcf = mean(tcf, na.rm = TRUE)) %>% 
  ungroup()
tcf_average <- merge(tcf_average, tcf_wrld,
                     by = c("com_code", "tcf_type"), all.x = TRUE)
tcf_average[is.na(tcf), `:=`(tcf = w_tcf)]
tcf_average[, `:=`(w_tcf = NULL, continent = NULL)]
rm(tcf_cont, tcf_wrld)

# Unite with rest of tcf_prep, replacing old tcf
tcf_average <- tcf_average[, `:=`(description = NA, literature = NA, subcom_code = NA, subitem = NA, source = NA, source_code = NA)]
tcf_prep <-tcf_prep[com_code !="c08"]
tcf_prep <- rbind(tcf_prep, tcf_average)

rm(tcf_average)


cat("\nBuilding average for c11b.\n")
# Build average of wood pulp mechanical and wood pulp semi-chemical
# Not weighted because no great difference

# Preparing structure of table
tcf_average <- tcf_prep[com_code == "c11b"]
tcf_average[, `:=` (subcom_code = NA, subitem = NA, source_code = NA, source = NA, tcf = NA)]

tcf_1 <- tcf_prep[subcom_code == "c11b_1"]
tcf_1 <- tcf_1 %>%
  rename(tcf_1 = tcf)
tcf_2 <- tcf_prep[subcom_code == "c11b_2"]
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

# Calculation of average
tcf_average[!is.na(tcf_1) & !is.na(tcf_2), `:=`(tcf = na_sum(tcf_1, tcf_2) / 2)]
tcf_average[!is.na(tcf_1) & is.na(tcf_2), `:=`(tcf = tcf_1)]
tcf_average[!is.na(tcf_2) & is.na(tcf_1), `:=`(tcf = tcf_2)]

rm(tcf_1, tcf_2)

# Recalculate continental averages
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
tcf_cont <- tcf_cont[ area != 'XXX']
tcf_cont <- tcf_cont[ area != 'ROW']
tcf_cont <- tcf_cont[, "continent":= NA]
tcf_cont <- tcf_cont[, "area_code":= NA]
tcf_average <- rbind(tcf_average, tcf_cont, fill=TRUE)

# Apply continental average tcf where no country-specific value available
tcf_average <- merge(tcf_average, tcf_cont[, .(continent = area, com_code, tcf_type, c_tcf = tcf)],
                     by = c("continent", "com_code", "tcf_type"), all.x = TRUE)
tcf_average[is.na(tcf), `:=`(tcf = c_tcf)]
tcf_average[, `:=`(c_tcf = NULL)]

# Apply world average tcf where no continental average available
tcf_wrld <- tcf_cont %>% 
  group_by(com_code, tcf_type) %>% 
  summarize(w_tcf = mean(tcf, na.rm = TRUE)) %>% 
  ungroup()
tcf_average <- merge(tcf_average, tcf_wrld,
                     by = c("com_code", "tcf_type"), all.x = TRUE)
tcf_average[is.na(tcf), `:=`(tcf = w_tcf)]
tcf_average[, `:=`(w_tcf = NULL, continent = NULL)]
rm(tcf_cont, tcf_wrld)

# Unite with rest of tcf_prep, replacing old tcf
tcf_average <- tcf_average[, `:=`(description = NA, literature = NA, subcom_code = NA, subitem = NA, source = NA, source_code = NA)]
tcf_prep <-tcf_prep[com_code !="c08"]
tcf_prep <- rbind(tcf_prep, tcf_average)

rm(tcf_average)


# I STAYED HERE #

cat("\nBuilding averages for c13.\n")
## c13 recovered fibre pulp
# build average of all tcf of all wood pulp multiplied by 'recovered paper(input to output)'

cat("\nBuilding tcf for c17.\n")
# c17 chips
# build  "oven-dry tonne/m³ loose"
# or odmt/m3p for tcf_carbon
# by: 1/(green swe to oven-dry tonne)/(Oven-dry tonne/m³ loose)
# be careful with unit (which will later be turn around or not, in the Use Script)

# The following should be done more elegantly (with region)
#tcf_average <- tcf_average[continent != 'XXX']
# this last line delets the tcf_cont rows that we just added with rbind
# to do: find out other way to delete "XXX" rows
# update: why deleting "XXX"???


# Outcome should be a
# mb_sup_prep.csv
# tcf_use_prep.csv
# tcf_density_prep.csv
# wood_wbp_prep.csv
# shrinkage.csv 

# TCF USE ---------------------------------------------------------------------

cat("\nTidying tcf_use for use tables.\n")

use <- fread("inst/tcf_use.csv")
use[tcf == 0, tcf := NA]

use_cont <- use[is.na(area_code)]
use <- merge(use, regions[, .(area_code, continent)],
  by = "area_code")
use[continent == "EU", continent := "EUR"]

# calculate world averages
use_wrld <- use_cont %>% 
  group_by(com_code, source_code, unit) %>% 
  summarize(w_tcf = mean(tcf, na.rm = TRUE)) %>% 
  ungroup()

# apply continental average tcf where no country-specific value available
use <- merge(use, use_cont[, .(continent = area, com_code, source_code, unit, c_tcf = tcf)], 
             by = c("continent", "com_code", "source_code", "unit"), all.x = TRUE)

# apply world average tcf where no continental average available
use <- merge(use, use_wrld,
  by = c("com_code", "source_code", "unit"), all.x = TRUE)

use[, `:=`(tcf = ifelse(!is.na(tcf), tcf, 
  ifelse(!is.na(c_tcf), c_tcf, w_tcf)), 
  c_tcf = NULL, w_tcf = NULL)]


use <- use[, c("continent", "area_code", "area", "com_code","item","source_code","source","unit","tcf")]

fwrite(use, "inst/tcf_use_tidy.csv")


# TCF CARBON ------------------------------------------------------------------------------

cat("\nTidying different technical conversion factors to calculate carbon TCF.\n")
# tcf in c01, c02, c03, c17 and c18 already refer to the basic density
# that is (oven) dry weight to green volumen (c01-c03) or m3 product (c17,c18)

## step 1
cat("\nStep 1: Tidying density-related factors.\n")

# upload density-related factors
density <- fread("inst/tcf_density.csv")

density[tcf == 0, tcf :=NA]

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

# TCF swe ---------------------------------------------------------------------

#cat("\nTidying swe technical conversion factors.\n")

# provisorisch:
# used "tcf_use_tidy" as basis
# divided 1/tcf for c17 and c18 to obtain the right unit (m3sw/m3p)
# I used this: tcf_in[unit=="m3rw/m3p", `:=`(tcf = 1 / tcf, unit = "m3p/m3rw")]
# calculated mean of tcf from c04-c10 and multiplied by 2 (average of m3p/tonne) to obtain tcf for c19
# divided tcf of c11a to obtain tcf for c20