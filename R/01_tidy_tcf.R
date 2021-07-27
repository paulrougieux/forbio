
library("data.table")
library("tidyverse")
source("R/01_tidy_functions.R")

regions <- fread("inst/regions.csv")
products <- fread("inst/products.csv")


# MB SUP ----------------------------------------------------------------

cat("\nTidying material balances for supply tables.\n")

sup <- fread("inst/mb_sup.csv")
sup_cont <- sup[is.na(area_code)]
sup <- merge(sup, regions[, .(area_code, continent)],
  by = "area_code")
sup[continent == "EU", continent := "EUR"]

# apply continental average MB where no country-specific value available
sup <- merge(sup, sup_cont[, .(continent = area, proc_code, 
  c_product = product, c_chips = chips, c_residues = residues)],
  by = c("continent", "proc_code"), all.x = TRUE)
sup[, c_losses := 100 - (c_product + c_chips + c_residues)]
sup[!is.na(product) & is.na(chips), 
    `:=`(chips = (100 - product - c_losses) / (c_chips + c_residues) * c_chips,
         residues = (100 - product - c_losses) / (c_chips + c_residues) * c_residues)]
sup[is.na(product), 
    `:=`(product = c_product, chips = c_chips, residues = c_residues)]
sup[, `:=`(c_product = NULL, c_chips = NULL, c_residues = NULL, c_losses = NULL)]

# apply world average MB where no continental average available
sup_wrld <- sup_cont %>% 
  group_by(proc_code) %>% 
  summarize(w_product = mean(product, na.rm = TRUE),
            w_chips = mean(chips, na.rm = TRUE),
            w_residues = mean(residues, na.rm = TRUE)) %>% 
  ungroup()
sup <- merge(sup, sup_wrld,
  by = c("proc_code"), all.x = TRUE)
sup[is.na(product), 
    `:=`(product = w_product, chips = w_chips, residues = w_residues)]
sup[, `:=`(w_product = NULL, w_chips = NULL, w_residues = NULL)]

# add world average MB for RoW
row <- sup[area_code == 252]
row[, `:=`(area = "RoW", area_code = 999)]
sup <- rbindlist(list(sup, row))


# shift 20% of chips to product and residues
# because chips actually includes peeler cores, etc.
sup[proc_code %in% c("p06", "p07"), 
    `:=`(product = product + chips * 0.2, residues = residues + chips * 0.2, chips = chips * 0.6)]


# convert into percentages ignoring losses
sup[, total := product + chips + residues]
sup[, `:=`(product = product / total,
           chips = chips / total,
           residues = residues / total,
           total = NULL)]

fwrite(sup, "inst/mb_sup_tidy.csv")
rm(sup, sup_cont, sup_wrld, row)



# TCF USE ----------------------------------------------------------------

cat("\nTidying technical conversion factors for use tables.\n")

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

# add world average MB for RoW
row <- use[area_code == 252]
row[, `:=`(area = "RoW", area_code = 999)]
use <- rbindlist(list(use, row))


use <- use[, c("continent", "area_code", "area", "com_code","item","source_code","source","unit","tcf")]

fwrite(use, "inst/tcf_use_tidy.csv")
