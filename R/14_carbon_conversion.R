
library("data.table")
library("Matrix")
library(tidyverse)
source("R/01_tidy_functions.R")

regions <- fread("inst/regions.csv")

sup <- readRDS("data/sup_final.rds")

cbs <- readRDS("data/cbs_final.rds")

use <- readRDS("data/use_final.rds")
use_fd <- readRDS("data/use_fd_final.rds")

years <- seq(1997, 2017)

#---------------------------------------
# GLOBAL RESULTS (2016) in tC
#---------------------------------------

# Upload data #

carbon_cf <- fread("inst/carbon_tidy_2.csv")

yr <- 2016

cbs <- cbs[year==yr]
sup <- sup[year==yr]
use <- use[year==yr]
use_fd <- use_fd[year==yr]


# Merge of carbon tcf into each dataset #

sup_carbon <- merge(sup, carbon_cf[, .(area_code, com_code, tcf_carbon)],
                    all.x = TRUE, by = c("com_code", "area_code"))

use_carbon <- merge(use, carbon_cf[, .(area_code, com_code, tcf_carbon)],
                    all.x = TRUE, by = c("com_code", "area_code"))

fd_carbon <- merge(use_fd, carbon_cf[, .(area_code, com_code, tcf_carbon)],
                    all.x = TRUE, by = c("com_code", "area_code"))

# cbs_carbon <- merge(cbs, carbon_cf[, .(area_code, com_code, tcf_carbon)],
#                     all.x = TRUE, by = c("com_code", "area_code"))


# Conversion of each dataset into tC #

sup_carbon[, production_tC := production * tcf_carbon]
sup_carbon[, `:=`(tcf_carbon = NULL, production = NULL)]

use_carbon[, use_tC := use * tcf_carbon]
use_carbon[, `:=`(tcf_carbon = NULL, use = NULL)]

fd_carbon[, mat_use_tC := material_use * tcf_carbon]
fd_carbon[, energy_use_tC := energy_use * tcf_carbon]
fd_carbon[, `:=`(tcf_carbon = NULL, material_use = NULL, energy_use = NULL)]


# Save data sets #

# saveRDS(sup_carbon, "data/sup_final_2016_tC.rds")
# saveRDS(use_carbon, "data/use_final_2016_tC.rds")
# saveRDS(fd_carbon, "data/use_fd_final_2016_tC.rds")


# Aggregation of all countries #

sup_total <- aggregate(x = sup_carbon$production_tC, by = list(sup_carbon$com_code, sup_carbon$proc_code), FUN = na_sum)

use_total <- aggregate(x = use_carbon$use_tC, by = list(use_carbon$com_code, use_carbon$proc_code), FUN = na_sum)

fd_mat_total <- aggregate(x = fd_carbon$mat_use_tC, by = list(fd_carbon$com_code), FUN = na_sum)

fd_ene_total <- aggregate(x = fd_carbon$energy_use_tC, by = list(fd_carbon$com_code), FUN = na_sum)


# Rename and save SUP #

sup_total <- sup_total %>%
                rename(
                com_code = Group.1,
                proc_code = Group.2,
                value = x
                )

sup_total$MtC <- (sup_total$value / 1000000)
sup_table <- spread(sup_total, com_code, MtC, fill = NA)

write.csv(sup_table,"./output/sup_table_2016.csv", row.names = FALSE)


# Rename and save USE #

use_total <- use_total %>%
                rename(
                  com_code = Group.1,
                  proc_code = Group.2,
                  value = x
                 )

use_total$MtC <- (use_total$value / 1000000)
use_table <- spread(use_total, com_code, MtC, fill = NA)

write.csv(use_table,"./output/use_table_2016.csv", row.names = FALSE)


# Rename and save FD energy use

fd_ene_total <- fd_ene_total %>%
  rename(
    com_code = Group.1,
    value = x
  )

fd_ene_total$MtC <- (fd_ene_total$value / 1000000)
# fd_ene_table <- spread(fd_ene_total, com_code, MtC, fill = NA)

write.csv(fd_ene_total,"./output/fd_ene_2016.csv", row.names = FALSE)

# Rename and save FD material use 

fd_mat_total <- fd_mat_total %>%
  rename(
    com_code = Group.1,
    value = x
  )

fd_mat_total$MtC <- (fd_mat_total$value / 1000000)

# fd_mat_table <- spread(fd_mat_total, com_code, MtC, fill = NA)

write.csv(fd_mat_total,"./output/fd_mat_2016.csv", row.names = FALSE)



#---------------------------------------
#GLOBAL RESULTS (2017) in m3 swe
#---------------------------------------

# tcf <- fread("inst/tcf_use_tidy.csv")
# 
# yr <- 2017
# 
# cbs <- cbs[year==yr]
# sup <- sup[year==yr]
# use <- use[year==yr]
# 
# sup_totals <- aggregate(x = sup$production, by = list(sup$com_code, sup$proc_code), FUN = na_sum)
# 
# use_totals <- aggregate(x = use$use, by = list(use$com_code, use$proc_code), FUN = na_sum)
# 
# # Convert into m3 swe (SUP)
# 
# sup_totals <- sup_totals %>%
#   rename(
#     com_code = Group.1,
#     proc_code = Group.2,
#     value = x
#   )
# 
# sup_totals <- merge(sup_totals, tcf[, .(com_code, tCdunit)],
#                     all.x = TRUE, by = "com_code")
# 
# sup_totals$value_totals <- sup_totals$value * sup_totals$tCdunit
# 
# # Convert into m3 swe (USE)
# 
# use_totals <- use_totals %>%
#   rename(
#     com_code = Group.1,
#     proc_code = Group.2,
#     value = x
#   )
# 
# use_totals <- merge(use_totals, carbon_cf[, .(com_code, tCdunit)],
#                     all.x = TRUE, by = "com_code")
# 
# use_totals$value_totals <- use_totals$value * use_totals$tCdunit




#---------------------------------------
# DATA CHECKS
#---------------------------------------

regions <- regions[baci==TRUE]
regions <- regions[order(area_code)]

items <- fread("inst/products.csv")
areas <- sort(unique(cbs$area_code))
processes <- sort(unique(sup$proc_code))
commodities <- sort(unique(sup$com_code))

btd <- readRDS("data/btd_final.rds")

mr_sup <- readRDS("data/mr_sup.rds")
mr_use <- readRDS("data/mr_use.rds")

yr <- 2017
country <- 41 # China
com_codes <- c("c01","c02")

cbs[area_code==country & year==yr & com_code %in% com_codes]
sup[area_code==country & year==yr & com_code %in% com_codes]
use[area_code==country & year==yr & com_code %in% com_codes]

reg <- which(regions$area_code==country)
y_use <- as.matrix(mr_use[[as.character(yr)]])
y_sup <- as.matrix(mr_sup[[as.character(yr)]])
dim(y_use) / 222
dim(y_sup) / 222
dim(Z) / 222

usedom <- y_use[(23*(reg-1)+1):(23*reg),(21*(reg-1)+1):(21*reg)]
supdom <- y_sup[(21*(reg-1)+1):(21*reg),(23*(reg-1)+1):(23*reg)]

