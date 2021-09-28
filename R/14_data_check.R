
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
# GLOBAL RESULTS (2017) in tC
#---------------------------------------

carbon_cf <- fread("inst/carbon_cf.csv")

yr <- 2017

cbs <- cbs[year==yr]
sup <- sup[year==yr]
use <- use[year==yr]
use_fd <- use_fd[year==yr]

sup_total <- aggregate(x = sup$production, by = list(sup$com_code, sup$proc_code), FUN = na_sum)

use_total <- aggregate(x = use$use, by = list(use$com_code, use$proc_code), FUN = na_sum)

use_fd_mat <- aggregate(x = use_fd$material_use, by = list(use_fd$com_code), FUN = na_sum)
use_fd_ene <- aggregate(x = use_fd$energy_use, by = list(use_fd$com_code), FUN = na_sum)

## Convert into tC (SUP) ##

sup_total <- sup_totals %>% 
                rename(
                com_code = Group.1,
                proc_code = Group.2,
                value = x
                )

sup_total <- merge(sup_total, carbon_cf[, .(com_code, tCdunit)],
                    all.x = TRUE, by = "com_code")

sup_total$value_total <- sup_total$value * sup_total$tCdunit
sup_total$value_total <- sup_total$value_total / 1000000

sup_table <- spread(sup_total, com_code, value_total, fill = NA)
sup_table <- sup_table[, -3]

write.csv(sup_table,"./output/sup_table_2017.csv", row.names = FALSE)

## Convert into tC (USE) ##

use_total <- use_total %>%
                rename(
                  com_code = Group.1,
                  proc_code = Group.2,
                  value = x
                 )

use_total <- merge(use_total, carbon_cf[, .(com_code, tCdunit)],
                    all.x = TRUE, by = "com_code")

use_total$value_total <- use_total$value * use_total$tCdunit
use_total$value_total <- use_total$value_total / 1000000

use_table <- spread(use_total, com_code, value_total, fill = NA)
use_table <- use_table[, -3]

write.csv(use_table,"./output/use_table_2017.csv", row.names = FALSE)


## Convert into tC (energy USE) ##

use_fd_ene <- use_fd_ene %>%
                      rename(
                      com_code = Group.1,
                      value = x
                      )
use_fd_ene <- merge(use_fd_ene, carbon_cf[, .(com_code, tCdunit)],
                    all.x = TRUE, by = "com_code")

use_fd_ene$value_total <- use_fd_ene$value * use_fd_ene$tCdunit
use_fd_ene$value_total <- use_fd_ene$value_total / 1000000

write.csv(use_fd_ene,"./output/use_fdene_2017.csv", row.names = FALSE)

## Convert into tC (material USE) ##

use_fd_mat <- use_fd_mat %>%
  rename(
    com_code = Group.1,
    value = x
  )
use_fd_mat <- merge(use_fd_mat, carbon_cf[, .(com_code, tCdunit)],
                    all.x = TRUE, by = "com_code")

use_fd_mat$value_total <- use_fd_mat$value * use_fd_mat$tCdunit
use_fd_mat$value_total <- use_fd_mat$value_total / 1000000

write.csv(use_fd_mat,"./output/use_fdmat_2017.csv", row.names = FALSE)

#---------------------------------------
# GLOBAL RESULTS (2017) in m3 swe
#---------------------------------------
# 
# 'tcf <- fread("inst/tcf_use_tidy.csv")
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
# 
# 


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



