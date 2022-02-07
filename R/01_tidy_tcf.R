
library("data.table")
library("tidyverse")
source("R/01_tidy_functions.R")

regions <- fread("inst/regions.csv")
products <- fread("inst/products.csv")

# Preparation ----------------------------------------------------------

cat("\nAggregation of (sub)items and building (weighted) averages.\n")

## c03 Wood fuel: weighted average of c/nc wood basic density

# read files
density <- fread("inst/tcf_density_raw.csv")
woodfuel_den <- density[com_code == 'c03']
woodfuel_cnc <- fread("inst/woodfuel_cnc.csv")

# create separate tables for c and nc respectively
tcf_nc <- woodfuel_den[source == 'Roundwood, non-coniferous']
tcf_nc <- tcf_nc %>%
  rename(tcf_nc = tcf)

tcf_c <- woodfuel_den[source == 'Roundwood, coniferous']
tcf_c <- tcf_c %>%
  rename(tcf_c = tcf)

# Calculation of weighted average of tcf
woodfuel <- merge(tcf_c, tcf_nc[, .(area_code, area, tcf_nc)],
                  by = c("area_code","area"), all.x = TRUE)
woodfuel <- merge(woodfuel, woodfuel_cnc[, .(area_code, area, c_share, nc_share)],
                  by = c("area_code","area"), all.x = TRUE)
woodfuel[, `:=`(tcf = c_share * tcf_c + nc_share * tcf_nc)]
woodfuel[, `:=`(c_share = NULL, tcf_c = NULL, nc_share = NULL, tcf_nc = NULL)]
woodfuel$source <- NA

woodfuel <- woodfuel[order(woodfuel$area_code),]

# Replace tcf
density <- density[!(density$com_code=="c03"),]
density <- rbind(density, woodfuel)
density <- density[order(density$com_code),]

rm(woodfuel_den, tcf_c, tcf_nc, woodfuel_cnc, woodfuel)

# ATTENTION: CHECK IF CONTINENTAL NUMBERS HAVE CHANGED ? PROBABLY NOT
# MAYBE I NEED TO BUILD THEM AGAIN BASED ON THE JUST CALCULATED WEIGHTED AVERAGE

## c06 veneer & c07 plywood: build c/nc weighted average

# read files
veneerplywood_cnc <- fread("inst/veneerplywood_cnc.csv")

veneerplywood_den <- density[com_code == c("c06","c07")]


# # create separate tables for c and nc respectively
# tcf_nc <- woodfuel_den[source == 'Roundwood, non-coniferous']
# tcf_nc <- tcf_nc %>%
#   rename(tcf_nc = tcf)
# 
# tcf_c <- woodfuel_den[source == 'Roundwood, coniferous']
# tcf_c <- tcf_c %>%
#   rename(tcf_c = tcf)
# 
# # Calculation of weighted average of tcf
# woodfuel <- merge(tcf_c, tcf_nc[, .(area_code, area, tcf_nc)],
#                   by = c("area_code","area"), all.x = TRUE)
# woodfuel <- merge(woodfuel, woodfuel_cnc[, .(area_code, area, c_share, nc_share)],
#                   by = c("area_code","area"), all.x = TRUE)
# woodfuel[, `:=`(tcf = c_share * tcf_c + nc_share * tcf_nc)]
# woodfuel[, `:=`(c_share = NULL, tcf_c = NULL, nc_share = NULL, tcf_nc = NULL)]
# woodfuel$source <- NA
# 
# woodfuel <- woodfuel[order(woodfuel$area_code),]
# 
# # Replace tcf
# density <- density[!(density$com_code=="c03"),]
# density <- rbind(density, woodfuel)
# density <- density[order(density$com_code),]
# 
# rm(woodfuel_den, tcf_c, tcf_nc, woodfuel_cnc, woodfuel)






## c08 fibreboard
# read files
# this file does not exist yet
fibreboard_subcom <- fread("inst/fibreboard_subcom.csv")

## c11a wood pulp mechanical/semi-chemical
# build average
# or if single number, then use this number


## c13 recovered fibre pulp
# build average of all tcf of all wood pulp multiplied by 'recovered paper(input to output)'


# c15 pellets & agglomerates
# build average of both ("solid wood m3 per tonne product")
# and if single number, use that number

# c17 chips
# build "oven-dry tonne/m³ loose"
# by: 1/(green swe to oven-dry tonne)/(Oven-dry tonne/m³ loose)
# be careful with unit (which will later be turn around or not, in the Use Script)

# c18 wood residues
# select "sawdust" and "shavings"
# build "oven-dry tonne/ m³ loose"
# by: 1/(green swe to oven-dry tonne)/(Oven-dry tonne/m³ loose)
# build category "wood residues"
# build average 




# Outcome should be a
# mb_sup_prep.csv
# tcf_use_prep.csv
# tcf_density_prep.csv
# wood_wbp_prep.csv
# shrinkage.csv 

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
rm(sup, sup_cont, sup_wrld)



# TCF USE ---------------------------------------------------------------------

cat("\nTidying input-output technical conversion factors for use tables.\n")

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


# Carbon TCF -------------------------------------------------------------------------

cat("\nTidying different technical conversion factors to calculate carbon TCF.\n")
# tcf in c01, c02, c03, c17 and c18 already refer to the basic density
# that is (oven) dry weight to green volumen (c01-c03) or m3 product (c17,c18)

## step 1
cat("\nStep 1: Tidying density-related factors.\n")

# upload density-related factors
density <- fread("inst/tcf_density.csv")
density <- fread ("inst/tcf_density_correctedbyhand.csv")

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