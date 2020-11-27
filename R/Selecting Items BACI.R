## Path to files ##

#setwd ("W://WU/Projekte/GRU/04_Daten/Socio-Economic data/BACI") #Dataset is under this path in the WU system

setwd ("C://Users/ZRosadio/Documents/WU R") #This is where I saved it locally


## Unzip and upload main files ##

rm(list=ls())

unzip ("C://Users/ZRosadio/Documents/WU R/BACI_HS07_V202001.zip", files = NULL, list = FALSE, overwrite = TRUE,
      junkpaths = FALSE, exdir = ".", unzip = "internal",
      setTimes = FALSE)
      
HS07_2007 <- read.csv("BACI_HS07_Y2007_V202001.csv") #Example year 2007, repeat for other years

View(HS07_2007)


## Upload product and country code files ##

Product <- read.csv("product_codes_HS07_V202001.csv")

View(Product)

Country <- read.csv("country_codes_V202001.csv")

View(Country)

## Select one item or more  ##

library(base)

item_coal1 <- subset(HS07_2007, k == 270111) #Example item 270111,"Coal: anthracite, whether or not pulverised, but not agglomerated"

View(item_coal1)

item_coal_all <- subset(HS07_2007, k == 270111 | k == 270112 | k == 270119) #Three items "Coal..."

View(item_coal_all)


## Write new file ##

write.csv(item_coal1, "item_coal1_HS07_2007.csv")

write.csv(item_coal_all, "item_coal_all_HS07_2007.csv")
