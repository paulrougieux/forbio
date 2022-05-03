# The Forestry Biomass Input-Output database

FORBIO provides a set of multi-regional physical supply-use and input-output tables covering global forestry. The work is based on mostly freely available data from FAOSTAT and BACI.
FORBIO currently covers 221 countries/territories + RoW, 20 processes and 24 commodities for 1997-2017.

This reposity provices all scripts and auxiliary data used to generate the beta version of the FORBIO database.
The documentation is work in progress.

## Usage

### Dependencies

#### R
In order to run the script, you need the following software version `R`. The scripts also depends on the following R packages from CRAN:
```{r}
install.packages(c("data.table", "tidyverse", "Matrix", "mipfp", "ggplot2"))
```

#### git
You only need git installed if you want to contribute to the repository or clone it without having to download it manually.

#### Data sets 
Input data are from FAOSTAT Forestry Production and Trade, FAOSTAT Forestry Trade Flows and BACI. FAOSTAT has an open access while for BACI registration is needed. 

#### Auxiliary data
Folder "inst" contains auxiliary data in csv format. These data include tables of products, processes, regions, etc. to be used in the scripts.
Additionally, Forestry Conversion Factors mainly from FAO, ITTO and United Nations (2020) as well as feestock estimates are in "raw" state.
Files with "tidy" in their titles are outputs from scripts.

### Get the scripts 
To get the scripts, you can either

- download the source using the "Clone or download"-button above
- use `git clone https://github.com/fineprint-global/forbio`

### How to run
Scripts are saved in folder "R". This is the order in which scripts should be run:

- `00_prep_fao.R` for FAOSTAT Forestry Production and Trade data preparation
- `00_prep_function.R` for functions preparation
- `00_prep_trade.R` for FAOSTAT Forestry Trade and BACI data preparation

- `01_tidy_cf.R` for forestry conversion factors tidying
- `01_tidy_fao.R` for FAOSTAT Production and Trade data (from now on CBS) tidying
- `01_tidy_functions.R` for functions tidying
- `01_tidy_trade.R` for FAOSTAT Forestry Trade Flows and BACI data (from now on BTD) tidying

- `02_build_btd.R` for building BTD (biletaral trade data)
- `03_build_cbs.R` for building CBS (Commodity balance sheets)
- `04_estimate_btd.R` for building own estimations of BTD 
- `05_balance_btd.R` for balancing final BTD

- `06_supply.R` for building national supply tables including balancing CBS
- `07_re-exports.R` for calculating re-exports
- `08_use.R` for building national use tables including balancing CBS

- `09_mrsut.R` for trade-linking of national supply and use tables
- `10_mrio.R` for building multi-regional input-output tables
- `11_leontief_inverse.R` for applying Leontief inverse (footprint)
- `13_plot_countries.R` for visualizations of results

These scripts should be ignored (work in progress):
- `06_carbon_conversion.R`
- `12_extensions.R`

## Acknowledgement
This project gratefully acknowledges financial support from Austria Science Fund (FWF) as part of the [MF-Globe](https://www.wu.ac.at/mfglobe) project and the ERC as part of the [FINEPRINT](https://www.fineprint.global/) project.
