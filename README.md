# ROcD-nGoM

5/23/2023

ROcD-nGoM: A River-Ocean Coupled Database for the Northern Gulf of Mexico

Bailey Armos and Shuang Zhang

Department of Oceanography, Texas A&M University

b.m.armos@gmail.com shuang-zhang@tamu.edu

Keywords: Gulf of Mexico, river chemistry, nutrients, discharge, ocean properties

Data sources include the Water Quality Portal, United States Geological Survey, Gulf of Mexico Coastal Ocean Observing System, and NASA’s OceanColor Web.

The WRTDS model from the EGRET R package (Hirsch & De Cicco, 2015) was used to estimate daily river chemical solute concentrations and fluxes. The R programming language was used to create this database.

Here is the link to the database: https://zenodo.org/records/10152141

This repository includes the R script used to mine, clean, run through the WRTDS model, and QA/QC the river chemistry, discharge, and flux data.

## Libraries needed to run the code
-rstudioapi
-data.table
-dataRetrieval
-httr
-lubridate
-ggplot2
-scales
-EGRET
-dplyr
-readxl
-PeriodicTable
-rapportools
