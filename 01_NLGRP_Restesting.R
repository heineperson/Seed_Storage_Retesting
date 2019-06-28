# Set Up
library(data.table)
library(httr)
library(RCurl)
library(rjson)
library(jsonlite)
library(DT)

# Reading in Caspio Token
source("Tokens/caspioSource.R")
# Reading in caspio GET command (don't know if this works for things > 1000)
source("caspioFunctions.R")

# Reading in NLGRP table directly from Caspio
NLGRP <- caspio_get_table("MTRA_Accessions",login1)

# Getting more than 1000 rows! This is for the "LimitedRarePlantsView' that combines the Rare Plant Table and the NatureServe Table
tableNew <- caspio_get_view("LimitedRarePlantsView",login1,1)
TableCombined = NULL
pagenumber=1

while (dim(tableNew)[1]==1000)
{
  pagenumber = pagenumber + 1
  tableNew <- caspio_get_view("LimitedRarePlantsView",login1,pagenumber)
  TableCombined <- rbind(TableCombined,tableNew)
}


#