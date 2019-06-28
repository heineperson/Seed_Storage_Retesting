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

# Combining multiple tables
TableCombined <- caspio_get_table("tblSpeciesJepCAPR",login1,1)

pagenumber=1

while (dim(CaPRSpeciesNew)[1]==1000)
{
  pagenumber = pagenumber + 1
  CaPRSpeciesNew <- caspio_get_table("tblSpeciesJepCAPR",login1,pagenumber)
  TableCombined <- rbind(TableCombined,CaPRSpeciesNew)
}





# Set variable stock and price
stock <- 50
price <- 50

# Loop variable counts the number of loops 
loop <- 1

# Set the while statement
while (price > 45){
  
  # Create a random price between 40 and 60
  price <- stock + sample(-10:10, 1)
  
  # Count the number of loop
  loop = loop +1 
  
  # Print the number of loop
  print(loop)
}