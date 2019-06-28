#R version 3.6
library(gbm) #version 2.1.5
library(dismo) #version 1.1-4
library(jsonlite) #version 1.2
library(Taxonstand) #version 1.8
library(doParallel) #version 1.0.10
library(foreach) #version 1.4.3
library(rgdal)
library(raster)


#funciton to remove any trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)


#function to summarise data by species
summarise = function(X, name.col, data.cols, data, func = mean, na.rm = TRUE){
  colnames(data)[which(colnames(data)==name.col)] = "name"
  sub = subset(data, name == X, select = data.cols)
  output = apply(sub[, c(1:ncol(sub))], 2, FUN = func, na.rm = na.rm)
  return(output)
}

#funciton to calculate means of climate variables, excluding values outside 95% confidence limits
mean.inliers = function(X, lower = 0.025, upper = 0.975, na.rm = TRUE){
  if(length(X) >= 3){
    output = mean(X[X > quantile(X, c(lower, upper), na.rm = na.rm)[1] & X < quantile(X, c(lower, upper), na.rm = na.rm)[2]], na.rm = na.rm)
  } else{
    output = mean(X, na.rm = na.rm)
  }
  return(output)
}

#working directory
# wd =
# setwd(wd)

#read in the BRT models to predict from (part of supplementary material)
load("Wyse2018/brtGenus.Rdata")
load("Wyse2018/brtFamily.Rdata")
load("Wyse2018/brtOrder.Rdata")

#read in known data of species seed storage behaviour (available from SID)
##we advise running the species names from SID, Zanne dataset etc through the TPL function, or the online
##"Taxonomic names resolution service to ensure consistent taxonomy throughout
data.SID =
#read in seed mass data (available from SID)
seedweight =

#list of families in orders (I compiled this from the angiosperm phylogeny website: http://www.mobot.org/MOBOT/research/APweb/)
orders =

#woodiness dataset (from Zanne et al. 2014)
wood =

#dispersal dataset (available from SID)
disp =
#---------------------------------------------------------------------------------------------------------------------------
#read in a vector of species names
spp =
spp = trim.trailing(spp)

cl = makePSOCKcluster(16)
registerDoParallel(cl)
tpl.check = (foreach(i = 1:length(spp), .combine = rbind) %dopar% {
  tpl = TPL(spp[i])
  tpl.check = tpl
})
stopCluster(cl)


species = data.frame("Name_submitted" = spp, "Accepted_name" = paste(tpl.check$New.Genus, tpl.check$New.Species), "Genus" = tpl.check$New.Genus, "Family" = tpl.check$Family, stringsAsFactors = F)
#species not matched
not.found = species[which(tpl.check$Plant.Name.Index == FALSE),]

data = species[which(tpl.check$Plant.Name.Index == TRUE),]

#change a couple of family names
data[which(data$Family == "Asteraceae"), "Family"] = "Compositae"
data[which(data$Family == "Fabaceae"), "Family"] = "Leguminosae"

#get orders
for(n in 1:nrow(data)){
  if(nrow(orders[orders$Family %in% data[n, "Family"],]) == 1){
    ord.sub = orders[orders$Family == data[n, "Family"], "Order"]
    data[n, "Order"] = ord.sub
  }
}

#remove rows which don't match to an order
data = data[which(is.na(data$Order) == FALSE),]
rownames(data) = seq(1:nrow(data))

#change family to all caps
data$Family = toupper(data$Family)

#add columns
data = data.frame(data, thousandseedweight = rep(NA, nrow(data)), min.precip = rep(NA, nrow(data)), precip.dispersal = rep(NA, nrow(data)), tmin.dispersal = rep(NA, nrow(data)), annual.precip = rep(NA, nrow(data)), diurnal.temp.range = rep(NA, nrow(data)), mean.annual.temp = rep(NA, nrow(data)), alt = rep(NA, nrow(data)), woodiness = rep(NA, nrow(data)), tax.level = rep(NA, nrow(data)), storBehav = rep(NA, nrow(data)))

#pull out any known data about the species
knownSpp = data[data$Accepted_name %in% data.SID$Species, "Accepted_name"]
knownSpp = unique(knownSpp)

for(n in knownSpp){
  data[data$Accepted_name == n, c("thousandseedweight", "min.precip", "precip.dispersal", "tmin.dispersal", "annual.precip", "diurnal.temp.range", "mean.annual.temp", "alt", "storBehav")] = data.SID[data.SID$Species == n, c("thousandseedweight", "min.precip", "precip.dispersal", "tmin.dispersal", "annual.precip", "diurnal.temp.range", "mean.annual.temp", "alt", "storBehav")]
  data[data$Accepted_name == n,"tax.level"] = "Species"
}

knownRows = which(data$tax.level == "Species")
if(length(knownRows) == 0){
  knownRows = nrow(data)+1
}

#---------------------------------------------------------------------------------------------------------------------
#Seed weights
spp = paste(seedweight$genus, seedweight$spname, seedweight$infra, sep = " ")
spp = trim.trailing(spp)
seedweight$Species = spp
remove(spp)

SWspp = data[-knownRows, "Accepted_name"]
SWspp = SWspp[SWspp %in% seedweight$Species]

for(n in SWspp){
  weight = mean(seedweight[seedweight$Species == n, "thousandseedweight"])
  data[data$Accepted_name == n, "thousandseedweight"] = weight
}

#---------------------------------------------------------------------------------------------------------------------
#get porportions of recalcitrant species in genus/family/order per species based on available data
taxa.prop.binom = rep(NA, nrow(data))

for(n in as.numeric(rownames(data[-knownRows,]))){
  if(nrow(subset(data.SID, Genus == data[n, "Genus"] & Family == data[n, "Family"]))>0){
       taxa.prop.binom[n] = nrow(subset(data.SID, Genus == data[n, "Genus"] & Family == data[n, "Family"] & storBehav == "Recalcitrant"))/
        (nrow(subset(data.SID, Genus == data[n, "Genus"] & Family == data[n, "Family"])))


    data$tax.level[n] = "Genus"
  }


  if(is.na(taxa.prop.binom[n])){
    if(nrow(subset(data.SID, Family == data[n, "Family"]))>0){
      sub = subset(data.SID, Family == data[n, "Family"])
      gen = unique(sub$Genus)

      taxa.prop = mean(sapply(gen, FUN = function(X){nrow(subset(sub, Genus == X & storBehav == "Recalcitrant"))/nrow(subset(sub, Genus == X))}))

      taxa.prop.binom[n] = taxa.prop


      data$tax.level[n] = "Family"
    }
  }
  if(is.na(taxa.prop.binom[n])){
    if(nrow(subset(data.SID, Order == data[n, "Order"]))>0){
      sub = subset(data.SID, Order == data[n, "Order"])
      gen = unique(sub$Genus)

      taxa.prop = mean(sapply(gen, FUN = function(X){nrow(subset(sub, Genus == X & storBehav == "Recalcitrant"))/nrow(subset(sub, Genus == X))}))

      taxa.prop.binom[n] = taxa.prop


      data$tax.level[n] = "Order"
    }
  }
}

data = cbind(data, taxa.prop.binom)
remove(taxa.prop.binom)

#---------------------------------------------------------------------------------------------------------------------#woodiness

for(n in 1:nrow(data)){
  value = wood[wood$gs == data[n, "Accepted_name"], "woodiness"]
  if(length(value) == 1){
    data[n,"woodiness"] = value
    }
}

for(n in 1:nrow(data)){
  if(is.na(data[n, "woodiness"])){
    gen.vals = na.omit(data[data$Genus == data[n, "Genus"], "woodiness"])
    if(length(gen.vals)>0){
      data[n, "woodiness"] = names(table(gen.vals)[order(table(gen.vals), decreasing = T)])[1]
      }
  }
}

data$woodiness = as.factor(data$woodiness)

#---------------------------------------------------------------------------------------------------------------------
#dispersal

dispersal = rep(NA, nrow(data))
for(n in 1:nrow(data)){
  sub = subset(disp, Species == data[n, "Species"])
  if(nrow(sub) > 0){
    dispersal[n] = sub[1, "pdaid"]
  }

}

#impute dispersal values of congeners
for(n in 1:nrow(data)){
  if(is.na(dispersal[n])){
    gen.vals = subset(disp, Genus == data[n, "Genus"], select = "pdaid")[,1]
    if(length(gen.vals)>0){
      dispersal[n] = names(table(gen.vals)[order(table(gen.vals), decreasing = T)])[1]
    }
  }
}

#reduce the number of levels down
dispersal[which(dispersal == 12)] = 3
dispersal = as.character(dispersal)
dispersal[which(dispersal == 1)] = "Animal"
dispersal[which(dispersal == 2)] = "Wind"
dispersal[which(dispersal == 3)] = "Water"
dispersal[which(dispersal == 6)] = "Unassisted"
dispersal[which(dispersal %in% as.vector(na.omit(as.numeric(unique(dispersal)))))] = "Other"

data = cbind(data, dispersal)

data$dispersal = as.factor(data$dispersal)


#---------------------------------------------------------------------------------------------------------------------
#climate

##get gbif lat longs
spp = data[-knownRows, "Accepted_name"]


cl = makePSOCKcluster(32)
registerDoParallel(cl)

gbif.results = (foreach(i = 1:length(spp), .combine = rbind) %dopar% {
  library(dismo)
  library(jsonlite)
  genus = strsplit(spp[i], " ")[[1]][1]
  species = strsplit(spp[i], " ")[[1]][2]

  gbif.data = try(gbif(genus, species, geo = TRUE, removeZeros = TRUE, args = "/occurrence/search", ntries = 10, end = 100), silent = TRUE)

  if(is.data.frame(gbif.data)){
    if(!is.null(gbif.data$lat) & !is.null(gbif.data$lon)){
      if(!is.null(gbif.data$species)){
        gbif.data = na.omit(subset(gbif.data, basisOfRecord == "HUMAN_OBSERVATION" | basisOfRecord == "PRESERVED_SPECIMEN" | basisOfRecord == "LIVING_SPECIMEN" | basisOfRecord == "OBSERVATION", select = c("species", "basisOfRecord", "lat", "lon")))
      } else{
        species = rep(paste(genus, species), nrow(gbif.data))
        gbif.data = cbind(gbif.data, species)
        gbif.data = na.omit(subset(gbif.data, basisOfRecord == "HUMAN_OBSERVATION" | basisOfRecord == "PRESERVED_SPECIMEN" | basisOfRecord == "LIVING_SPECIMEN" | basisOfRecord == "OBSERVATION", select = c("species", "basisOfRecord", "lat", "lon")))
        gbif.data = unique(gbif.data)
      }
    } else {
      gbif.data = t(as.matrix(c(paste(genus, species), rep(NA, 3))))
      colnames(gbif.data) = c("species", "basisOfRecord", "lat", "lon")
    }
  } else{
    gbif.data = t(as.matrix(c(paste(genus, species), rep(NA, 3))))
    colnames(gbif.data) = c("species", "basisOfRecord", "lat", "lon")
  }


  gbif.results = gbif.data

})

stopCluster(cl)

gbif.results$lat = as.numeric(gbif.results$lat)
gbif.results$lon = as.numeric(gbif.results$lon)



#----------------------------------------------------------------------------------------------------
#get worldclim data
#download data from worldClim, saved in folders named as follows.
#working directory that worldclim folders are saved in:
wd_WC =

folders = c("precip_5min", "tmin_5min", "tmax_5min", "tmean_5min")

cl = makePSOCKcluster(4)
registerDoParallel(cl)
worldClim.results = foreach(i = folders, .combine = cbind) %dopar% {
  setwd(paste(wd_WC, i, sep = ""))
  files = list.files(pattern =  ".bil")
  files = files[c(1, 5:12, 2:4)]

  dataStack = raster(files[1])

  for(n in 2:length(files)){
    data.in = raster(files[n])
    dataStack = stack(dataStack, data.in)
  }

  output = extract(dataStack, as.matrix(gbif.results[,c("lon", "lat")]))
  if(i != folders[1]){
    output = output/10
  } else{
    output = output
  }

}
stopCluster(cl)

#calculate things
annual.precip = apply(worldClim.results[,1:12], 1, FUN = sum)
min.precip = apply(worldClim.results[,1:12], 1, FUN = min)
mean.annual.temp = apply(worldClim.results[,37:48], 1, FUN = mean)

diurnal.temp.range = vector(length = nrow(worldClim.results), mode = "numeric")
for(n in 1:nrow(worldClim.results)){
  range = worldClim.results[n,25:36]-worldClim.results[n,13:24]
  diurnal.temp.range[n] = mean(unlist(range))
}

gbif.results = cbind(gbif.results, annual.precip, min.precip, mean.annual.temp, diurnal.temp.range)
remove(annual.precip, min.precip, mean.annual.temp, diurnal.temp.range, worldClim.results)

#get altitude
setwd("C:/Users/sw23wk/Dropbox/Seed behaviour/WorldClim data/alt_5min")
files = list.files(pattern =  ".bil")
alt.data = raster(files)
alt = extract(alt.data, as.matrix(gbif.results[,c("lon", "lat")]))

gbif.results = cbind(gbif.results, alt)

#summarise by species - one mean value per species
spp = unique(gbif.results$species)
summaries = data.frame()
for(n in 1:length(spp)){
  summary = summarise(spp[n], name.col = "species", data.cols = c(5:9), data = gbif.results, func = mean.inliers)
  summaries = rbind(summaries, summary)
}
colnames(summaries) = names(summary)
worldClim.summary = data.frame(spp, summaries, stringsAsFactors = F)

#fill in the original dataframe
for(n in 1:nrow(worldClim.summary)){
  data[data$Accepted_name == worldClim.summary[n, "spp"], c("annual.precip", "min.precip", "mean.annual.temp", "diurnal.temp.range", "alt")] = worldClim.summary[n,2:6]
}

#---------------------------------------------------------------------------------------------------------------------
#make sure any NaNs are NAs
for(n in 1:ncol(data)){
  data[which(is.nan(data[,n])),n] = NA
}


#predict
data$tax.level = as.factor(data$tax.level)

data = data[which(!is.na(data$tax.level)),]

model.predictions = function(X){
  if(data[X, "tax.level"] == "Species"){
    prediction = NA
  }
  if(data[X, "tax.level"] == "Genus"){
    prediction = predict.gbm(brt.genus, newdata = data[X,],n.trees=3500,
                             type = "response")
  }
  if(data[X, "tax.level"] == "Family"){
    prediction = predict.gbm(brt.family, newdata = data[X,],n.trees=3500,
                             type = "response")
  }
  if(data[X, "tax.level"] == "Order"){
    prediction = predict.gbm(brt.order, newdata = data[X,],n.trees=3500,
                               type = "response")
  }
  return(prediction)
}

predictions = sapply(1:nrow(data), FUN = model.predictions)


data$probability.of.recalcitrance = predictions

for(n in 1:nrow(data)){
  if(!is.na(data[n, "probability.of.recalcitrance"])){
  if(data[n, "probability.of.recalcitrance"] != "Insufficient.information"){
    if(as.numeric(data[n, "probability.of.recalcitrance"]) >=0.5){
      data[n, "storBehav"] = "Recalcitrant"
    }
    if(as.numeric(data[n, "probability.of.recalcitrance"]) <0.5){
      data[n, "storBehav"] = "Orthodox"
    }
  }
  }
}

data[which(data$tax.level == "Species" & data$storBehav == "Recalcitrant"), "probability.of.recalcitrance"] = 1
data[which(data$tax.level == "Species" & data$storBehav == "Orthodox"), "probability.of.recalcitrance"] = 0


colnames(data)[which(colnames(data) %in% c("alt", "taxa.prop.binom"))] = c("altitude", "prop.taxon.recal")
data = data[,-which(colnames(data) %in% c("precip.dispersal", "tmin.dispersal"))]

setwd(wd)



data.out = data[,c("Name_submitted", "Accepted_name", "Genus", "Family", "Order", "tax.level", "probability.of.recalcitrance", "storBehav")]

not.matched = data.out[FALSE,]
not.matched[1:length(species$Name_submitted[which(!species$Name_submitted %in% data$Name_submitted)]),"Name_submitted"] = species$Name_submitted[which(!species$Name_submitted %in% data$Name_submitted)]
data.out = rbind(data.out, not.matched)

rownames(data.out)[order(data.out$Name_submitted)] = rownames(species[order(species$Name_submitted),])

data.out = data.out[order(as.numeric(rownames(data.out))),]

#output the results
#---------------------------------------------------------------------------------------------------------------------
write.table(data.out, "BRT prediction results.txt", col.names = T, row.names = F, sep = "\t", quote = F)

