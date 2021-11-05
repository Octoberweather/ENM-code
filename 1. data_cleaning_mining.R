#using coordinate cleaner package

library(countrycode)
library(CoordinateCleaner)
library(spThin)
library(dplyr)
library(plyr)
library(readr)
library(ggplot2)
library(rgbif)
library(sp)
library(magrittr)
library(raster)
library(rworldmap)
library(rgeos)

library(maps)
library(mapproj)
library(maptools)
library(ggmap)
library(rgdal)
library(sp)
library(mapdata)
library(dismo)
library(data.table)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(googleway)
library(ggrepel)
library(ggspatial)

#Prepare data IUCN threatened -> GBIF ->SPTHIN

bat <- read.csv("D:/FileHistory/nicolem/data copy/simple summary chiroptera - Copy.csv")

# Reference data
world <- getData("countries")

#getting the data from gbif
#save to dir
for (i in 132:nrow(bat)) {
  #get data from gbif
  dat <- occ_search(scientificName = bat[i,3], limit = 5000, hasCoordinate = T)

  #get the data records
  dat <- dat$data
  
  #write csv to directory && means if the first one fails, don't do the second
  if(!is.null(dat) && nrow(dat)>15){ 
    write.csv(dat, paste("D:/FileHistory/nicolem/data copy/A. cc_bat_loc/", 
                      bat[i,3], ".csv", sep = ""))
  }
}

getwd()
setwd("D:/FileHistory/nicolem/data copy/A. bat_loc/")
#before you run make sure your wd is set!
filelist = list.files(pattern="*.csv") #80 species
#merge csv to see total occurrences
filelistall = lapply(filelist, read.csv)
#count the number of rows in each data.frame
sapply(filelistall, NROW)
#add the numbers minus 80 to account for header
137+ 39+ 103+ 1167+ 82+ 212+ 24+ 28+ 37+ 61+ 188+ 50+ 31+ 26+ 43+ 46+ 19+ 20+ 22+ 17+ 88+ 1798+ 1890+ 222+ 867+ 116+ 35+ 5000+ 240+ 99+ 243+ 21+ 16+ 138+ 29+ 79+ 2903+ 386+ 209+ 23+ 939+ 38+ 261+ 1321+ 56+ 772+ 36+ 107+ 98+ 3565+ 32+ 42+ 66+ 33+ 423+ 80+ 269+ 83+ 90+ 4412+ 27+ 204+ 432+ 106+ 364



#write correct names for scientificName column in occ_search
for(batfile in filelist){
  currentspecies=read.csv(batfile)
  batnames= (currentspecies[, "scientificName"]= gsub(".csv", "", batfile))
  name=batnames
  #save currentspecies to dir
  write.csv(currentspecies, paste0("D:/FileHistory/nicolem/data copy/A.1. cc_bat_loc_with_correct_names/", 
                            name, ".csv", sep = ""))
}

#modifying columns
getwd()
setwd("D:/FileHistory/nicolem/data copy/")
newfilelist = list.files(pattern="*.csv")
filelistall = lapply(newfilelist, read.csv)

#getting the first element in filelistall

#assign the column structure to a blank df
#get the columns with the right name for blank df
#grab a bat with a lot of columns
samplebat <- filelistall[[4]]
#select only these columns
#does not alter samplebat
fulldataframe <- samplebat %>%
  dplyr::select(scientificName, decimalLongitude, decimalLatitude, countryCode, individualCount,
                gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
                basisOfRecord, institutionCode, datasetName) #select only these 13 columns
#then clear it out to create a blank df
fulldataframe <- fulldataframe[0:0]

#loop through and make sure they all have the same columns
for(listelement in filelistall){
  if("scientificName" %in% colnames(listelement) &&
     "decimalLongitude" %in% colnames(listelement) &&
     "decimalLatitude" %in% colnames(listelement) &&
     "countryCode" %in% colnames(listelement) &&
     "individualCount" %in% colnames(listelement) &&
     "gbifID" %in% colnames(listelement) &&
     "family" %in% colnames(listelement) &&
     "taxonRank" %in% colnames(listelement) &&
     "coordinateUncertaintyInMeters" %in% colnames(listelement) &&
     "year" %in% colnames(listelement) &&
     "basisOfRecord" %in% colnames(listelement) &&
     "institutionCode" %in% colnames(listelement) &&
     "datasetName" %in% colnames(listelement))
{ #select only these 13 columns for each bat
    reducedcolumnscurrent <- listelement %>%
      dplyr::select(scientificName, decimalLongitude, decimalLatitude, countryCode, individualCount,
                    gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
                    basisOfRecord, institutionCode, datasetName) 
   #take the growing fulldataframe and add each reduced bat df
    #and rbind them together, and save it all in the growing fulldataframe
    fulldataframe <- rbind(fulldataframe, reducedcolumnscurrent)
 } 
}

#convert all zero values to NA for is.na command
fulldataframe[fulldataframe==0] <- NA
fulldataframe[fulldataframe=="NA"] <- NA

# remove records without coordinates
fulldataframe <- fulldataframe%>%
  filter(!is.na(decimalLongitude))%>%
  filter(!is.na(decimalLatitude))

#remove duplicates
fulldataframe <- fulldataframe[!duplicated(fulldataframe$decimalLongitude),]

#remove records below 100km grain size
fulldataframe <- fulldataframe %>%
  filter(coordinateUncertaintyInMeters / 1000 <= 100 | is.na(coordinateUncertaintyInMeters))

#remove unsuitable sources
fulldataframe <- filter(fulldataframe, basisOfRecord == "HUMAN_OBSERVATION" |
                basisOfRecord == "OBSERVATION" |
                basisOfRecord == "PRESERVED_SPECIMEN")

#flag problems. make sure it's a dataframe
fulldataframe <- data.frame(fulldataframe)

#take out country tests, there is a bug in cc_coun
flags <- clean_coordinates(x = fulldataframe,
                           lon = "decimalLongitude",
                           lat = "decimalLatitude",
                           species = "scientificName",
                           tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                     "zeros")) 
summary(flags)
plot(flags, lon = "decimalLongitude", lat = "decimalLatitude")

#Exclude problematic records
dat_cl <- fulldataframe[flags$.summary,]

#The flagged records
dat_fl <- fulldataframe[!flags$.summary,]

#make sure all the plots look ok before the next step

#plot data to get an overview
wm <- borders("world", colour="gray50", fill="gray50")
ggplot()+ coord_fixed()+ wm +
  geom_point(data = dat_cl, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred", size = 0.5)+
  theme_bw()

#save dat_cl to dir
write.csv(dat_cl, paste0("D:/FileHistory/nicolem/data copy/", 
                          "bat_loc_cc", ".csv", sep = ""))

dat_cl <- read.csv("D:/FileHistory/nicolem/data copy/bat_loc_cc.csv")

#split the data into a list corresponding to each of the species
dat_cl_sp <- split.data.frame(dat_cl, dat_cl$scientificName)

#write each element in the list to a csv
for(sp in dat_cl_sp){
  write.csv(sp, paste0("D:/FileHistory/nicolem/data copy/A.2. cc_bat_loc_byspecies/",
                       sp$scientificName[[1]], ".csv", sep = ""  ))
}

#read in the list of species and save to df
getwd()
setwd("D:/FileHistory/nicolem/data copy/A.2. cc_bat_loc_byspecies/")

cclist = list.files(pattern="*.csv")
#ccspecies=ldply(cclist, read.csv, .id=NULL)
ccspecies=lapply(cclist, read.csv)
#start spatial thinning

#loop for thinning
for(i in ccspecies) {
  # need to save thinned data to a dataframe
  df = thin(i,lat.col = "decimalLatitude",
            long.col = "decimalLongitude",
            spec.col = "scientificName",
            thin.par = 1,
            reps = 10,
            out.dir = "D:/FileHistory/nicolem/data copy/TEST/",
            out.base = i$scientificName[[1]],
            max.files = 1,
            write.files = T)
}

#remove bats with less than 15 locs
getwd()
setwd("D:/FileHistory/nicolem/data copy/C. spthin_bat_loc/")

#upload sp thin data as a list
file_list <- list.files(pattern = "*.csv")

#upload all files and create a dataset
thin_d <-ldply(file_list, read.csv, .id=NULL)

#15 or more occurrences are thin_d_15
thin_d_15 <- (thin_d %>%
                group_by(scientificName) %>%
                filter(n()>14))

#split thin_d_15 by species
thin_d_15_sp <- split(thin_d_15, thin_d_15$scientificName)

#write csvs for each species from thin_d_15_sp 
getwd()
setwd("/FileHistory/nicolem/data copy/C.1. spthin_15_byspecies/")

for(i in names(thin_d_15_sp)){
  write.csv(thin_d_15_sp[[i]], paste0(i,".csv"))
}

#read in the final bat thin 15
getwd()
setwd("D:/FileHistory/nicolem/data copy/C.1. spthin_15_byspecies/")
file_list <- list.files(pattern = "*.csv")
#thin15list=lapply(file_list, read.csv)

#export to dataframe
thin15df=ldply(file_list, read.csv, .id = NULL)
#get rid of integer column
thin15df[[1]] <- NULL


#to only get the unique names
sps <- unique(thin15df$scientificName)
#write a csv
write.csv(sps, "bat names 35.csv")

#change scientific name column to species for sp thin
names(thin15df)[names(thin15df) == "scientificName"] <- "species"
 

#read in the master driver list csv with just bat names
batnames35=read.csv("D:/FileHistory/nicolem/data copy/bat names 35.csv")
#change name of "bat" column to "species"
names(batnames35)[names(batnames35) == "Bat"] <- "species"
#take out the integer first column
batnames35[[1]] <- NULL

#clean according to iucn range

#pull shp files from directory
spdirs = list.dirs("D:/FileHistory/nicolem/data copy/1.range_shp", full.names = T, recursive = F)
shp = list.files(spdirs, pattern = "data_0.shp", full.names=T, recursive=FALSE)
#example shp dummy dataframe to store growing list of shp in for loop
sampleshp <- shp[[1]]
#cleared out, set it to a a ch to fill with shp
sampleshp <- sampleshp[0:0] #creates a blank list
#i=1

#reading in all shp, changing column name, and saving to df called sampleshp
for (i in 1:nrow(batnames35)){
  #read in ranges and modify species column
  iucn_range_string <- paste0("D:/FileHistory/nicolem/data copy/1.range_shp/", batnames35[i,1], "/data_0.shp")
  iucn_range_shp <- shapefile(iucn_range_string)
  #append the shp to the growing df
  names(iucn_range_shp)[names(iucn_range_shp) == "BINOMIAL"] <- "species"
  #sampleshp is list of df
  sampleshp <- append(sampleshp, iucn_range_shp)
  #take the growing sampleshp 
  #and append them together, and save it all in the growing list
}



#thin15df is for gbif ranges, but it needs to be subset by species
#sampleshp is for iucn ranges

#need to isolate out singular bat names from thin15df, and run cc_iucn for each
for (i in 1:nrow(batnames35)){
  #get one bat name from the row. make sure it is only one column
  singularbatname = batnames35[i,]
  #subset a df based on a condition, which is species name
  #gives all the data points for the current bat
  singular_sp_thin15df = subset(thin15df, species == singularbatname)
  #clean gbif coordinates accding to iucn range
  range_flags <- cc_iucn(x = singular_sp_thin15df, #df
                         range = sampleshp[[i]], #spdf
                         lon = "decimalLongitude",
                         lat = "decimalLatitude",
                         species="species",
                         value = "flagged")
  #saving the cleaned (flagged) records to dat_fin
  dat_fin <- singular_sp_thin15df[range_flags, ]
  #if the count is 15 or more, write the file
  if(nrow(dat_fin)>=15){ 
    write.csv(dat_fin, paste("D:/FileHistory/nicolem/data copy/D. cc_thin_trim_iucn/", 
                         singularbatname, ".csv", sep = ""), row.names = F)
  }
  
}


#read in dr and visualize the maps
setwd("D:/FileHistory/nicolem/data copy/D. cc_thin_trim_iucn/")

file_list <- list.files(pattern = "*.csv")
finalbat23=lapply(file_list, read.csv)
finalbat23df=ldply(file_list, read.csv, .id = NULL)

fb23dfnames=unique(finalbat23df$species)
write.csv(fb23dfnames, "final bat names 23.csv")
#list of bat names
batnames23=read.csv("/FileHistory/nicolem/data copy/final bat names 23.csv")

#subset iucn range spdf files according to species in the finalbat23 list
#remove multiple list elements at once
sampleshp23 <- sampleshp[-c(2,7,10,12,16,20,21,22,26,27,31,35)]


#read in the cc thin trim iucn range bats
getwd()
setwd("D:/FileHistory/nicolem/data copy/D. cc_thin_trim_iucn/")

file_list = list.files(pattern = "*.csv")
#list of df
trimspecies=lapply(file_list, read.csv)

#visualize iucn range shp in ggplot
plo <- fortify(sampleshp23[[23]])

#visualize trimmed gbif
trimmedgbif <- trimspecies[[23]]


#plot one more time to see if okay
#plo and trimmedgbif
wm <- borders("world", colour="grey70", fill="black")
ggplot()+ coord_fixed()+ wm +
  geom_point(data = trimmedgbif, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "green", size = 2.0, alpha=0.1)+
  geom_polygon(data = plo, aes(x = long, y = lat, group = group))+
  theme_bw()


# Add/subtract 1 degree to add a small buffer around the data points
PopRange<-c(min(trimmedgbif$decimalLongitude)-1,
            min(trimmedgbif$decimalLatitude)-1,
            max(trimmedgbif$decimalLongitude)+1,
            max(trimmedgbif$decimalLatitude)+1)


#now zoom in
wm <- borders("world", colour="grey70", fill="grey50")
ggplot()+ coord_fixed(xlim=PopRange[c(1,3)], ylim=PopRange[c(2,4)])+ wm +
  geom_point(data = trimmedgbif, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "green", size = 2.0)+
  geom_polygon(data = plo, aes(x = long, y = lat, group = group, fill="coral2", 
                               alpha=0.2))+
  theme_bw()+
  ggtitle(trimmedgbif$species)+
  theme(legend.position = "none",
        axis.title = element_blank())

#save current plot to dir
setwd("D:/FileHistory/nicolem/data copy/D.1. cc_thin_trim_iucn_plots/")
ggsave("Taphozous hildegardeae.png")


##### extra 

 #extent object
 e<-extent(min(trimmedgbif$decimalLongitude)-1,max(trimmedgbif$decimalLongitude)+1,min(trimmedgbif$decimalLatitude)-1,max(trimmedgbif$decimalLatitude)+1)
 #find the max lon and max lat and extence to 1 degree
 max_lon <- max(trimmedgbif$decimalLongitude) +1 
 max_lat <- max(trimmedgbif$decimalLatitude) +1

 
#change BINOMIAL column to species so it will work
#for each element in the list

#passing in a list of objects that have df
#need to reference the individual object
for(s in seq_along(test)){
  names(test[[s]])[names(test[[s]]) == "BINOMIAL"] <- "species"
}

#get subset in first file of species in second file
result=dat_cl[dat_cl$scientificName %in% batnames35$scientificName,]
#this also works for vector
# vectorOfShapes <- vector() #creates a blank vector

#this is for one shapefile
#change BINOMIAL column to species so it will work
names(balantiopteryxIUCN)[names(balantiopteryxIUCN) == "BINOMIAL"] <- "species"
head(balantiopteryxIUCN) #check that it worked

#set ggplot theme
#for mapping one shapefile
ggplot() +
  borders("world", colour="gray50", fill="gray50")+
  geom_polygon(data = balantiopteryxIUCN , aes(x = long, y = lat, group = group))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_blank())

#flag records based on species natural ranges
#loop through and change the range for all the bats in the list of shp dfs

#remove extra column position index from test if this applies
test[[1]] <- NULL 

#plot data to get an overview
wm <- borders("world", colour="gray50", fill="gray50")
ggplot()+ coord_fixed()+ wm +
  geom_point(data = dat_fin, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred", size = 0.5)+
  theme_bw()
 
name="Amorphochilus_schnablii"
#save dat_fin to A. cc_bat_loc
write.csv(dat_fin, paste0("D:/FileHistory/nicolem/data copy/A. cc_bat_loc/", 
                          name, ".csv", sep = ""))





























