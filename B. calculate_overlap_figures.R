library(sf)
library(rgdal)
library(scales)
library(raster)
library(rgeos)
library(dplyr)
library(plyr)
library(ggplot2)

# get in current ENM shapefiles
curshp = list.files("D:/FileHistory/nicolem/output_shp/1.enm_shp/", pattern = "shp", full.names = T)
curshp2 = lapply(curshp, readOGR)

# get in fut ENM shapefiles
futshp = gsub("1.enm_shp", "3.enm_shp_future", curshp)
futshp2 = lapply(futshp, readOGR)

#get in iucn range
# overlap of current with IUCN
iucndirs = list.dirs("D:/FileHistory/nicolem/output_shp/1.range_shp", full.names = T)
iucnfiles = list.files(iucndirs, pattern = "data_0.shp", full.names=T)
iucnfiles
iucnshp2 <-lapply(iucnfiles, readOGR)


# you will need to load your elevation raster
elev = raster("D:/FileHistory/nicolem/output_shp/wc2.1_2.5m_elev.tif")

# need to record
res = vector("list", length(curshp2))

for (i in 1:length(curshp2)) {
  curbat = curshp2[[i]]
  futbat = futshp2[[i]]
  iucnbat = iucnshp2[[i]]
  
  curbat2 <- sp::spTransform(curbat, CRS('+init=epsg:3395'))
  futbat2 <- sp::spTransform(futbat, CRS('+init=epsg:3395'))
  iucnshp3 <- sp::spTransform(iucnbat, CRS('+init=epsg:3395'))
  
  
  # name of bat
  batname = gsub(".*//", "", curshp[i])
  batname = gsub(".shp", "", batname)
  
  # current area in km^2
  curarea = gArea(curbat2) / 1000 / 1000
  futarea = gArea(futbat2) / 1000 / 1000
  
  # amount of current - future overlap (area)
  inter = gIntersection(curbat2, futbat2)
  interarea = gArea(inter) / 1000 / 1000
  interper = interarea / curarea
  
  # current geographic elevation (mean)
  curelev = extract(elev, curbat)
  curelev2 = mean(curelev[[1]], na.rm = T) 
  # future geographic elevation (mean)
  futelev = extract(elev, futbat)
  futelev2 = mean(futelev[[1]], na.rm = T)

  #overlap of current with iucn 
  iucnarea = gArea(iucnshp3) / 1000 / 1000
  
  # amount of current - IUCN overlap (area)
  interIUCN = gIntersection(curbat2, iucnshp3)
  interIUCNarea = gArea(interIUCN) / 1000 / 1000
  interperIUCN = interIUCNarea / iucnarea
  
  res[[i]] = c(batname, curarea, futarea,
               interarea, interper, curelev2, 
               futelev2, iucnarea, interIUCNarea,
               interperIUCN)
  }
  
res2 = data.frame(do.call("rbind", res), stringsAsFactors = F)
names(res2) = c("species", "current_area", "future_area",
                "current_future_overlap_area", 
                "current_future_overlap_percent",
                "current_elevation", 
                "future_elevation", "IUCN_area",
                "IUCN_current_overlap_area",
                "IUCN_current_overlap_percent")
res3 = res2 %>% mutate_at(vars(-species), as.numeric)
write.csv(res3, "D:/FileHistory/nicolem/output_shp/bat_data_sp-31Dec2020.csv",
          quote = F, row.names = F)


ggplot(res3, aes(percent_area)) + geom_histogram()
ggplot(res3, elevation, percent_area) + geom_point()
ggplot(res3, aes(current_elevation, percent_area)) + geom_point()

plot(iucnshp2)
plot(curbat, add = T, col = alpha("blue", 0.3))
