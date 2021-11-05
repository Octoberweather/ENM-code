library(raster)
library(rgdal)
library(rgeos)

par(mar=c(1,1,1,1))
par("mar")

reclassify_raster <- function(r, logistic) {
  n <- c(0, logistic, NA, logistic, 1, 1)
  rcltptp <- matrix(n, ncol = 3, byrow = TRUE)
  rc <- reclassify(r, rcltptp)
  return(rc)
}

binaryToPolygon <- function(px, projection, buffer = 0.1){
  #buffer 0.1
  master <- geometry(rasterToPolygons(px, dissolve=TRUE))
  #epsg:3395 WGS World Mercator eurocentric excluding poles
  master <- spTransform(master, CRS('+init=epsg:3395'))
  master <- gBuffer(master, width=buffer)
  master <- gSimplify(master, tol = buffer, topologyPreserve = TRUE)
  
  #remove holes
  outerRings = Filter(function(f){f@ringDir == 1}, master@polygons[[1]]@Polygons)
  master = SpatialPolygons(list(Polygons(outerRings,ID=1)))
  
  IDs <- sapply(slot(master, "polygons"), function(x) slot(x, "ID"))
  df <- data.frame(rep(0, length(IDs)), row.names=IDs)
  master <- SpatialPolygonsDataFrame(master, df)
  proj4string(master) <- '+init=epsg:3395'
  master <- spTransform(master, CRS(projection))
  
  return(master)
}

aggregate_raster <- function(r, maxcells = 1e6) {
  ncells = ncell(r)
  fac = sqrt(ncells / maxcells)
  if (fac >= 1.5) {
    ra = raster::aggregate(r, fact = round(fac, digits = 0))
  } else {
    ra = r
  }
  return(ra)
}

#ne_50m_land.shp land polygons
worldmap = readOGR(dsn = "D:/FileHistory/nicolem/data copy/land polygons/ne_50m_land.shp")

#read in csv of logistic thresholds
#average of maxent runs for 23 bats

bcthresh = read.csv("D:/FileHistory/nicolem/data copy/23_bats_logistic.csv")

projection = "+proj=longlat +datum=WGS84 +no_defs"


for (i in 11:nrow(bcthresh)) {
  raster_string <-paste0("D:/FileHistory/nicolem/output copy/", bcthresh[i, 1], "/bc/", bcthresh[i, 1], "_avg.asc")
  r = raster(raster_string)
  # aggregate to get fewer cells for big extent bats
  # might want to play around with maxcell size if too slow
  ra = aggregate_raster(r, maxcells = 1e6)
  # reclassify based on logistic
  # need to change logistic based on species
  rc = reclassify_raster(ra, logistic = bcthresh[i,3])
  # mask to worldmap
  # first set to same projection as worldmap
  proj4string(rc) <- CRS(projection)
  rcm <- mask(rc, worldmap)
  rcm[rcm != 1] = NA
  # convert to shape file
  rc2 = binaryToPolygon(rcm, projection)
  plot(rc2)
  # write shp to directory
  enmshp <- file.path("D:/FileHistory/nicolem/output_shp/1.enm_shp/")
  #layer is the name of each bat from the csv
  rgdal::writeOGR(rc2, dsn=enmshp, layer = bcthresh[i,1], driver = "ESRI Shapefile")  
  # save shp jpeg to directory
  mypath <- file.path("D:/FileHistory/nicolem/output_shp/2.enm_plots/",paste(bcthresh[i, 1], i, ".jpg", sep = ""))
  
  jpeg(file=mypath)
  mytitle = paste(bcthresh[i,1], i)
  plot(rc2, main = mytitle)
  dev.off()
}






