library(rgdal)
library(scales) #transparency
library(raster)
library(rgeos)
library(dplyr)
library(maptools)
library(colorspace)
library(RColorBrewer)
library(prettymapr) #north arrow and scale bar

wrld = readOGR("D:/FileHistory/nicolem/data copy/land polygons/ne_110m_coastline/ne_110m_coastline.shp")

# iucn ranges
batdirs = list.files("D:/FileHistory/nicolem/data copy/1.range_shp/", full.names = T)
# get the files
iucn = paste0(batdirs, "/", "data_0.shp")
# get the ranges
iucn2 = lapply(iucn, readOGR)
#iucn2 = lapply(iucn [1:4], readOGR) #to do only the first 4

# need to improve color choices
colorspace::sequential_hcl(23)
colorspace::rainbow_hcl(23)
#tool for choosing palate
pal <- choose_palette()
pal(23)


#colors = rep(c("blue", "red", "yellow", "orange"), 10)
colors = pal(23)

# consider saving as a PNG
#save the PDF here
pdf("D:/FileHistory/nicolem/output_shp/world_map2.pdf", height = 6, width = 10)
plot(wrld, lwd = 0.5)
for (i in 1:length(iucn2)) {
  plot(iucn2[[i]], add = T, 
       col = alpha(colors[i], 0.5),
       border = NA)
  addnortharrow(pos = "bottomleft") #customize fix alignment
  addscalebar(pos = "bottomleft") #customize
}
dev.off()

