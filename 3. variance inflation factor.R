#library(car) 
library(raster)
library(usdm) #for vif and vifstep function

#read in the maxent trimmed bioclim variables
getwd()
setwd("D:/FileHistory/nicolem/data copy/10. MAXENT/Taphozous hildegardeae/")

#the *.asc loads in the raster extension
climr = list.files(pattern = "bio.*asc", full.names=T, recursive=FALSE)
tail(climr)

climrbrk<- stack(lapply(climr, raster))

par(mar = c(1,1,1,1))
plot(climrbrk[[1]]) 

##takes a while to compute
# v3 <- Variogram(climrbrk[[1]]) # compute variogram for the first raster
# 
# plot(v3)
# plot(v3,cloud=TRUE)
# plot(v3,box=TRUE)
#we used variance inflation factors (VIF), 
#calculated with the 'vifstep' function with a threshold of 10 using the USDM R package

#using variance inflation factor th = 0.9
#using vifcor and vifstep
vif(climrbrk) #calculates vif for the variables in climrbrk

# v1 <- vifcor(climrbrk, th=0.9) #id colinear variables that should be excluded
# re1 <- exclude(climrbrk, v1)
# re1

v2 <- vifstep(climrbrk, th=10)
v2
re2 <- exclude(climrbrk, v2)
re2




