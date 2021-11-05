library(raster)
library(sp)

#crop the larger xmax to the smaller xmax

#bio13, bio14, bio18, bio19, bio2, bio4, bio8

getwd()
setwd("D:/FileHistory/nicolem/data copy/10. MAXENT - Copy/")

a <- raster("../10. MAXENT - Copy/Acerodon jubatus/bio13.asc")
b <- raster("../10. MAXENT - Copy/Acerodon jubatus/bio14.asc")
c <- raster("../10. MAXENT - Copy/Acerodon jubatus/bio18.asc")
d <- raster("../10. MAXENT - Copy/Acerodon jubatus/bio19.asc")
e <- raster("../10. MAXENT - Copy/Acerodon jubatus/bio2.asc")
f <- raster("../10. MAXENT - Copy/Acerodon jubatus/bio4.asc")
g <- raster("../10. MAXENT - Copy/Acerodon jubatus/bio8.asc")

w <- raster("../10. MAXENT - Copy/Acerodon jubatus/elev.asc")
x <- raster("../10. MAXENT - Copy/Acerodon jubatus/east.asc")
y <- raster("../10. MAXENT - Copy/Acerodon jubatus/north.asc")
z <- raster("../10. MAXENT - Copy/Acerodon jubatus/slope.asc")

#get all the bioclim extents the same

extent(a) #a is the largest extent
#b through g have the same extent
extent(b)
extent(c)
extent(d)
extent(e)
extent(f)
extent(g)

ex = extent(b) #smaller extent

a1 = crop(a, ex)

b1 = crop(b, ex)

res(a1)[1] = 1/120 #x
res(a1)[2] = 1/120 #y

res(b1)[1] = 1/120 #x
res(b1)[2] = 1/120 #y

setwd("D:/FileHistory/nicolem/data copy/10. MAXENT - Copy/Acerodon jubatus/")

writeRaster(a1, "test13", format='ascii', overwrite=T)

writeRaster(b1, "test14", format='ascii', overwrite=T)

#if the extent is the same, just fix the resolution for c-g

res(g)[1] = 1/120 #x
res(g)[2] = 1/120 #y

writeRaster(g, "test8", format='ascii', overwrite=T)

#compare a new identical extent/res bioclim with a topo variable

#crop to the smaller extent xmax
extent(a1)
extent(w)

ex = extent(w) #smaller extent #ERROR was here

w1 = crop(w, ex)

w1 

#now fix the resolution
#set the resolution of the rasters of both dimensions as 1/xmax
#bioclim variable is already fixed

res(w1)[1] = 1/120 #x
res(w1)[2] = 1/120 #y

writeRaster(w1, "testelev", format='ascii', overwrite=T)

#use the new topo variable to crop the other topo variables

#see which has smaller extent  w-z

extent(w1)  
extent(x) 
extent(y) 
extent(z) 

ex = extent(w1) #smaller extent

x1 = crop(x, ex)

res(x1)[1] = 1/120 #x
res(x1)[2] = 1/120 #y

writeRaster(x1, "testeast", format='ascii', overwrite=T)

y1 = crop(y, ex)

res(y1)[1] = 1/120 #x
res(y1)[2] = 1/120 #y

writeRaster(y1, "testnorth", format='ascii', overwrite=T)

z1 = crop(z, ex)

res(z1)[1] = 1/120 #x
res(z1)[2] = 1/120 #y

writeRaster(z1, "testslope", format='ascii', overwrite=T)

#ready for MaxEnt

help("raster-package")

