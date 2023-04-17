library(ggplot2)
library(OpenStreetMap)
library(sp)
long = c(-34.868, -34.868, -34.868)
lat = c(-8.045, -8.047, -8.048)

bb = matrix(c(-34.87, -34.86, 
              -8.06, -8.04), 2,2, byrow=T)
rownames(bb) = c('long', 'lat')
colnames(bb) = c('min', 'max')

crs = CRS("+proj=utm +zone=25 +south +datum=WGS84")


df = data.frame(veloc = c(3,4,5))


df$long = long
df$lat = lat
lonr = bb[1,2]; latu = bb[2,2] 
lonl = bb[1,1]; latd = bb[2,1]

sa_map = openmap(c(latu+0.001, lonl-0.001), 
                 c(latd-0.001, lonr+0.001),
                 type = "osm", mergeTiles = TRUE, minNumTiles = 9L)

sa_map2 = openproj(sa_map)


sa_map2_plt = OpenStreetMap::autoplot.OpenStreetMap(sa_map2) + 
  geom_point(data = df,
             aes(x = long, y = lat), # slightly shift the points
             colour = "red", size =  2.5) +
  xlab("Longitude") + ylab("Latitude")
sa_map2_plt
