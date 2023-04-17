library("readxl")
library(tidyverse)
library(ggplot2)
library(OpenStreetMap)
library(sp)

dados = read_excel("~/Dashboard/resource/dados_de_caminhada_corrida.xlsx")
cood = dados %>%
  separate(Coordenadas, into = c("lat", "long"), sep = ",") %>%
  select(long, lat) %>%
  mutate(long = as.numeric(long), lat = as.numeric(lat))

bb = matrix(c(-34.9525, -34.949, 
              -8.018, -8.014), 2,2, byrow=T)

rownames(bb) = c('long', 'lat')
colnames(bb) = c('min', 'max')

crs = CRS("+proj=utm +zone=25 +south +datum=WGS84")

lonr = bb[1,2]; latu = bb[2,2] 
lonl = bb[1,1]; latd = bb[2,1]

sa_map = openmap(c(latu+0.001, lonl-0.001), 
                 c(latd-0.001, lonr+0.001),
                 type = "osm", mergeTiles = TRUE, minNumTiles = 9L)

sa_map2 = openproj(sa_map)


sa_map2_plt = OpenStreetMap::autoplot.OpenStreetMap(sa_map2) + 
  geom_point(data = cood,
             aes(x = long, y = lat), # slightly shift the points
             colour = "red", size =  2.5) +
  xlab("Longitude") + ylab("Latitude")
sa_map2_plt
