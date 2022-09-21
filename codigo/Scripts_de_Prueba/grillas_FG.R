# Para ello, primero analizaremos el total de los registros de la plataforma para Uruguay (más de 50,000 observaciones verificables) y caracterizaremos los datos en función de sus sesgos taxonómicos, espaciales y temporales.

library(tmap)
library(sf)
sf::sf_use_s2(FALSE)
library(tidyverse)

NatUY <- read_rds('datos/natuysf.rds')
Uruguay <- geouy::load_geouy("Dptos")
UY <- st_union(Uruguay) %>% st_cast()

# grilla para Uruguay
st_bbox(UY)
bbox_Uruguay <- c(xmin=366580, ymin=6127910, xmax=858260, ymax= 6671740)
Uruguay_grid <- st_make_grid(bbox_Uruguay, cellsize = 25000, crs =st_crs(Uruguay)) %>% 
  st_intersection(UY) %>% st_sf('geometry' = ., 'ID'=sprintf('%i', 1:length(.)))

ggplot() + 
  geom_sf(data= Uruguay_grid, fill='white', size=0.5) +
  #geom_sf(data=Uruguay, fill=NA, size=0.5) +
  geom_sf(data= NatUY, aes(col=taxon_class_name), show.legend = F)

# sesgos taxonómicos
NatUY
# espaciales 
# temporales