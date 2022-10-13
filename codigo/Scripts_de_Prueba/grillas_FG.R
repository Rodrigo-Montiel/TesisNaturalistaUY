# Para ello, primero analizaremos el total de los registros de la plataforma para Uruguay (más de 50,000 observaciones verificables) y caracterizaremos los datos en función de sus sesgos taxonómicos, espaciales y temporales.

library(tmap)
library(sf)
sf::sf_use_s2(FALSE)
library(tidyverse)

# Uruguay <- geouy::load_geouy("Dptos")
# UY <- st_union(Uruguay) %>% st_cast()
# st_write(Uruguay, 'datos/Uruguay.shp', append=FALSE )
# saveRDS(UY, 'datos/UY.rds')

NatUY <- read_rds('datos/natuysf.rds')
Uruguay <- st_read('datos/Uruguay.shp')
UY <- read_rds('datos/UY.rds')


# grilla para Uruguay
st_bbox(UY)
bbox_Uruguay <- c(xmin=366580, ymin=6127920, xmax=858260, ymax=6671740)
Uruguay_grid <- st_make_grid(st_bbox(UY), 
                             cellsize = 25000, square = FALSE,
                             crs = 'EPSG:32721') %>% 
  st_intersection(UY) %>% st_sf(gridID=1:length(.), geometry= .)

ggplot() + 
  geom_sf(data= Uruguay_grid, fill='white', size=0.5) +
  geom_sf(data= NatUY)



####################
# espaciales 

NatUY_grids <- st_join(Uruguay_grid, NatUY) %>% 
  group_by(gridID) %>% 
  summarise(NR=n(),
            SR=n_distinct(scientific_name)) %>% 
  st_cast()

plot_NR <- ggplot() +
  geom_sf(data=NatUY_grids, aes(fill=log(NR))) +
  scale_fill_fermenter(palette ='YlGnBu', direction = 1) + 
  theme_bw()

plot_SR <- ggplot() +
  geom_sf(data=NatUY_grids, aes(fill=log(SR))) +
  scale_fill_fermenter(palette ='YlOrBr', direction = 1) + 
  theme_bw()

library(patchwork)

plot_SR + plot_NR

plot(NatUY_grids$NR, NatUY_grids$SR)

####################
# temporales

########################################
# sesgos taxonómicos

