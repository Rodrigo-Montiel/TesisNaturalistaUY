# PAQUETES Y DATOS -------------------------------------------------------------
library(tmap)
library(sf)
sf::sf_use_s2(FALSE)
library(tidyverse)
library(patchwork)
library(stringr)

NatUY <- read_rds('datos/natuysf.rds')
Uruguay <- geouy::load_geouy("Dptos")
UY <- st_union(Uruguay) %>% st_cast()

# GRILLA PARA URUGUAY ----------------------------------------------------------

st_bbox(UY)
bbox_Uruguay <- c(xmin=366580, ymin=6127910, xmax=858260, ymax= 6671740) 
Uruguay_grid <- st_make_grid(bbox_Uruguay, 
                             cellsize = 25000, square = FALSE) 
Uruguay_grid <- Uruguay_grid %>% st_set_crs(32721)                       

Uruguay_grilla <- st_intersection(Uruguay_grid, UY) %>% 
  st_sf(gridID=1:length(.), geometry= .)



# SESGOS------------------------------------------------------------------------

## Sesgo Espacial

NatUY_grilla <- st_join(Uruguay_grilla, NatUY) %>% 
  group_by(gridID) %>% 
  summarise(Abundancia=n(),
            Riqueza=n_distinct(scientific_name)) %>% st_cast()

plot_Abundancia <- ggplot() +
  geom_sf(data=NatUY_grilla, aes(fill=log(Abundancia)), show.legend = FALSE) +
  ggtitle("N° de Registros") +
  scale_fill_fermenter(palette ='YlGnBu', direction = 1) + 
  theme_bw()

plot_Riqueza <- ggplot() +
  geom_sf(data=NatUY_grilla, aes(fill=log(Riqueza)), show.legend = FALSE) + 
  ggtitle("Riqueza de especies") + 
  scale_fill_fermenter(palette ='YlOrBr', direction = 1) + 
  theme_bw()

### N° de registros en la plataforma y vs cantidad de especies registradas

plot_Abundancia + plot_Riqueza


## Sesgo Taxonomico



## Sesgo Espacial

       