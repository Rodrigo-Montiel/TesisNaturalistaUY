# Para ello, primero analizaremos el total de los registros de la plataforma para Uruguay (más de 50,000 observaciones verificables) y caracterizaremos los datos en función de sus sesgos taxonómicos, espaciales y temporales.

library(patchwork)
library(lubridate)
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
# bbox_Uruguay <- c(xmin=366580, ymin=6127920, xmax=858260, ymax=6671740)
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
  geom_sf(data=Uruguay, fill=NA) +
  theme_bw()

plot_SR <- ggplot() +
  geom_sf(data=NatUY_grids, aes(fill=log(SR))) +
  scale_fill_fermenter(palette ='YlOrBr', direction = 1) + 
  geom_sf(data=Uruguay, fill=NA) +
  theme_bw()

plot_relationship <- ggplot(NatUY_grids, aes(x=NR, y=SR)) +
  geom_point() +
  geom_smooth(method='loess') +
  labs(x='Number of records', y='Species Richness') +
  theme_bw()

plot_SR + plot_NR + plot_relationship

####################
# temporales

dataset_NatUY <- NatUY %>% st_drop_geometry() %>% 
  select(id, observed_on, user_id, user_login, created_at, updated_at, 
         quality_grade, num_identification_agreements, 
         num_identification_disagreements, positional_accuracy, 
         coordinates_obscured, place_admin1_name, scientific_name,
         taxon_kingdom_name, taxon_phylum_name, taxon_class_name, 
         taxon_order_name, taxon_family_name, taxon_genus_name)

dataset_NatUY %>% 
  filter(year(observed_on)>=2000) %>% 
  filter(!is.na(taxon_phylum_name)) %>% 
  filter(taxon_kingdom_name=='Plantae' | taxon_kingdom_name=='Animalia' | taxon_kingdom_name=='Fungi') %>% 
  ggplot(aes(x=observed_on, y=taxon_phylum_name, color=taxon_kingdom_name)) +
  geom_point(show.legend = FALSE) +
  facet_grid(taxon_kingdom_name~., scales = "free", space= 'free_y', switch='x' ,drop=TRUE) +
  theme_bw() +
  theme(strip.text.y = element_text(size=10, angle=0)) +
  labs(title = 'Cobertura temporal de los Filos registrados para los reinos Animales, Plantas y Hongos', 
       x='Año', y='Filo', color = '') 

# Ejemplo, plantas vasculares con especies que se hayan registrado más de 50 veces
dataset_NatUY %>% 
  filter(year(observed_on)>=2000) %>% 
  filter(!is.na(taxon_genus_name)) %>% 
  filter(taxon_phylum_name=='Tracheophyta') %>% 
  group_by(scientific_name) %>% 
  filter(n()>50) %>% ungroup() %>% 
  ggplot(aes(x=observed_on, y=taxon_genus_name, color=taxon_family_name)) +
  geom_point(show.legend = FALSE) +
  facet_grid(taxon_family_name~., scales = "free", space= 'free_y', switch='x' ,drop=TRUE) +
  theme_bw() +
  theme(strip.text.y = element_text(size=10, angle=0)) +
  labs(title = 'Cobertura temporal de Familias de plantas vasculares con más de 50 registros en NaturalistaUY', 
       x='Año', y='Familia', color = '') 


########################################
# sesgos taxonómicos
dataset_NatUY %>% 
  filter(taxon_kingdom_name=='Plantae' | taxon_kingdom_name=='Animalia' | taxon_kingdom_name=='Fungi') %>% 
  group_by(taxon_kingdom_name, taxon_class_name) %>% 
  count() %>% 
  ggplot(aes(x='', y=n, fill=taxon_class_name)) +
  geom_bar(width = 0.5, stat = "identity", show.legend = F) +
  facet_grid(~taxon_kingdom_name) 

# dataset_NatUY %>% 
#   filter(taxon_kingdom_name=='Animalia') %>% 
#   filter(year(observed_on)>=2000) %>% 
#   filter(!is.na(taxon_genus_name)) %>% 
#   group_by(scientific_name) %>% 
#   filter(n()>50) %>% ungroup() %>% 
#   distinct(scientific_name, taxon_class_name, .keep_all = T) %>% 
#   group_by(taxon_class_name, taxon_order_name) %>% 
#   count() %>% 
#   ggplot(aes(x='', y=n, fill=taxon_order_name)) +
#   geom_bar(width = 0.5, stat = "identity") +
#   facet_wrap(~taxon_class_name) + 
#   labs(x='', y= 'Number of Species', fill = '') +
#   theme_minimal() 

