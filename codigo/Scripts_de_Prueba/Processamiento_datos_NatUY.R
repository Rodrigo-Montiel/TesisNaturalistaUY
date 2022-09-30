library(geouy)
library(sf)
library(tidyverse)

Uruguay <- geouy::load_geouy("Dptos")

NatUY <- read_csv('datos/observations-248320.csv')

NatUY_sf <- NatUY %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  st_set_crs(4326) %>% 
  st_transform(32721)

quintadelhorno <- st_read('datos/quinta-del-horno.kml')
quintadelhorno <- quintadelhorno %>% st_transform(32721)

ggplot() +
  geom_sf(data=Uruguay, fill='white') +
  geom_sf(data=NatUY_sf) +
  geom_sf(data=quintadelhorno, fill='red', col=NA, size=10)

library(lubridate)
fecha_inicial <- lubridate::as_date('2022-05-07')
fecha_final <- lubridate::as_date('2022-05-08')

# San José
NatUY_quintadelhorno <- st_intersection(NatUY_sf, quintadelhorno) %>% 
  filter(observed_on>=fecha_inicial & observed_on<=fecha_final) 

# Bella Unión
NatUY_sf %>% 
  filter(place_admin1_name=='Artigas') %>% 
  filter(observed_on>=as_date('2022-05-14') & observed_on<=as_date('2022-05-15')) 


NatUY %>% print(n=100)
