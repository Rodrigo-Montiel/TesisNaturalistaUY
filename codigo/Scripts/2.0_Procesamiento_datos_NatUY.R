# PAQUETES Y DATOS -----------------------------------------------------------
library(geouy)
library(sf)
library(tidyverse)

NatUY <- read_csv('datos/observations-248320.csv')
Uruguay <- load_geouy("Deptos")


# PROCESAMIENTO --------------------------------------------------------------

# Convertir NatUY a sf
NatUY_sf <- NatUY %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  st_set_crs(4326) %>% 
  st_transform(32721)
