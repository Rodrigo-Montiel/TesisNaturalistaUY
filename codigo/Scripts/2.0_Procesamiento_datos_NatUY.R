# PAQUETES Y DATOS -------------------------------------------------------------
library(geouy)
library(sf)
sf::sf_use_s2(FALSE)
library(tidyverse)

NatUY <- read_csv('datos/Observaciones_27-10-22.csv')
Uruguay <- load_geouy("Deptos")



# PROCESAMIENTO ----------------------------------------------------------------

# Convertir NatUY a sf
NatUY_sf <- NatUY %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  st_set_crs(4326) %>% 
  st_transform(32721)



# GUARDAR LOS DATOS ------------------------------------------------------------

saveRDS(NatUY_sf, "datos/natuysf.rds")
saveRDS(Uruguay, "datos/uruguay.rds")
