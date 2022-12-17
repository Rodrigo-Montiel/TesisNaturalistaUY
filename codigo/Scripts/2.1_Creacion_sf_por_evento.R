# PAQUETES Y DATOS--------------------------------------------------------------
library(geouy)
library(sf)
sf::sf_use_s2(FALSE)
library(tidyverse)

NatUY_sf <- readRDS("datos/natuysf.rds")



# KML DE LOS EVENTOS------------------------------------------------------------

# San José
quintadelhorno <- st_read('datos/quinta-del-horno.kml')
quintadelhorno <- quintadelhorno %>% st_transform(32721)

  
# Paysandú
pueblochico <- st_read("datos/pueblo-chico-nativas-de-san-francisco.kml")
pueblochico <- pueblochico %>% st_transform(32721)


# Bella Union (tenemos 2 kml distintos)
rincon <- st_read("datos/area-de-manejo-de-habitats-y-o-especies-rincon-de-franquia.kml")
rincon <- rincon %>% st_transform(32721)

chacra <- st_read("datos/chacra-bella-union.kml")
chacra <- chacra %>% st_transform(32721)

bella_union <- st_union(rincon,chacra)



# FILTRADO DE NATUY PARA CADA EVENTO -------------------------------------------

# San José
NatUY_SanJose <- st_intersection(NatUY_sf, quintadelhorno) %>% 
  filter(observed_on>="2022-05-07" & observed_on<="2022-05-08")
   
# Paysandú
NatUY_Paysandu <- st_intersection(NatUY_sf, pueblochico) %>% 
  filter(observed_on>="2022-05-21" & observed_on<="2022-05-22")

# Bella Unión
NatUY_BellaUnion <- st_intersection(NatUY_sf, bella_union) %>% 
  filter(observed_on>="2022-05-14" & observed_on<="2022-05-15")


# GUARDAR LOS DATOS ------------------------------------------------------------

saveRDS(NatUY_SanJose, "datos/natuysanjose.rds")
saveRDS(NatUY_Paysandu, "datos/natuypaysandu.rds")
saveRDS(NatUY_BellaUnion, "datos/natuybellaunion.rds")
