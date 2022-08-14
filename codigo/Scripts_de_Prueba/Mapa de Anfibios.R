library(geouy)
library(sf)
library(tidyverse)

# Descargar los datos desde NaturalistaUY y cargarlos
NatUY <- read_csv("datos/observations-248320.csv")


# Descargar la capa de departamentos de Uruguay

Uruguay <- load_geouy("Deptos", folder = "datos")

# Filtrado de NatUY: Todos los anfibios + 
# observados en octubre de 2021 + 
# con especie confirmada (Grado de investigación)

Anfibios <- NatUY %>% 
  filter(taxon_class_name == "Amphibia" & quality_grade == "research" & 
         between(observed_on, as.Date("2021-10-01"), as.Date("2021-10-31")))

# Transformamos la tabla de Anfibios (spec_tbl_df) en un sf (Simple Features)
# Como lo es Uruguay

Anfibios_XY <- Anfibios %>% 
  select(scientific_name, observed_on, latitude, longitude) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  st_set_crs(4326) %>% 
  st_transform(32721)
         


# Poligono de Uruguay + puntos de anfibios
Uruguay_Anfibios <- st_join(Uruguay, Anfibios_XY)


Uruguay_Anfibios <- Uruguay_Anfibios %>%
  group_by(nombre) %>% 
  summarise(riqueza = n_distinct(scientific_name), 
            abundancia = n())


#Mapa de riqueza de los anfibios filtrados
ggplot(data = Uruguay_Anfibios) + 
  geom_sf(aes(fill=riqueza)) + theme_bw()

#Mapa de abundancia de los anfibios filtrados
ggplot(data = Uruguay_Anfibios) + 
  geom_sf(aes(fill=abundancia)) + theme_bw()
