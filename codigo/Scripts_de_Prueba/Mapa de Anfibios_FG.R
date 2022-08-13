# Descargar la capa de departamentos de Uruguay

install.packages("geouy")
library(geouy)
library(sf)
library(tidyverse) # dplyr, readr

Uruguay <- load_geouy("Dptos")
NatUY <- read_csv('datos/observations-248320.csv')

# Filtrado de: Todos los anfibios en Uruguay + 
# observados en octubre de 2021 + 
# con especie confirmada (Grado de investigación)

Anfibios <- NatUY %>% 
  filter(taxon_class_name == "Amphibia" & 
         quality_grade == "research" &
          observed_on >= "2021-10-01" & observed_on <= "2021-10-31")
         
# PRUEBA 1
#Separamos en un nuevo mapa los datos que nos interesan de los departamentos
#Renombramos la columna de departamentos para que coincida con Anfibios
Uruguay2 <- Uruguay %>% 
  select(nombre, geom) %>% 
  rename(place_admin1_name = nombre)

#Unimos las dos tablas (Anfibios y Uruguay2)
Anfibios <- left_join(Anfibios, Uruguay2, by= "place_admin1_name")
view(Anfibios)

?st_join

#Mapa de los anfibios filtrados
ggplot(data = Anfibios) + 
  geom_sf(aes(fill=taxon_family_name)) + theme_bw()

# PRUEBA 2

# dataframe no espacial
Anfibios_XY <- Anfibios %>% 
  select(scientific_name, observed_on, latitude, longitude) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  st_set_crs(4326) %>% 
  st_transform(32721)

# polígono de Uruguay + puntos de anfibios
Uruguay_Anfibios <- st_join(Uruguay, Anfibios_XY)

Uruguay_Anfibios <- Uruguay_Anfibios %>% 
  group_by(nombre) %>% 
  summarise(riqueza=n_distinct(scientific_name),
            abundancia=n())

ggplot(data=Uruguay_Anfibios) +
  geom_sf(aes(fill=abundancia))

