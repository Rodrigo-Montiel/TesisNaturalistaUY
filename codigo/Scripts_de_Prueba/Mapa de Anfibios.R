# Descargar la capa de departamentos de Uruguay

install.packages("geouy")
library(geouy)
Uruguay <- load_geouy("Deptos", folder = "datos")

# Filtrado de: Todos los anfibios en Uruguay + 
# observados en octubre de 2021 + 
# con especie confirmada (Grado de investigación)

Anfibios <- NatUY %>% 
  filter(taxon_class_name == "Amphibia", quality_grade == "research", 
         between(observed_on, as.Date("2021-10-01"), as.Date("2021-10-31")))
         

#Separamos en un nuevo mapa los datos que nos interesan de los departamentos
#Renombramos la columna de departamentos para que coincida con Anfibios
Uruguay2 <- Uruguay %>% 
  select(nombre, geometry) %>% 
  rename(place_admin1_name = nombre)

#Unimos las dos tablas (Anfibios y Uruguay2)
Anfibios <- left_join(Anfibios, Uruguay2, by= "place_admin1_name")
view(Anfibios)

#Mapa de los anfibios filtrados
ggplot(data = Anfibios) + 
  geom_sf(aes(fill=taxon_family_name)) + theme_bw()
