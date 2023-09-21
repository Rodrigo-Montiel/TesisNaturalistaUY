# PAQUETES Y DATOS--------------------------------------------------------------
library(tidyverse)
library(sf)
sf::sf_use_s2(FALSE)
library(dplyr)

NatUY <- read_csv('datos/Tablas/Observaciones_27-10-22.csv')

# FILTRADO DE ESPECIES CASUALES y SUBESPECIES-----------------------------------
## De la categorización de registros (4_categorizacion_de_registros.R) nos 
## encontramos con especies "casuales" y subespecies dentro de los regstros.
## Vamos a eliminarlas:

NatUY <- NatUY %>%  filter(str_count(scientific_name, "\\S+") ==2 ) %>% 
  filter(scientific_name!= "Anas platyrhynchos" | 
           scientific_name!= "Anser anser" | 
           scientific_name!= "Canis familiaris" | 
           scientific_name!= "Equus asinus" | 
           scientific_name!= "Equs caballus" | 
           scientific_name!= "Felis catus" | 
           scientific_name!= "Corvus splendens" | 
           scientific_name!= "Melopsittacus undulatus" | 
           scientific_name!= "Cervus elaphus" | 
           scientific_name!= "Oryctolagus cuniculus" | 
           scientific_name!= "Agapornis personatus" | 
           scientific_name!= "Amazona aestiva")

## str_count(scientific_name, "\\S+") ==2 nos permite seleccionar aquellos
## registros que en el campo scientific_name cuenten con dos palabras


write.csv(NatUY,"datos/Tablas/NatUY.csv")


# CONTRIBUCIONES NATURALISTAUY-------------------------------------------------

## Cantidad de registros
nrow(NatUY)


## Cantidad de Especies registradas
NatUY %>% st_drop_geometry() %>%
  group_by(scientific_name) %>% 
  filter(!is.na(scientific_name)) %>% 
  filter(str_count(scientific_name, "\\S+") ==2 ) %>% 
  count() %>% arrange(desc(n)) %>% nrow()


## Registros con Grado de investigación
NatUY %>% st_drop_geometry() %>% filter(quality_grade == "research") %>% 
  group_by(quality_grade) %>% nrow()


## Cantidad de usuarios
NatUY %>% st_drop_geometry() %>% group_by(user_id) %>% 
  count() %>% arrange(desc(n)) %>% nrow()


# REGISTROS POR REINOS----------------------------------------------------------

Tabla_reinos <- NatUY %>% st_drop_geometry() %>%  
  filter(str_count(scientific_name, "\\S+") ==2 ) %>% 
  group_by(taxon_kingdom_name) %>% 
  summarise("Número de observaciones"= n(), 
            "% Observaciones GI"= sum(quality_grade == "research" & 
                                        !is.na(taxon_species_name) & 
                                        taxon_species_name!="")/ n()*100, 
            "% Observaciones necesitan ID"=  sum(quality_grade =="needs_id")/
              n()*100,
            "Número de especies" = length(unique(scientific_name)))
