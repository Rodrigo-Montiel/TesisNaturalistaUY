# PAQUETES Y DATOS--------------------------------------------------------------
library(tidyverse)
library(sf)
sf::sf_use_s2(FALSE)
library(dplyr)

NatUY <- read_csv('datos/Tablas/Observaciones_27-10-22.csv')

# OBSERVACIONES EN NATURALISTAUY------------------------------------------------


## Cantidad de registros
NatUY %>% st_drop_geometry() %>% 
  filter(!is.na(taxon_kingdom_name)) %>% nrow()


## Cantidad de usuarios
NatUY %>% st_drop_geometry() %>% 
  filter(!is.na(taxon_kingdom_name)) %>% 
  group_by(user_id) %>% 
  count() %>% nrow()


## Registros con Grado de investigación
NatUY %>% st_drop_geometry() %>% 
  filter(!is.na(taxon_kingdom_name)) %>% 
  filter(quality_grade == "research") %>% 
  group_by(quality_grade) %>% nrow()


## Cantidad de Especies registradas
NatUY %>% st_drop_geometry() %>%
  group_by(scientific_name) %>% 
  filter(!is.na(taxon_kingdom_name)) %>% 
  filter(!is.na(scientific_name)) %>% 
  filter(str_count(scientific_name, "\\S+") ==2 ) %>% 
  count() %>% arrange(desc(n)) %>% nrow()


# REGISTROS POR REINOS----------------------------------------------------------

Tabla_reinos <- NatUY %>% st_drop_geometry() %>%  
  filter(!is.na(taxon_kingdom_name)) %>% 
  filter(str_count(scientific_name, "\\S+") ==2 ) %>% 
  group_by(taxon_kingdom_name) %>% 
  summarise("Número de observaciones"= n(), 
            "% Observaciones GI"= sum(quality_grade == "research" & 
                                        !is.na(taxon_species_name) & 
                                        taxon_species_name!="")/ n()*100, 
            "% Observaciones necesitan ID"=  sum(quality_grade =="needs_id")/
              n()*100,
            "Número de especies" = length(unique(scientific_name)))
