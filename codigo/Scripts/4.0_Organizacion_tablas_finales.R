# PAQUETES Y DATOS--------------------------------------------------------------
library(dplyr)
library(tmap)
library(sf)
sf::sf_use_s2(FALSE)
library(tidyverse)
library(patchwork)
library(stringr)
library(lubridate)

NatUY <- read.csv("datos/Observaciones_27-10-22.csv")
tetrapodos_final <- read.csv("datos/tetrpodos_final.csv")
observadoresUY <- read.csv("datos/usuarios_uy.csv")

# TABLAS------------------------------------------------------------------------

tabla_especies <- tetrapodos_final %>% mutate(Size..cm.=str_replace
                                          (Size..cm., ',', '.')) %>% 
  select(especie=taxon_species_name, clase=taxon_class_name,
         familia=taxon_family_name, distribucion=Distribution,
         largo_cm=Size..cm., status_global=IUCNglobal,
         status_regional=IUCNregional) %>% as_tibble() %>% 
  mutate(largo_cm=as.numeric(largo_cm))



tabla_registros <- NatUY %>% filter(quality_grade == "research" & 
                                      !is.na(taxon_species_name) & 
                                      taxon_species_name!="") %>% 
  filter(taxon_class_name == "Aves" | taxon_class_name == "Amphibia" |
           taxon_class_name == "Mammalia" | taxon_class_name == "Reptilia") %>% 
  select(id=id, observado=observed_on, user_id, latitude, longitude,
         place_admin1_name,especie=scientific_name)


tabla_usuarios <- observadoresUY %>% arrange(desc(registros)) %>% 
  mutate(ranking = 1:n()) %>% select(usuario=user_login, user_id,
                                            nivel=categoria_usuario,ranking) 


#UNIENDO TABLAS-----------------------------------------------------------------

## Registros + especies

registros_especies <- tabla_registros %>% group_by(especie) %>% 
  left_join(tabla_especies) %>% ungroup()

### le agregamos los usuarios

registros_de_tetrapodos <- registros_especies %>% group_by(user_id) %>% 
  left_join(tabla_usuarios) %>% ungroup()

write.csv(registros_de_tetrapodos, "datos/registros_de_tetrapodos.csv")

