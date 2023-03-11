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
tetrapodos_final <- read.csv("datos/tetrapodos_final.csv")
observadoresUY <- read.csv("datos/usuarios_uy.csv")
observadoresEX <- read.csv("datos/usuarios_ex.csv")

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
  select(id=id, observado=observed_on,usuario=user_login, latitude, longitude,
         place_admin1_name,especie=scientific_name) %>% 
  filter(str_count(especie, "\\S+") ==2)


tabla_usuariosuy <- observadoresUY %>% arrange(desc(registros)) %>% 
  mutate(ranking = 1:n()) %>% select(usuario=user_login,
                                     nivel=categoria_usuario,ranking)

tabla_usuariosex <- observadoresEX %>% arrange(desc(registros)) %>% 
  mutate(ranking = 1:n()) %>% select(usuario=user_login, 
                                     nivel=categoria_usuario,ranking)





#UNIENDO TABLAS-----------------------------------------------------------------

## Registros + especies

registros_especies <- left_join(tabla_registros,tabla_especies)

## Registros + especies + usuarios

registros_de_tetrapodosuy <- left_join(registros_especies, tabla_usuariosuy) %>% 
  na.omit()

registros_de_tetrapodosex <- left_join(registros_especies, tabla_usuariosex) %>%
  na.omit() %>% mutate(nivel= ifelse(nivel=="", "visitante", "visitante"))


