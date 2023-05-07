# PAQUETES Y DATOS--------------------------------------------------------------
library(tmap)
library(sf)
sf::sf_use_s2(FALSE)
library(tidyverse)
library(patchwork)
library(stringr)
library(lubridate)

NatUY <- read_rds('datos/natuysf.rds')


# CONTRIBUCIONES NATURALISTAUY-------------------------------------------------

## Cantidad de registros
Registros <- nrow(NatUY)


## Cantidad de Especies registradas
Especies <- NatUY %>% st_drop_geometry() %>%  
  group_by(scientific_name) %>% 
  filter(!is.na(scientific_name)) %>% 
  filter(str_count(scientific_name, "\\S+") ==2 ) %>% 
  count() %>% arrange(desc(n)) %>% nrow()


## Registros con Grado de investigacion
GI <- NatUY %>% st_drop_geometry() %>% filter(quality_grade == "research") %>% 
  group_by(quality_grade) %>% nrow()


## Cantidad de usuarios
Usuarios <- NatUY %>% st_drop_geometry() %>% 
  group_by(user_id) %>% 
  count() %>% arrange(desc(n)) %>% nrow()


ContribucionesNATUY <- data.frame(Registros, GI, Especies, Usuarios)
