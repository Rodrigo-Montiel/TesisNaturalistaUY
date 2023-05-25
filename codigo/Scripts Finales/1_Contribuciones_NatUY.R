# PAQUETES Y DATOS--------------------------------------------------------------
library(tidyverse)
library(sf)
sf::sf_use_s2(FALSE)

NatUY <- read_csv('datos/Observaciones_27-10-22.csv')

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
