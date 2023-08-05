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


# CONTRIBUCIONES NATURALISTAUY + NIVELES DE USUARIO-----------------------------

## UNIMOS LA TABLA DE NATUY CON LA INFORMACION DE USUARIOS
##(sin filtrar los registros pero con la informacion de los niveles)

NatUY_full <- left_join(NatUY,observadoresUY)


# CANTIDAD DE REGISTROS Y ESPECIES TOTALES

## Registros: 
nrow(NatUY_full)

## Especies registradas:
NatUY_full %>%  st_drop_geometry() %>%  
  group_by(scientific_name) %>% 
  filter(!is.na(scientific_name)) %>% 
  filter(str_count(scientific_name, "\\S+") ==2 ) %>% 
  count() %>% arrange(desc(n)) %>% nrow()


# REGISTROS DE EXPERIMENTADOS

##Registros
NatUY_full %>% filter(categoria_usuario=="experimentado") %>% nrow()

##Especies
NatUY_full %>%  st_drop_geometry() %>%  
  group_by(scientific_name) %>% 
  filter(categoria_usuario=="experimentado") %>% 
  filter(!is.na(scientific_name)) %>% 
  filter(str_count(scientific_name, "\\S+") ==2 ) %>% 
  count() %>% arrange(desc(n)) %>% nrow()


# REGISTROS DE Intermedios

##Registros
NatUY_full %>% filter(categoria_usuario=="intermedio") %>% nrow()

##Especies
NatUY_full %>%  st_drop_geometry() %>%  
  group_by(scientific_name) %>% 
  filter(categoria_usuario=="intermedio") %>% 
  filter(!is.na(scientific_name)) %>% 
  filter(str_count(scientific_name, "\\S+") ==2 ) %>% 
  count() %>% arrange(desc(n)) %>% nrow()


# REGISTROS DE PRINCIPIANTES

##Registros
NatUY_full %>% filter(categoria_usuario=="principiante") %>% nrow()

##Especies
NatUY_full %>%  st_drop_geometry() %>%  
  group_by(scientific_name) %>% 
  filter(categoria_usuario=="principiante") %>% 
  filter(!is.na(scientific_name)) %>% 
  filter(str_count(scientific_name, "\\S+") ==2 ) %>% 
  count() %>% arrange(desc(n)) %>% nrow()


# REGISTROS DE NA

##Registros
NatUY_full %>% filter(is.na(categoria_usuario)) %>% nrow()

##Especies
NatUY_full %>%  st_drop_geometry() %>%  
  group_by(scientific_name) %>% 
  filter(is.na(categoria_usuario)) %>% 
  filter(!is.na(scientific_name)) %>% 
  filter(str_count(scientific_name, "\\S+") ==2 ) %>% 
  count() %>% arrange(desc(n)) %>% nrow()


# GRADO DE INVESTIGACION

##Registros
NatUY_full %>% st_drop_geometry() %>% filter(quality_grade == "research") %>% 
  group_by(quality_grade) %>% nrow()

##Especies
NatUY_full %>%  st_drop_geometry() %>%  
  group_by(scientific_name) %>% 
  filter(quality_grade == "research") %>% 
  filter(!is.na(scientific_name)) %>% 
  filter(str_count(scientific_name, "\\S+") ==2 ) %>% 
  count() %>% arrange(desc(n)) %>% nrow()







