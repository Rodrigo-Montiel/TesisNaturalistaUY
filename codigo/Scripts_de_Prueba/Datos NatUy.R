# PAQUETES Y DATOS -------------------------------------------------------------
library(tmap)
library(sf)
sf::sf_use_s2(FALSE)
library(tidyverse)
library(patchwork)
library(stringr)

NatUY <- read_rds('datos/natuysf.rds')
Uruguay <- geouy::load_geouy("Dptos")

# ANALISIS ---------------------------------------------------------------------

## Cantidad de registros y usuarios

Cantidad_Registros <- nrow(NatUY)       # Cantidad de registros

Usuarios <- NatUY %>% st_drop_geometry() %>% 
  group_by(user_id) %>% 
  count() %>% arrange(desc(n))

Cantidad_Usuarios <- nrow(Usuarios)    # Cantidad de usuarios


## Cantidad de especies

Especies <- NatUY %>% st_drop_geometry() %>% 
  group_by (scientific_name) %>% 
  count() %>% arrange(desc(n)) %>% na.omit()

Grupos <- NatUY %>% st_drop_geometry() %>% 
  group_by (iconic_taxon_name) %>% 
  distinct(scientific_name) %>% count() %>% 
  arrange(desc(n)) %>% na.omit()

Cantidad_Especies <- nrow(Especies)    # Cantidad de especies (No es 
                                       # necesariamente el nombre científico, 
                                       # sino que toma el nivel taxonómico más 
                                       # bajo alcanzado por el registro)


## Registros identificados a nivel de especies

Nivel_Especie <- NatUY %>% st_drop_geometry() %>%  group_by(scientific_name) %>% 
  filter(!is.na(scientific_name)) %>% 
  filter(str_count(scientific_name, "\\S+") ==1 ) %>% 
  count() %>% arrange(desc(n))

Identificaciones_Nivel_Especie <- nrow(Nivel_Especie)


## Grado de investigacion

NatUY %>% NatUY %>% st_drop_geometry()(quality_grade) %>% count()

GI <- NatUY %>% st_drop_geometry() %>% filter(quality_grade == "research") %>% 
  group_by(quality_grade)
  
  
Grado_de_Investigacion <- nrow(GI)


Tabla1 <- data.frame(Cantidad_Registros, Cantidad_Usuarios, Cantidad_Especies, 
                     Grado_de_Investigacion, Identificaciones_Nivel_Especie)
