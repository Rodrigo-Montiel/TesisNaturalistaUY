# PAQUETES Y DATOS--------------------------------------------------------------
library(tmap)
library(sf)
sf::sf_use_s2(FALSE)
library(tidyverse)
library(patchwork)
library(stringr)
library(lubridate)


usuarios_dataset <- readRDS("datos/usuarios_dataset.rds")

# MUESTRAS----------------------------------------------------------------------

##Categoria: Principiantes
Principiantes <- usuarios_dataset %>% filter(categoria_usuario=="principiante" & 
                              ultimo_registro>(today()-182)) %>% 
  pull(user_login) %>% sample(., size=100)

saveRDS(Principiantes, "datos/usuarios_principiantes")


##Categoria: Intermedios
Intermedios <- usuarios_dataset %>% filter(categoria_usuario=="intermedio") %>% 
  pull(user_login) %>% sample(., size=40)

saveRDS(Intermedios, "datos/usuarios_intermedios")


##Categoria: Experimentados
Experimentados <- usuarios_dataset %>% 
  filter(categoria_usuario=="experimentado") %>% 
  pull(user_login) %>% sample(., size=10)

saveRDS(Experimentados, "datos/usuarios_experimentados")


Tabla_usuarios_encuestados <- 
  data.frame(Experimentados, Intermedios, Principiantes)


#PARA FILTRAR
Usuarios_encuestados <- read.csv("datos/Usuarios para encuestar.csv")


usuarios_dataset %>% filter(categoria_usuario=="principiante" & 
                              ultimo_registro>(today()-182)) %>% 
  filter(user_id %in% Usuarios_encuestados$Principiantes)

