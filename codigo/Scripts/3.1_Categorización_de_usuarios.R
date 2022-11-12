# PAQUETES Y DATOS -------------------------------------------------------------
library(tmap)
library(sf)
sf::sf_use_s2(FALSE)
library(tidyverse)
library(patchwork)
library(stringr)
library(lubridate)

NatUY <- read_rds('datos/natuysf.rds')

# ANALISIS ---------------------------------------------------------------------

## Cantidad de registros y usuarios

Cantidad_Registros <- nrow(NatUY)       # Cantidad de registros

Usuarios <- NatUY %>% st_drop_geometry() %>% 
  group_by(user_id) %>% 
  count() %>% arrange(desc(n))

Cantidad_Usuarios <- nrow(Usuarios)    # Cantidad de usuarios


## Registros identificados a nivel de especies

Nivel_Especie <- NatUY %>% st_drop_geometry() %>%  
  group_by(scientific_name) %>% 
  filter(!is.na(scientific_name)) %>% 
  filter(str_count(scientific_name, "\\S+") ==1 ) %>% 
  count() %>% arrange(desc(n))

Identificaciones_Nivel_Especie <- nrow(Nivel_Especie)


## Grado de investigacion

GI <- NatUY %>% st_drop_geometry() %>% filter(quality_grade == "research") %>% 
  group_by(quality_grade)


Grado_de_Investigacion <- nrow(GI)


#CATEGORIZACIÓN DE USUARIOS ---------------------------------------------------

usuarios_login <- NatUY %>% st_drop_geometry() %>% 
  select(user_id, user_login) %>% group_by(user_id) %>% distinct()


usuarios_dataset <- NatUY %>% st_drop_geometry() %>% 
  select(user_id, observed_on, created_at) %>% 
  filter(year(observed_on)>=2000) %>% 
  group_by(user_id) %>% 
  summarise(primer_registro = min(created_at), ultimo_registro = max(created_at), 
            registros = n(), tiempo_activo = 
              difftime(ultimo_registro,primer_registro, 
                       units = "days")+1, 
            registros_x_tiempo = registros/as.numeric(tiempo_activo)) %>% 
  mutate(categoria_usuario = ifelse(registros>=1000 & tiempo_activo>=365 &
                                      registros_x_tiempo>=0.6, "experimentado",
                                    ifelse(registros>=50 & tiempo_activo>90 & 
                                             registros_x_tiempo>0.2,
                                           "intermedio", "principiante"))) %>% 
  merge(usuarios_login)



 ### Cantidad de usuarios por categorias:

 usuarios_dataset %>% group_by(categoria_usuario) %>% count()


saveRDS(usuarios_dataset, "datos/usuarios_dataset.rds")


## Gráfico

usuarios_registros <- usuarios_dataset %>% filter(tiempo_activo>=8) %>% 
  ggplot(aes(registros_tiempo)) + geom_histogram(binwidth = 1) + 
  scale_x_continuous()