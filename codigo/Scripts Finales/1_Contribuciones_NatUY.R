# PAQUETES Y DATOS--------------------------------------------------------------
library(tidyverse)
library(sf)
sf::sf_use_s2(FALSE)

NatUY <- read_csv('datos/Observaciones_27-10-22.csv')

# FILTRADO DE ESPECIES CASUALES-------------------------------------------------
##De la categorización de registros (4_categorizacion_de_registros.R) nos 
##encontramos con especies "casuales". Vamos a eliminarlas:

NatUY <- NatUY %>% filter(scientific_name!= "Anas platyrhynchos" | 
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

write.csv(NatUY,"datos/NatUY.csv")


# CONTRIBUCIONES NATURALISTAUY-------------------------------------------------

## Cantidad de registros
nrow(NatUY)


## Cantidad de Especies registradas
NatUY %>% st_drop_geometry() %>%
  group_by(scientific_name) %>% 
  filter(!is.na(scientific_name)) %>% 
  filter(str_count(scientific_name, "\\S+") ==2 ) %>% 
  count() %>% arrange(desc(n)) %>% nrow()


## Registros con Grado de investigacion
NatUY %>% st_drop_geometry() %>% filter(quality_grade == "research") %>% 
  group_by(quality_grade) %>% nrow()

### Especies GI
NatUY %>% st_drop_geometry() %>%  
  group_by(scientific_name) %>% 
  filter(quality_grade == "research") %>% 
  filter(!is.na(scientific_name)) %>% 
  filter(str_count(scientific_name, "\\S+") ==2 ) %>% 
  count() %>% arrange(desc(n)) %>% nrow()


## Registros que necesitan identificación

NatUY %>% st_drop_geometry() %>% filter(quality_grade == "needs_id") %>% 
  group_by(quality_grade) %>% nrow()

### Especies ID
NatUY %>% st_drop_geometry() %>%  
  group_by(scientific_name) %>% 
  filter(quality_grade == "needs_id") %>% 
  filter(!is.na(scientific_name)) %>% 
  filter(str_count(scientific_name, "\\S+") ==2 ) %>% 
  count() %>% arrange(desc(n)) %>% nrow()


## Cantidad de usuarios
NatUY %>% st_drop_geometry() %>% group_by(user_id) %>% 
  count() %>% arrange(desc(n)) %>% nrow()





