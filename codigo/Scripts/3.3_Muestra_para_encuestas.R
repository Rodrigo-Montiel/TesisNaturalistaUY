# PAQUETES Y DATOS--------------------------------------------------------------
library(tmap)
library(sf)
sf::sf_use_s2(FALSE)
library(tidyverse)
library(patchwork)
library(stringr)
library(lubridate)


usuarios_uy <- read_csv("datos/usuarios_uy.csv")
usuarios_dataset <- read_rds("datos/usuarios_dataset.RDS")


# MUESTRAS----------------------------------------------------------------------

##Categoria: Principiantes
Principiantes <- usuarios_uy %>% filter(categoria_usuario=="principiante" & 
                              ultimo_registro>(today()-365)) %>% 
  filter(!user_login %in% Usuarios_encuestados$Principiantes) %>% 
  pull(user_login) %>% sample(., size=100)

saveRDS(Principiantes, "datos/usuarios_principiantes")


##Categoria: Intermedios
Intermedios <- usuarios_uy %>% filter(categoria_usuario=="intermedio") %>% 
  filter(!user_login %in% Usuarios_encuestados$Intermedios) %>% 
  pull(user_login) %>% sample(., size=40)

saveRDS(Intermedios, "datos/usuarios_intermedios")


##Categoria: Experimentados
Experimentados <- usuarios_uy %>% 
  filter(categoria_usuario=="experimentado") %>% 
  pull(user_login) %>% sample(., size=10)

saveRDS(Experimentados, "datos/usuarios_experimentados")


nueva_encuesta <- cbind(Experimentados, Intermedios, Principiantes)
view(nueva_encuesta)

write.csv(nueva_encuesta, "datos/nueva_encuesta.csv")


#NUEVA TANDA DE ENCUESTAS-------------------------------------------------------

nueva_encuesta <- read_csv("datos/nueva_encuesta.csv")
 ###Estos usuarios ya fueron encuestados



## Nuevos principiantes
Encuesta3 <- usuarios_uy %>% filter(categoria_usuario == "principiante") %>% 
  filter(!user_login %in% nueva_encuesta$principiantes) %>% 
  pull(user_login) %>% sample(., size=100)

write.csv(Encuesta3, "datos/Encuesta3.csv")




