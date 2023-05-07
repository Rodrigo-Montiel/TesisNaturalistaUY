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
tetrapodos <- read.csv("datos/tetrapodos_final.csv")
plantas <- read.csv("datos/plants_conservation_status.csv")
observadoresUY <- read.csv("datos/usuarios_uy.csv")
observadoresEX <- read.csv("datos/usuarios_ex.csv")

# TABLAS------------------------------------------------------------------------

tabla_tetrapodos <- tetrapodos %>% mutate(Size..cm.=str_replace
                                          (Size..cm., ',', '.')) %>% 
  select(especie=taxon_species_name, clase=taxon_class_name,
         familia=taxon_family_name, distribucion=Distribution,
         largo_cm=Size..cm., status_global=IUCNglobal,
         status_regional=IUCNregional) %>% as_tibble() %>% 
  mutate(largo_cm=as.numeric(largo_cm))


tabla_plantas <- plantas %>% 
  select(especie=taxon_species_name, clase=taxon_class_name,
         familia=taxon_family_name, distribucion=Distribution,
         Habito1, Habito2, status_global=IUCNglobal) %>% as_tibble()


tabla_registros_tetrapodos <- NatUY %>% filter(quality_grade == "research" & 
                                      !is.na(taxon_species_name) & 
                                      taxon_species_name!="") %>% 
  filter(taxon_class_name == "Aves" | taxon_class_name == "Amphibia" |
           taxon_class_name == "Mammalia" | taxon_class_name == "Reptilia") %>%
  select(id=id, observado=observed_on,usuario=user_login, latitude, longitude,
         place_admin1_name,especie=scientific_name) %>% 
  filter(str_count(especie, "\\S+") ==2)


tabla_registros_plantas <- NatUY %>% 
  filter(quality_grade == "research" & !is.na(taxon_species_name) & 
           taxon_species_name!="") %>% 
  filter(taxon_order_name == "Fabales" | taxon_order_name == "Asterales" |
           taxon_order_name == "Solanales" | taxon_order_name == "Caryophyllales") %>%
  select(id=id, observado=observed_on,usuario=user_login, latitude, longitude,
         place_admin1_name,especie=scientific_name) %>% 
  filter(str_count(especie, "\\S+") ==2)


tabla_usuariosuy <- observadoresUY %>% arrange(desc(registros)) %>% 
  mutate(ranking = 1:n()) %>% select(usuario=user_login,
                                     nivel=categoria_usuario,ranking)

tabla_usuariosex <- observadoresEX %>% arrange(desc(registros)) %>% 
  mutate(ranking = 1:n()) %>% select(usuario=user_login, 
                                     nivel=categoria_usuario,ranking)


# UNIENDO TABLAS----------------------------------------------------------------

## TETRÁPODOS
### Registros + especies

registros_especies_tetrapodos <- left_join(tabla_registros_tetrapodos,
                                           tabla_tetrapodos)

### Registros + especies + usuarios

registros_de_tetrapodosuy <- left_join(registros_especies_tetrapodos, 
                                       tabla_usuariosuy) %>% na.omit()

registros_de_tetrapodosex <- left_join(registros_especies_tetrapodos, 
                                       tabla_usuariosex) %>% na.omit() %>% 
  mutate(nivel= ifelse(nivel=="", "visitante", "visitante"))

write.csv(registros_de_tetrapodosuy, "datos/registros_de_tetrapodosuy.csv")
write.csv(registros_de_tetrapodosex, "datos/registros_de_tetrapodosex.csv")


## PLANTAS
### Registros + especies

registros_plantas <- left_join(tabla_registros_plantas,tabla_plantas)

## Registros + especies + usuarios

registros_de_plantasuy <- left_join(registros_plantas, tabla_usuariosuy) %>% 
  na.omit()

registros_de_plantasex <- left_join(registros_plantas, tabla_usuariosex) %>%
  na.omit() %>% mutate(nivel= ifelse(nivel=="", "visitante", "visitante"))

## Limpiando los espacion en blanco
registros_de_plantasuy$Habito1 <- trimws(registros_de_plantasuy$Habito1)
registros_de_plantasuy$Habito2 <- trimws(registros_de_plantasuy$Habito2)

write.csv(registros_de_plantasuy, "datos/registros_de_plantasuy.csv")
write.csv(registros_de_plantasex, "datos/registros_de_plantasex.csv")


# CARACTERISTICAS ESPECIES-ESPECIFICAS------------------------------------------

## Vamos a pasar las caracteristicas que son cuantitativas a cualitativas

### Distribución Tetrapodos
registros_de_tetrapodosuy <- registros_de_tetrapodosuy %>% 
  mutate(distribucion_2 = ifelse(distribucion>=17,"Alta",
                  ifelse(distribucion>=6,"Media","Baja")))

registros_de_tetrapodosex <- registros_de_tetrapodosex %>% 
  mutate(distribucion_2 = ifelse(distribucion>=17,"Alta",
                                 ifelse(distribucion>=6,"Media","Baja")))

### Distribución Plantas
registros_de_plantasuy <- registros_de_plantasuy %>% 
  mutate(distribucion_2 = ifelse(distribucion>=17,"Alta",
                                 ifelse(distribucion>=6,"Media",
                                        ifelse(distribucion>=1,"Baja","NE"))))

registros_de_plantasex <- registros_de_plantasex %>% 
  mutate(distribucion_2 = ifelse(distribucion>=17,"Alta",
                                 ifelse(distribucion>=6,"Media",
                                        ifelse(distribucion>=1,"Baja","NE"))))

### Tamaño Tetrapodos
registros_de_tetrapodosuy <- 
  registros_de_tetrapodosuy %>% 
  mutate(tamaño = 
           ifelse(clase=="Mammalia" & largo_cm>=200, "Grande", 
                         ifelse(clase=="Mammalia" & largo_cm>=50, "Mediano", 
                                ifelse(clase=="Mammalia" & largo_cm<50, "Pequeño", 
                                       ifelse(clase=="Aves" & largo_cm>=50,"Grande", 
                                              ifelse(clase=="Aves" & largo_cm>=20,"Mediano", 
                                                     ifelse(clase=="Aves" & largo_cm<20,"Pequeño", 
                                                            ifelse(clase=="Reptilia" & largo_cm>=100, "Grande", 
                                                                   ifelse(clase=="Reptilia" & largo_cm>=50, "Mediano", 
                                                                          ifelse(clase=="Reptilia" & largo_cm<50,"Pequeño", 
                                                                                 ifelse(clase=="Amphibia" & largo_cm>=10, "Grande", 
                                                                                        ifelse(clase=="Amphibia" & largo_cm>=5,"Mediano","Pequeño"))))))))))))

registros_de_tetrapodosex <- 
  registros_de_tetrapodosex %>% 
  mutate(tamaño = 
           ifelse(clase=="Mammalia" & largo_cm>=200, "Grande", 
                  ifelse(clase=="Mammalia" & largo_cm>=50, "Mediano", 
                         ifelse(clase=="Mammalia" & largo_cm<50, "Pequeño", 
                                ifelse(clase=="Aves" & largo_cm>=50,"Grande", 
                                       ifelse(clase=="Aves" & largo_cm>=20,"Mediano", 
                                              ifelse(clase=="Aves" & largo_cm<20,"Pequeño", 
                                                     ifelse(clase=="Reptilia" & largo_cm>=100, "Grande", 
                                                            ifelse(clase=="Reptilia" & largo_cm>=50, "Mediano", 
                                                                   ifelse(clase=="Reptilia" & largo_cm<50,"Pequeño", 
                                                                          ifelse(clase=="Amphibia" & largo_cm>=10, "Grande", 
                                                                                 ifelse(clase=="Amphibia" & largo_cm>=5,"Mediano","Pequeño"))))))))))))

# FRECUENCIA DE ESPECIES--------------------------------------------------------

## Queremos calcular la frecuencia con la que esta presente algunas
## caracteristicas, asi podemos graficar estos valores

## TETRAPODOS
tetrapodos_frecuencia <- table(registros_de_tetrapodosuy$especie)
tetrapodos_frecuencia <- data.frame(tetrapodos_frecuencia)  %>% 
  select(especie=Var1, frequencia=Freq)

### Unimos las tablas
registros_de_tetrapodosuy <- 
  left_join(registros_de_tetrapodosuy,tetrapodos_frecuencia)

## PLANTAS
plantas_frecuencia <- table(registros_de_plantasuy$especie)
plantas_frecuencia <- data.frame(plantas_frecuencia)  %>% 
  select(especie=Var1, frequencia=Freq)

### Unimos las tablas
registros_de_plantasuy <- 
  left_join(registros_de_plantasuy, plantas_frecuencia)


## Habitos en plantas
habito1_frecuencia <- table(registros_de_plantasuy$Habito1)
habito1_frecuencia <- data.frame(habito1_frecuencia)  %>% 
  select(Habito1=Var1, Hab1_freq=Freq)

registros_de_plantasuy <- 
  left_join(registros_de_plantasuy, habito1_frecuencia)


habito2_frecuencia <- table(registros_de_plantasuy$Habito2)
habito2_frecuencia <- data.frame(habito2_frecuencia)  %>% 
  select(Habito2=Var1, Hab2_freq=Freq)

registros_de_plantasuy <- 
  left_join(registros_de_plantasuy, habito2_frecuencia)



# REORDENAMOS LAS COLUMNAS------------------------------------------------------

## TETRÁPODOS
registros_de_tetrapodosuy <- registros_de_tetrapodosuy %>% 
  select(id,observado,usuario,nivel,ranking,latitude,longitude,
         departamento = place_admin1_name,clase,familia,especie,frequencia,
         distribucion,distribucion_2,largo_cm,tamaño,
         status_regional,status_global)

registros_de_tetrapodosex <- registros_de_tetrapodosex %>% 
  select(id,observado,usuario,nivel,ranking,latitude,longitude,
         departamento = place_admin1_name,clase,familia,especie,distribucion,
         distribucion_2,largo_cm,tamaño,status_regional,status_global)

## PLANTAS
registros_de_plantasuy <- registros_de_plantasuy %>% 
  select(id,observado,usuario,nivel,ranking,latitude,longitude,
         departamento = place_admin1_name,clase,familia,especie,
         frequencia,distribucion,distribucion_2,Habito1,Hab1_freq,
         Habito2,Hab2_freq,status_global)

registros_de_plantasex <- registros_de_plantasex %>% 
  select(id,observado,usuario,nivel,ranking,latitude,longitude,
         departamento = place_admin1_name,clase,familia,especie,distribucion,
         distribucion_2,Habito1,Habito2,status_global)


# PARA GUARDAR------------------------------------------------------------------

write.csv(registros_de_tetrapodosuy, "datos/registros_de_tetrapodosuy.csv")
write.csv(registros_de_tetrapodosex, "datos/registros_de_tetrapodosex.csv")

write.csv(registros_de_plantasuy, "datos/registros_de_plantasuy.csv")
write.csv(registros_de_plantasex, "datos/registros_de_plantasex.csv")

