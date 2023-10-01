# PAQUETES Y DATOS--------------------------------------------------------------
library(tidyverse)
library(stargazer)


registros_de_tetrapodosuy <- 
  read.csv('datos/Tablas finales/registros_de_tetrapodos.uy.csv')
registros_de_plantasuy <- 
  read.csv("datos/Tablas finales/registros_de_plantasuy")


# REGRESIONES-------------------------------------------------------------------

## TETRAPODOS (T)

### Modelo con Distribución
mod_T1 <- 
  lm(ranking ~ distribucion, data = registros_de_tetrapodosuy)

summary(mod_T1)

### Modelo con Tamaño
mod_T2 <- 
  lm(ranking ~ largo_cm, data = registros_de_tetrapodosuy)

summary(mod_T2)

### Modelo con Estado de conservación
mod_T3 <- 
  lm(ranking ~ status_global, 
     data = registros_de_tetrapodosuy)

summary(mod_T3)

### Modelo con todos los atributos
mod_T4 <- 
  lm(ranking ~ distribucion + largo_cm + status_global, 
     data = registros_de_tetrapodosuy)

summary (mod_T4)

## UNIENDO LOS MODELOS EN UNA TABLA
Modelos_Tetra <- stargazer(mod_T1,mod_T2,mod_T3,mod_T4,
                           type = "html",
                           title = "Modelo atributos tetrápodos")



## PLANTAS (P)

### Modelo con distribucion
mod_P1 <- 
  lm(ranking ~ distribucion, data = registros_de_plantasuy)

summary(mod_P1)

### Modelo con habito de crecimiento
mod_P2 <- 
  lm(ranking ~ Habito1, data = registros_de_plantasuy)

summary(mod_P2)

### Modelo con estado de conservación
mod_P3 <- 
  lm(ranking ~ status_global, 
     data = registros_de_plantasuy)

summary(mod_P3)

### Modelo con todos los atributos
mod_P4 <- 
  lm(ranking ~ distribucion + Habito1 + status_global, 
     data = registros_de_plantasuy)

summary(mod_P4)


## UNIENDO LOS MODELOS EN UNA TABLA

Modelos_Plantas <- stargazer(mod_P1,mod_P2,mod_P3,mod_P4,
                               type = "html",
          title = "Modelo atriburos plantas")

