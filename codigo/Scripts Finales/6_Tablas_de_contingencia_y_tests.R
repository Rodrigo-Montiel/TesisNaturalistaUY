# PAQUETES Y DATOS--------------------------------------------------------------
library(tidyverse)

registros_de_tetrapodosuy <- 
  read.csv('datos/Tablas finales/registros_de_tetrapodos.uy.csv')
registros_de_plantasuy <- 
  read.csv("datos/Tablas finales/registros_de_plantasuy")


# CHI CUADRADO------------------------------------------------------------------

## TETRÁPODOS  

### Experticia vs tamaño
exp_tam <- aggregate(nivel ~ tamaño, 
                     data = registros_de_tetrapodosuy, FUN = table)

exp_tam <- exp_tam[,-1]
rownames(exp_tam) <- exp_tam[,1]
rownames(exp_tam) <- c('Grande', 'Mediano', 'chico')

chi_exp_tam <- chisq.test(exp_tam)
chi_exp_tam


### Experticia vs distribución
exp_dist <- aggregate(nivel ~ distribucion_2, 
                      data = registros_de_tetrapodosuy, FUN = table)

exp_dist <- exp_dist[,-1]
rownames(exp_dist) <- exp_dist[,1]
rownames(exp_dist) <- c('Alta', 'Baja', 'Media')

chi_exp_dist <- chisq.test(exp_dist)
chi_exp_dist


### Experticia vs estado de conservacion (ERROR)
exp_stat <- aggregate(nivel ~ factor(status_global), 
                      data = registros_de_tetrapodosuy, FUN = table)

exp_stat <- exp_stat[,-1]
rownames(exp_stat) <- exp_stat[,1]
rownames(exp_stat) <- c('CR', 'DD', 'EN',"LC","NE","NT","VU")

chi_exp_stat <- chisq.test(exp_stat)


## PLANTAS

### Experticia vs estado de conservacion (ERROR)
exp_stat_p <- aggregate(nivel ~ factor(status_global), 
                         data = registros_de_plantasuy, FUN = table)

exp_stat_p <- exp_stat_p[,-1]
rownames(exp_stat_p) <- exp_stat_p[,1]
rownames(exp_stat_p) <- c('CR', 'DD', 'EN',"EW","LC","NE","NT","VU")

chi_exp_stat_p <- chisq.test(exp_stat_P)


### Experticia vs habito
exp_hab1_p <- aggregate(nivel ~ Habito1,
                        data = registros_de_plantasuy, FUN = table)

exp_hab1_p <- exp_hab1_p[,-1]
rownames(exp_hab1_p) <- exp_hab1_p[,1]
rownames(exp_hab1_p) <- c("Arbol", "Arbusto", "Enredadera", "Hierba", "Liana",
                          "NE","Subarbusto")
chi_exp_hab1_p <- chisq.test(exp_hab1_p)
chi_exp_hab1_p


# REGRESIONES-------------------------------------------------------------------

## TETRAPODOS

### Distribucion
mod_distribucionT <- 
  lm(ranking ~ distribucion, data = registros_de_tetrapodosuy)

mod_distribucionT <- summary(mod_distribucionT)

### Tamaño
mod_tamañoT <- 
  lm(ranking ~ largo_cm, data = registros_de_tetrapodosuy)

mod_tamañoT <- summary(mod_tamañoT)

### Estatus
mod_statusT <- 
  lm(ranking ~ status_global, data = registros_de_tetrapodosuy)

summary(modelo_statusT)


## PLANTAS

### Distribucion
modelo_distribucionP <- 
  lm(ranking ~ distribucion, data = registros_de_plantasuy)

summary(modelo_distribucionP)

### Estatus
modelo_statusT <- 
  lm(ranking ~ status_global, data = registros_de_plantasuy)

summary(modelo_statusT)

### Habito
modelo_habitoT <- 
  lm(ranking ~ Habito1, data = registros_de_plantasuy)

summary(modelo_habitoT)


# TABLAS -----------------------------------------------------------------------

resul <- data.frame(modelo= c("modelo_distribucionT", "modelo_tamañoT"),
                    r2= c(modelo_distribucionT$r.squared,modelo_tamañoT$r.squared),
                    p.value= c(modelo_distribucionT$coefficients, modelo_tamañoT$coefficients))



