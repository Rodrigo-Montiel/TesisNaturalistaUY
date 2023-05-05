# PAQUETES Y DATOS--------------------------------------------------------------
library(tidyverse)

registros_de_tetrapodosuy <- read_csv('datos/registros_de_tetrapodos.uy.csv')


# TABLAS DE CONTINGENCIA--------------------------------------------------------

## TETRÁPODOS  

### Experticia vs tamaño
exp_tam <- aggregate(nivel ~ tamaño, 
                     data = registros_de_tetrapodosuy, FUN = table)

## Experticia vs distribución
exp_dist <- aggregate(nivel ~ distribucion_2, 
                      data = registros_de_tetrapodosuy, FUN = table)

## Experticia vs estado de conservacion
exp_stat <- aggregate(nivel ~ factor(status_global), 
                      data = registros_de_tetrapodosuy, FUN = table)

## PLANTAS

##Experticia vs estado de conservacion
exp_stat_p <- aggregate(nivel ~ factor(status_global), 
                         data = registros_de_plantasuy, FUN = table)

exp_hab1_p <- aggregate(nivel ~ Habito1,
                        data = registros_de_plantasuy, FUN = table)

# TEST CHI-CUADRADO-------------------------------------------------------------

## TETRÁPODOS  

### Experticia vs tamaño

exp_tam2 <- exp_tam[,-1]
rownames(exp_tam2) <- exp_tam[,1]
rownames(exp_tam2) <- c('Grande', 'Mediano', 'chico')
exp_tam2

chi_exp_tam <- chisq.test(exp_tam2)
chi_exp_tam


## PLANTAS

### Experticia vs Habito1
exp_hab1_p <- exp_hab1_p[,-1]
rownames(exp_hab1_p) <- exp_hab1_p[,1]
rownames(exp_hab1_p) <- c("Arbol", "Arbusto", "Enredadera", "Hierba", "Liana",
                          "NE","Subarbusto")
chi_exp_hab1_p <- chisq.test(exp_hab1_p)
chi_exp_hab1_p
