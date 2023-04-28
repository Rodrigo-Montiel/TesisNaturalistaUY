# PAQUETES Y DATOS--------------------------------------------------------------
library(tidyverse)

registros_de_tetrapodosuy <- read_csv('datos/registros_de_tetrapodos.uy.csv')


# TABLAS DE CONTINGENCIA--------------------------------------------------------

## TETRÁPODOS  

### Experticia vs tamaño
exp_tam <- aggregate(tamaño ~ nivel, 
                     data = registros_de_tetrapodosuy, FUN = table)

## Experticia vs distribución
exp_dist <- aggregate(distribucion_2 ~ nivel, 
                      data = registros_de_tetrapodosuy, FUN = table)

## Experticia vs estado de conservacion
exp_stat <- aggregate(factor(status_global) ~ nivel, 
                      data = registros_de_tetrapodosuy, FUN = table)

## PLANTAS


# TEST CHI-CUADRADO-------------------------------------------------------------

## TETRÁPODOS  

### Experticia vs tamaño

observed_exp_tam <- matrix(c(619, 1499, 1336, 1361, 3333, 2868, 435, 802, 714), 
                       nrow = 3, ncol = 3, byrow = T)
rownames(observed_exp_tam) <- c('experimentado', 'intermedio', "principiante")
colnames(observed_exp_tam) <- c('Grande', 'Mediano', 'chico')
observed_exp_tam

chisq.test(observed_exp_tam)
chisq.test(observed_tam)$expected

