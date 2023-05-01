# PAQUETES Y DATOS--------------------------------------------------------------
library(tidyverse)

registros_de_tetrapodosuy <- read_csv('datos/registros_de_tetrapodos.uy.csv')


# TABLAS DE CONTINGENCIA--------------------------------------------------------

## TETRÁPODOS  

### Experticia vs tamaño
exp_tam <- aggregate(nivel ~ tamaño, 
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

##matrix(c(619, 1499, 1336, 1361, 3333, 2868, 435, 802, 714), 
                       #nrow = 3, ncol = 3, byrow = T)
exp_tam2 <- exp_tam[,-1]
rownames(exp_tam2) <- exp_tam[,1]
rownames(exp_tam2) <- c('Grande', 'Mediano', 'chico')
exp_tam2

chi_exp_tam2 <- chisq.test(exp_tam2)
chi_exp_tam2$expected

### probando con una sola categoria 
###(experimentados)

observed_expertos <- matrix(c(619, 1499, 1336),
                            nrow = 3, ncol = 1, byrow = T)
colnames(observed_expertos) <- c('experimentado')
rownames(observed_expertos) <- c('Grande', 'Mediano', 'chico')
observed_expertos

chisq.test(observed_expertos)
chisq.test(observed_expertos)$expected

### probando con una sola categoria 
###(intermedios)

observed_intermedios <- matrix(c(1361, 3333, 2868),
                            nrow = 1, ncol = 3, byrow = T)
rownames(observed_intermedios) <- c('intermedio')
colnames(observed_intermedios) <- c('Grande', 'Mediano', 'chico')
observed_intermedios

chisq.test(observed_intermedios)

### probando con una sola categoria 
###(principiantes)
observed_principiantes <- matrix(c(435, 802, 714),
                               nrow = 1, ncol = 3, byrow = T)
rownames(observed_principiantes) <- c('intermedio')
colnames(observed_principiantes) <- c('Grande', 'Mediano', 'chico')
observed_principiantes

chisq.test(observed_intermedios)
