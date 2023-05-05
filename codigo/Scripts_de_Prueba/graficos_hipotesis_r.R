library(janitor)
library(tidyverse)
library(ggplot2)
library(tmap)
library(sf)

registros_de_tetrapodosuy <- read_csv('datos/registros_de_tetrapodos.uy.csv')
registros_de_plantasuy <- read_csv("datos/registros_de_plantasuy.csv")


registros_de_tetrapodosuy %>% group_by(usuario)

# GRAFICOS PARA LA VISUALIZACION DE DATOS DE TETRAPODOS-------------------------

# CLASES QUE REGISTRA CADA NIVEL DE USUARIO
ggplot(registros_de_tetrapodosuy) + 
  geom_bar(aes(x= clase)) + 
  facet_wrap(~nivel, scales = "free") + 
  labs(y="N° de observaciones", x="") +
  theme_grey()

  ### saque aves para ver mejor las demas clases
  registros_de_tetrapodosuy %>% 
    filter(clase!="Aves") %>% 
    ggplot(., aes(x=clase,)) + 
    geom_bar(show.legend = F) + 
    facet_wrap(~nivel) + 
    labs(y="N° de observaciones", x="") + 
    theme_grey()
  
  
# RANKING VS LARGO_CM
ggplot(registros_de_tetrapodosuy, aes(x=ranking, y=largo_cm, color = nivel)) +
  geom_point() + facet_wrap(~clase, scales = "free") + xlim(0,200) +
  labs(x='Ranking',
       y='Largo (cm)') +
  theme_bw()

registros_de_tetrapodosuy %>% 
  group_by(largo_cm, nivel) %>%  count() %>% arrange(desc(n)) %>% ggplot() +
  geom_bar(aes(x= largo_cm, y=n), width = 50, stat = "identity") + 
  facet_wrap(~nivel, scales = "free") + 
  labs(y="N° de observaciones", x="largo(cm)") +
  theme_grey()

## Usando el tamaño categorico
ggplot(registros_de_tetrapodosuy) + 
  geom_bar(aes(x= tamaño)) + 
  facet_wrap(~nivel) + 
  labs(y="N° de observaciones", x="") +
  theme_grey()

###tamaño de las aves
registros_de_tetrapodosuy %>% filter(clase=="Aves") %>% 
  group_by(largo_cm, nivel) %>%  count() %>% arrange(desc(n)) %>% 
  ggplot(., aes(x=largo_cm, y=n)) + geom_bar(stat = "identity") + 
  facet_wrap(~nivel) + labs(x="Tamaño (cm)")

###tamaño de los mamiferos
registros_de_tetrapodosuy %>% filter(clase=="Mammalia") %>% 
  group_by(largo_cm, nivel) %>%  count() %>% arrange(desc(n)) %>% 
  ggplot(., aes(x=largo_cm, y=n)) + geom_bar(width = 5, stat = "identity") + 
  facet_wrap(~nivel, scales = "free") + labs(x="Tamaño (cm)")

###tamaño de los anfibios
registros_de_tetrapodosuy %>% filter(clase=="Amphibia") %>% 
  group_by(largo_cm, nivel) %>%  
  count() %>% arrange(desc(n)) %>% 
  ggplot(., aes(x=largo_cm, y=n)) + geom_bar(width = 0.5, stat = "identity") + 
  facet_wrap(~nivel, scales = "free") + labs(x="Tamaño (cm)")

###tamaño de los reptiles
registros_de_tetrapodosuy %>% filter(clase=="Reptilia") %>% 
  group_by(clase,largo_cm, nivel) %>%  
  count() %>% arrange(desc(n)) %>% 
  ggplot(., aes(x=largo_cm, y=n)) + 
  geom_bar(width= 2, stat = "identity") + 
  facet_wrap(~nivel, scales = "free") + labs(x="Tamaño (cm)")


# RANKING VS DISTRIBUCIÓN
ggplot(registros_de_tetrapodosuy) +
  geom_point(aes(y= distribucion, x=ranking, col = nivel)) +
  labs(x="Ranking", y= 'departamentos en los que se encuentra la especie') + 
  scale_y_continuous(breaks = seq(1, 19, by = 1)) +
  theme_bw()

   ### Achique el limite del eje y para poder ver los demas registros  
  ggplot(registros_de_tetrapodosuy) +
    geom_bar(aes(y= distribucion, fill = nivel), show.legend = F) + 
    xlim(0,200) +
    labs(x="Registros", y= 'departamentos en los que se encuentra la especie') + 
    facet_wrap(~nivel) + 
    scale_y_continuous(breaks = seq(1, 19, by = 1)) +
    theme_bw()

## Usando la distribucion categorica
  ggplot(registros_de_tetrapodosuy) + 
    geom_bar(aes(x= distribucion_2)) + 
    facet_wrap(~nivel, scales = "free") + 
    labs(y="N° de observaciones", x="") +
    theme_grey()

#ESTATUS GLOBAL Y REGIONAL
  
  ggplot(registros_de_tetrapodosuy, 
         aes(x=fct_relevel(status_regional!="LC"),)) +
    geom_bar() +
    facet_wrap(~nivel, scales = 'free') +
    labs(x='National IUCN Conservation Status',
         y='Observer ranking') +
    theme_bw()
  
# GRAFICOS PARA LA VISUALIZACION DE DATOS DE PLANTAS----------------------------

# FAMILIA QUE REGISTRA CADA NIVEL DE USUARIO
  ggplot(registros_de_plantasuy) + 
    geom_bar(aes(x= familia)) + 
    facet_wrap(~nivel, scales = "free") + 
    labs(y="N° de observaciones", x="") +
    theme_grey()
  
# EXPERTICIA VS DISTRIBUCIÓN
ggplot(registros_de_plantasuy) + 
  geom_bar(aes(x= distribucion_2)) + 
  facet_wrap(~nivel) + 
  labs(y="N° de observaciones", x="") +
  theme_grey()

# EXPERTICIA Y ESTADO DE CONSERVACIÓN
ggplot(registros_de_plantasuy %>% filter(!is.na(status_global))) + 
  geom_bar(aes(x=fct_relevel
               (status_global,'LC','NT','VU', 'EN', 'CR','DD', "EW", 'NE',))) + 
  facet_wrap(~nivel, scales = "free") +
  labs(y='N° de observaciones') +
  theme_bw()

# HABITO 1
ggplot(registros_de_plantasuy %>% filter(Habito1 != "NE")) + 
  geom_bar(aes(y= Habito1)) + 
  facet_wrap(~nivel) + 
  labs(y="N° de observaciones", x="") +
  theme_grey()

#HABITO 2
ggplot(registros_de_plantasuy %>% filter(Habito2 != "NE")) + 
  geom_bar(aes(y= Habito2)) + 
  facet_wrap(~nivel, scales = "free") + 
  labs(y="N° de observaciones", x="") +
  theme_grey()
