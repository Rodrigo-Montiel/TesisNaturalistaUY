library(janitor)
library(tidyverse)
library(ggplot2)
library(tmap)
library(sf)

registros_de_tetrapodosuy <- read_csv('datos/registros_de_tetrapodosuy.csv')


registros_de_tetrapodosuy %>% group_by(usuario)

--------------------------------------------------------------------------------
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
  
   ###Boxplot
  ggplot(registros_de_tetrapodosuy) +
    geom_boxplot(aes(y= distribucion, x=ranking ), show.legend = F) + 
    labs(y="distribucion", x= 'ranking') + 
    facet_wrap(~nivel) + theme_bw()


#ESTATUS GLOBAL Y REGIONAL
  
  ggplot(registros_de_tetrapodosuy, 
         aes(x=fct_relevel(status_regional!="LC"),)) +
    geom_bar() +
    facet_wrap(~nivel, scales = 'free') +
    labs(x='National IUCN Conservation Status',
         y='Observer ranking') +
    theme_bw()

  
##ANALISIS ESTADISTICOS
  
#Regresion
lm(ranking~largo_cm, data = registros_de_tetrapodosuy) %>% summary()
