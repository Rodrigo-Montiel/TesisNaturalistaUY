library(janitor)
library(tidyverse)

registros_de_tetrapodosuy <- read_csv('datos/registros_de_tetrapodosuy.csv')


registros_de_tetrapodosuy %>% group_by(usuario)


## RANKING VS LARGO_CM
ggplot(registros_de_tetrapodosuy, aes(x=ranking, y=largo_cm, color = clase)) +
  geom_point() + facet_wrap(~nivel, scales = "free") +
  labs(x='Ranking',
       y='Largo (cm)') +
  theme_bw()

###tamaño de las aves
registros_de_tetrapodosuy %>% filter(clase=="Aves") %>% 
  group_by(largo_cm, nivel) %>%  count() %>% arrange(desc(n)) %>% head(50) %>% 
  ggplot(., aes(x=largo_cm, y=n)) + geom_bar(stat = "identity") + 
  facet_wrap(~nivel) + labs(x="Tamaño (cm)")

###tamaño de los mamiferos
registros_de_tetrapodosuy %>% filter(clase=="Mammalia") %>% 
  group_by(largo_cm, nivel) %>%  count() %>% arrange(desc(n)) %>% head(50) %>% 
  ggplot(., aes(x=largo_cm, y=n)) + geom_bar(width = 8, stat = "identity") + 
  facet_wrap(~nivel, scales = "free") + labs(x="Tamaño (cm)")



## RANKING VS DISTRIBUCIÓN
ggplot(registros_de_tetrapodosuy) +
  geom_bar(aes(y= distribucion, fill = nivel)) + 
  labs(x="Registros", y= 'Departamentos') +
  theme_bw()

   ### Achique el limite del eje y para poder ver los demas registros  
  ggplot(registros_de_tetrapodosuy) +
    geom_bar(aes(y= distribucion, fill = nivel)) + xlim(0,400) +
    labs(x="Registros", y= 'Departamentos') +
    theme_bw()

  
  ggplot(registros_de_tetrapodosuy) + 
    geom_bar(aes(y= clase), show.legend = T) +
    facet_wrap(~nivel, scales = "free") +
    theme_bw()
  