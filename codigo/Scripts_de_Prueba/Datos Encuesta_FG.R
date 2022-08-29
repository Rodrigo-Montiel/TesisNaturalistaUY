library(readr)
library(sf)
library(tidyverse)

 Encuesta <- read_csv("datos/Encuesta.csv")

##Para ver las clases de cada columna
str(Encuesta) 
lapply(Encuesta,class)

##Cambiar Edad y Frecuencia a Numerico

Encuesta$Edad <- as.numeric(Encuesta$Edad)
Encuesta$Frecuencia <- as.numeric(Encuesta$Frecuencia)

## Eliminar los NA
Encuesta <- na.omit(Encuesta)
Encuesta %>% group_by(Departamento) %>% count()

##Afinidad por la naturaleza +
## Contribucion de cada departamento
ggplot(Encuesta) +
  geom_bar(aes(x=Afinidad_Naturaleza, fill=Departamento), show.legend=F) + 
  scale_fill_brewer(palette = "Spectral") +
  labs(title = "Afinidad por la naturaleza",
       x = "Afinidad", y = "Participantes") + 
  facet_grid(~Departamento) +
  theme_dark()

##Afinidad por la tecnologia +
## Contribucion de cada departamento

ggplot(Encuesta) +
  geom_bar(aes(x=Afinidad_Tecnologia, fill=Departamento), show.legend=F) + 
  labs(title = "Afinidad por la Tecnologia",
       x = "Afinidad", y = "Participantes") + 
  scale_fill_brewer(palette = "Spectral") +
  facet_grid(~Departamento) 

##Frecuencia de salida a la Naturaleza +

ggplot(Encuesta) +
  geom_bar(mapping = aes(x=Frecuencia)) + 
  labs(title = "Frecuencia de salida a la Naturaleza",
       x = "Frecuencia", y = "Participantes")
  
## Histograma de edad
ggplot(Encuesta) +
  geom_histogram(aes(x=Edad), bins = 10, fill='red') +
  labs(y='Frecuencia')
  
## Afinidad en función de la edad
ggplot(Encuesta) + 
  geom_boxplot(aes(y=Edad)) +
  facet_wrap(~Afinidad_Tecnologia)
