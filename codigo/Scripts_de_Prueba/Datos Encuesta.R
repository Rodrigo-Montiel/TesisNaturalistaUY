library(readr)
library(sf)
library(tidyverse)

 Encuesta <- read_csv("datos/Encuesta.csv")

##Para ver las clases de cada columna
 
 lapply(Encuesta,class)

##Cambiar Edad y Frecuencia a Numerico

Encuesta$Edad <- as.numeric(Encuesta$Edad)
Encuesta$Frecuencia <- as.numeric(Encuesta$Frecuencia)

## Eliminar los NA
Encuesta <- na.omit(Encuesta)

##Afinidad por la naturaleza +
## Contribucion de cada departamento
ggplot(Encuesta) +
  geom_bar(mapping = aes(x=Afinidad_Naturaleza, fill=Departamento)) + 
  labs(title = "Afinidad por la naturaleza",
       x = "Afinidad", y = "Participantes") + 
  scale_fill_brewer(palette = "Spectral")


##Afinidad por la tecnologia +
## Contribucion de cada departamento

ggplot(Encuesta) +
  geom_bar(mapping = aes(x=Afinidad_Tecnologia, fill=Departamento)) + 
  labs(title = "Afinidad por la Tecnologia",
       x = "Afinidad", y = "Participantes") + 
  scale_fill_brewer(palette = "Spectral")

##Frecuencia de salida a la Naturaleza +

ggplot(Encuesta) +
  geom_bar(mapping = aes(x=Frecuencia)) + 
  labs(title = "Frecuencia de salida a la Naturaleza",
       x = "Frecuencia", y = "Participantes")
  

