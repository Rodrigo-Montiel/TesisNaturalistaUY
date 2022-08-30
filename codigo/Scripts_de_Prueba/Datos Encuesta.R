# Paquetes y datos
library(readr)
library(sf)
library(tidyverse)
Encuesta <- read_csv("datos/Encuesta.csv")

 
# Para ver las clases de cada columna
  lapply(Encuesta,class)

 
# Cambiar Edad y Frecuencia a Numerico
Encuesta$Edad <- as.numeric(Encuesta$Edad)
Encuesta$Frecuencia <- as.numeric(Encuesta$Frecuencia)


# Eliminar los NA
Encuesta <- na.omit(Encuesta)


# Histograma de edades
ggplot(Encuesta) +
  geom_histogram(aes(x=Edad), bins = 10, fill= "skyblue", col= "white") + 
  theme_classic()


# Afinidad por la naturaleza en cada departamento
ggplot(Encuesta) +
  geom_bar(mapping = aes(x=Afinidad_Naturaleza, fill=Departamento),  
           show.legend = F,) + 
  labs(title = "Afinidad por la naturaleza",
       x = "Afinidad", y = "Participantes") + 
  scale_fill_brewer(palette = "Spectral") + 
  facet_grid(~Departamento) +
  theme_classic()

## Afinidad por la naturaleza en función de la edad
ggplot(Encuesta) + 
  geom_boxplot(aes(y=Edad)) +
  facet_wrap(~Afinidad_Naturaleza)



# Afinidad por la tecnologia en cada departamento
ggplot(Encuesta) +
  geom_bar(mapping = aes(x=Afinidad_Tecnologia, fill=Departamento)) + 
  labs(title = "Afinidad por la Tecnologia",
       x = "Afinidad", y = "Participantes") + 
  scale_fill_brewer(palette = "Spectral") + 
  facet_grid(~Departamento) +
  theme_classic()

## Afinidad por la tecnologia en función de la edad
ggplot(Encuesta) + 
  geom_boxplot(aes(y=Edad)) +
  facet_wrap(~Afinidad_Tecnologia)


# Frecuencia de salida a la Naturaleza 
ggplot(Encuesta) +
  geom_bar(mapping = aes(x=Frecuencia, fill=Departamento), show.legend = F) + 
  labs(title = "Frecuencia de salida a la Naturaleza",
       x = "Frecuencia", y = "Participantes") + 
  scale_fill_brewer(palette = "Spectral") + 
  facet_wrap(~Departamento) +
  theme_classic()

