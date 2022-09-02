library(readr)
library(sf)
library(tidyverse)

encuesta <- read_csv("datos/encuesta.csv") %>% 
  janitor::clean_names()

##Para ver las clases de cada columna
View(encuesta) 
str(encuesta) 
lapply(encuesta,class)

##Cambiar Edad y Frecuencia a Numerico

encuesta$edad <- as.numeric(encuesta$edad)
encuesta$frecuencia <- as.numeric(encuesta$frecuencia)

##Cambiar Afinidad,Formación y Uso de la Plataforma a Factor
encuesta$afinidad_naturaleza <- as.factor(encuesta$afinidad_naturaleza)
encuesta$afinidad_tecnologia <- as.factor(encuesta$afinidad_tecnologia)
encuesta$formacion <- as.factor(encuesta$formacion)
encuesta$uso_de_plataforma <- as.factor(encuesta$uso_de_plataforma)
encuesta$interes_naturaleza <- as.factor(encuesta$interes_naturaleza)

## Eliminar los NA
encuesta <- na.omit(encuesta)
encuesta %>% group_by(departamento) %>% count()

##Afinidad por la naturaleza +
## Contribucion de cada departamento
ggplot(encuesta) +
  geom_bar(aes(x=afinidad_naturaleza, fill=departamento), show.legend=F) + 
  scale_fill_brewer(palette = "Spectral") +
  labs(title = "Afinidad por la naturaleza",
       x = "Afinidad", y = "Participantes") + 
  facet_grid(~departamento) +
  theme_linedraw()

##Afinidad por la tecnologia +
## Contribucion de cada departamento

ggplot(encuesta) +
  geom_bar(aes(x=afinidad_tecnologia, fill=departamento), show.legend=F) + 
  labs(title = "Afinidad por la Tecnologia",
       x = "Afinidad", y = "Participantes") + 
  scale_fill_brewer(palette = "Spectral") +
  facet_grid(~departamento) +
  theme_linedraw()

##Frecuencia de salida a la Naturaleza +
ggplot(encuesta) +
  geom_bar(aes(x=frecuencia)) + 
  labs(title = "Frecuencia de salida a la Naturaleza",
       x = "Frecuencia", y = "Participantes") +
  theme_linedraw()
  
## Histograma de edad
ggplot(encuesta) +
  geom_histogram(aes(x=edad), bins = 15, fill='red') +
  labs(y='Frecuencia') +
  theme_linedraw()

## Density
ggplot(encuesta) +
  geom_density(aes(x=edad), col='red') +
  labs(y='Frecuencia') +
  theme_linedraw()
  
## Afinidad en función de la edad
ggplot(encuesta) + 
  geom_boxplot(aes(y=edad)) +
  facet_wrap(~afinidad_tecnologia)


## Afinidad en función de la edad
ggplot(encuesta) + 
  geom_boxplot(aes(x=afinidad_tecnologia, y=edad), show.legend = F) +
  scale_fill_brewer(palette = "Spectral") +
  facet_wrap(~departamento) +
  labs(y='Edad', x='Afinidad por la Tecnologia') +
  theme_linedraw()

## Estadísticas
summary(encuesta)
cor(encuesta$edad, as.numeric(encuesta$afinidad_tecnologia))


summary(lm(encuesta$edad ~ as.numeric(encuesta$afinidad_tecnologia)))
plot(encuesta$edad, encuesta$afinidad_tecnologia)



