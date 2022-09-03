# PAQUETES Y DATOS ----------------------------------------------------------
library(RColorBrewer)
library(sf)
library(tidyverse)
Encuesta <- read_csv("datos/Encuesta.csv")


# LIMPIEZA DE DATOS ---------------------------------------------------------

# Si se quiere comprobar las clases de cada columna
lapply(Encuesta,class)
str(Encuesta)


# Cambiar la clase de "Edad" y "Frecuencia" (caracter a numérico)
Encuesta$Edad <- as.numeric(Encuesta$Edad)
Encuesta$Frecuencia <- as.numeric(Encuesta$Frecuencia)

   ## Eliminar los NA
   Encuesta <- na.omit(Encuesta)


# Estadisticas
summary(Encuesta)


# GRAFICOS ------------------------------------------------------------------

# Histograma de edades
ggplot(Encuesta) +
  geom_histogram(aes(x=Edad), bins = 10, fill= "skyblue", col= "white") + 
  theme_gray()


# Afinidad por la naturaleza en cada departamento
ggplot(Encuesta) +
  geom_bar(mapping = aes(x=Afinidad_Naturaleza, fill=Departamento),  
           show.legend = F,) + 
  scale_fill_brewer(palette = "Spectral") +
  labs(title = "Afinidad por la naturaleza",
       x = "Afinidad", y = "Participantes") + 
  facet_grid(~Departamento) +
  theme_gray()

   ## Afinidad por la naturaleza en función de la edad
   ggplot(Encuesta) + 
     geom_boxplot(aes(x=Afinidad_Naturaleza, y=Edad, fill=Departamento),
                  show.legend = F) +
     scale_fill_brewer(palette = "Spectral") +
     facet_wrap(~Departamento) +
     xlim(1,5) +
     labs(y='Edad', x='Afinidad por la Naturaleza') +
     theme_gray()


# Afinidad por la tecnologia en cada departamento
ggplot(Encuesta) +
  geom_bar(mapping = aes(x=Afinidad_Tecnologia, fill=Departamento),
           show.legend = F,) + 
  labs(title = "Afinidad por la Tecnologia",
       x = "Afinidad", y = "Participantes") + 
  scale_fill_brewer(palette = "Spectral") + 
  facet_grid(~Departamento) +
  theme_gray()

   ## Afinidad por la tecnologia en función de la edad
   ggplot(Encuesta) + 
     geom_boxplot(aes(x=Afinidad_Tecnologia, y=Edad, fill=Departamento),
                  show.legend = F) +
     scale_fill_brewer(palette = "Spectral") +
     facet_wrap(~Departamento) +
     labs(y='Edad', x='Afinidad por la Tecnologia') +
     theme_gray()


# Frecuencia de salida a la Naturaleza 
ggplot(Encuesta) +
  geom_bar(mapping = aes(x=Frecuencia, fill=Departamento), show.legend = F) + 
  labs(title = "Frecuencia de salida a la Naturaleza",
       x = "Frecuencia", y = "Participantes") + 
  scale_fill_brewer(palette = "Spectral") + 
  facet_wrap(~Departamento) +
  theme_gray()

   ## Frecuencia de salidas en funcion de la edad
   ggplot(Encuesta) + 
     geom_boxplot(aes(x=Frecuencia, y=Edad, fill=Departamento),
                  show.legend = F) +
     scale_fill_brewer(palette = "Greens") +
     facet_wrap(~Departamento) +
     labs(y='Edad', x='Frecuencia de salidas') +
     theme_gray()