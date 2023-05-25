# PAQUETES Y DATOS -------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(patchwork)
library(RColorBrewer)

registros_de_tetrapodosuy <- 
  read_csv("datos/Tablas finales/registros_de_tetrapodosuy.csv")
registros_de_plantasuy <- 
  read_csv("datos/Tablas finales/registros_de_plantasuy.csv")

# CANTIDAD DE USUARIOS Y REGISTROS----------------------------------------------

usuariosp <- registros_de_plantasuy %>% select(usuario,nivel)
usuariost <- registros_de_tetrapodosuy %>% select(usuario,nivel)

usuarios_unidos <- bind_rows(usuariosp,usuariost)
usuarios_unidos %>% group_by(nivel) %>% 
  summarise(Usuarios=n_distinct(usuario),Registros=n())

# GRAFICOS PARA LA VISUALIZACION DE DATOS DE TETRAPODOS-------------------------

## Registros expertos: 3454
## Registros intermedios: 7841
## Registros principiantes: 2087


# CLASES QUE REGISTRA CADA NIVEL DE USUARIO
clase.expertos <- registros_de_tetrapodosuy %>% 
  filter(nivel=="experimentado") %>% 
  group_by(clase) %>% count() %>% 
  ggplot(aes(x=clase, y=(n*100)/3454, fill=clase)) +
  geom_bar(width = 0.5, stat = "identity", show.legend = F) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Expertos", x="", y="Porcentaje de registros (%)") +
  scale_fill_manual(values = c("#990000","#EF6548","#FDBB84","#FDD49E"))

clase.intermedios <- registros_de_tetrapodosuy %>% 
  filter(nivel=="intermedio") %>% 
  group_by(clase) %>% count() %>% 
  ggplot(aes(x=clase, y=(n*100)/7841, fill=clase)) +
  geom_bar(width = 0.5, stat = "identity", show.legend = F) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Intermedios", x="Clases", y="") +
  scale_fill_manual(values = c("#990000","#EF6548","#FDBB84","#FDD49E"))

clase.principiantes <- registros_de_tetrapodosuy %>% 
  filter(nivel=="principiante") %>% 
  group_by(clase) %>% count() %>% 
  ggplot(aes(x=clase, y=(n*100)/2087, fill=clase)) +
  geom_bar(width = 0.5, stat = "identity", show.legend = F) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Principiantes", x="", y="") +
  scale_fill_manual(values = c("#990000","#EF6548","#FDBB84","#FDD49E"))

clase.expertos + clase.intermedios + clase.principiantes


## TAMAÑOS QUE REGISTRA CADA NIVEL DE USUARIO
tamaño.expertos <- registros_de_tetrapodosuy %>% 
  filter(nivel=="experimentado") %>% 
  group_by(tamaño) %>% count() %>% 
  ggplot(aes(x=tamaño, y=(n*100)/3454)) +
  geom_bar(width = 0.5, stat = "identity") + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Expertos", x="", y="Porcentaje de registros (%)") +
  theme_grey()

tamaño.intermedios <- registros_de_tetrapodosuy %>% 
  filter(nivel=="intermedio") %>% 
  group_by(tamaño) %>% count() %>% 
  ggplot(aes(x=tamaño, y=(n*100)/7841)) +
  geom_bar(width = 0.5, stat = "identity") + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Intermedios", x="Tamaño", y="") +
  theme_grey()

tamaño.principiantes <- registros_de_tetrapodosuy %>% 
  filter(nivel=="principiante") %>% 
  group_by(tamaño) %>% count() %>% 
  ggplot(aes(x=tamaño, y=(n*100)/2087)) +
  geom_bar(width = 0.5, stat = "identity") + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Principiantes", x="", y="") +
  theme_grey()

tamaño.expertos + tamaño.intermedios + tamaño.principiantes


## DISTRIBUCIÓNES QUE REGISTRA CADA NIVEL DE USUARIO
dist.expertos <- registros_de_tetrapodosuy %>% 
  filter(nivel=="experimentado") %>% 
  group_by(distribucion_2) %>% count() %>% 
  ggplot(aes(x=distribucion_2, y=(n*100)/3454)) +
  geom_bar(width = 0.5, stat = "identity") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Expertos", x="", y="Porcentaje de registros (%)") +
  theme_grey()

dist.intermedios <- registros_de_tetrapodosuy %>% 
  filter(nivel=="intermedio") %>% 
  group_by(distribucion_2) %>% count() %>% 
  ggplot(aes(x=distribucion_2, y=(n*100)/7841)) +
  geom_bar(width = 0.5, stat = "identity") + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Intermedios", x="N° de Departamentos", y="") +
  theme_grey()

dist.principiantes <- registros_de_tetrapodosuy %>% 
  filter(nivel=="principiante") %>% 
  group_by(distribucion_2) %>% count() %>% 
  ggplot(aes(x=distribucion_2, y=(n*100)/2087)) +
  geom_bar(width = 0.5, stat = "identity") + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Principiantes", x="", y="") +
  theme_grey()

dist.expertos + dist.intermedios + dist.principiantes


## ESTADOS DE CONSERVACION
ect.expertos <- registros_de_tetrapodosuy %>% 
  filter(nivel=="experimentado") %>% 
  group_by(status_global) %>% count() %>% 
  ggplot(aes(x=status_global, y=(n*100)/3454)) +
  geom_bar(width = 0.5, stat = "identity") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Expertos", x="", y="Porcentaje de registros (%)") +
  theme_grey()

ect.intermedios <- registros_de_tetrapodosuy %>% 
  filter(nivel=="intermedio") %>% 
  group_by(status_global) %>% count() %>% 
  ggplot(aes(x=status_global, y=(n*100)/7841)) +
  geom_bar(width = 0.5, stat = "identity") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Intermedios", x="Estado de Conservación", y="") +
  theme_grey()

ect.principiantes <- registros_de_tetrapodosuy %>% 
  filter(nivel=="principiante") %>% 
  group_by(status_global) %>% count() %>% 
  ggplot(aes(x=status_global, y=(n*100)/2087)) +
  geom_bar(width = 0.5, stat = "identity") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Principiantes", x="", y="") +
  theme_grey()

ect.expertos + ect.intermedios + ect.principiantes

# GRAFICOS PARA LA VISUALIZACION DE DATOS DE PLANTAS----------------------------

## Registros expertos: 1610
## Registros intermedios: 2030
## Registros principiantes: 1006

## FAMILIA QUE REGISTRA CADA NIVEL DE USUARIO
familia.expertos <- registros_de_plantasuy %>% 
  filter(nivel=="experimentado") %>% 
  group_by(familia) %>% count() %>% 
  ggplot(aes(x=familia, y=(n*100)/1610, fill=familia)) +
  geom_bar(width = 0.5, stat = "identity", show.legend = F) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Expertos", x="", y="Porcentaje de registros (%)") +
  scale_fill_manual(values = c("#034E7B","#3690C0","#A6BDDB","#D0D1E6"))
  

familia.intermedios <- registros_de_plantasuy %>% 
  filter(nivel=="intermedio") %>% 
  group_by(familia) %>% count() %>% 
  ggplot(aes(x=familia, y=(n*100)/2030, fill=familia)) +
  geom_bar(width = 0.5, stat = "identity", show.legend = F) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Intermedios", x="Familias", y="") +
  scale_fill_manual(values = c("#034E7B","#3690C0","#A6BDDB","#D0D1E6"))


familia.principiantes <- registros_de_plantasuy %>% 
  filter(nivel=="principiante") %>% 
  group_by(familia) %>% count() %>% 
  ggplot(aes(x=familia, y=(n*100)/1006, fill=familia)) +
  geom_bar(width = 0.5, stat = "identity", show.legend = F) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Principiantes", x="", y="") +
  scale_fill_manual(values = c("#034E7B","#3690C0","#A6BDDB","#D0D1E6"))
  

familia.expertos + familia.intermedios + familia.principiantes


## HABITOS QUE REGISTRA CADA NIVEL DE USUARIO
habito1.expertos <- registros_de_plantasuy %>% 
  filter(nivel=="experimentado") %>% filter(Habito1 != "NE") %>% 
  group_by(Habito1) %>% count() %>% 
  ggplot(aes(x=Habito1, y=(n*100)/1610)) +
  geom_bar(width = 0.5, stat = "identity") + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Expertos", x="", y="Porcentaje de registros (%)")

habito2.expertos <- registros_de_plantasuy %>% 
  filter(nivel=="experimentado") %>% filter(Habito2 != "NE") %>% 
  group_by(Habito2) %>% count() %>% 
  ggplot(aes(x=Habito2, y=(n*100)/1610)) +
  geom_bar(width = 0.5, stat = "identity") + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Expertos", x="", y="Porcentaje de registros (%)")


habito1.intermedios <- registros_de_plantasuy %>% 
  filter(nivel=="intermedio") %>% filter(Habito1 != "NE") %>% 
  group_by(Habito1) %>% count() %>% 
  ggplot(aes(x=Habito1, y=(n*100)/2030)) +
  geom_bar(width = 0.5, stat = "identity") + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Intermedios", x="Habito", y="")

habito2.intermedios <- registros_de_plantasuy %>% 
  filter(nivel=="intermedio") %>% filter(Habito2 != "NE") %>%
  group_by(Habito2) %>% count() %>% 
  ggplot(aes(x=Habito2, y=(n*100)/2030)) +
  geom_bar(width = 0.5, stat = "identity") + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Intermedios", x="Habito", y="")


habito1.principiantes <- registros_de_plantasuy %>% 
  filter(nivel=="principiante") %>% filter(Habito1 != "NE") %>% 
  group_by(Habito1) %>% count() %>% 
  ggplot(aes(x=Habito1, y=(n*100)/1006)) +
  geom_bar(width = 0.5, stat = "identity") + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Principiantes", x="", y="")

habito2.principiantes <- registros_de_plantasuy %>% 
  filter(nivel=="principiante") %>% filter(Habito2 != "NE") %>%
  group_by(Habito2) %>% count() %>% 
  ggplot(aes(x=Habito2, y=(n*100)/1006)) +
  geom_bar(width = 0.5, stat = "identity") + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Principiantes", x="", y="")


habito1.expertos + habito1.intermedios + habito1.principiantes

habito2.expertos + habito2.intermedios + habito2.principiantes


## DISTRIBUCIÓNES QUE REGISTRA CADA NIVEL DE USUARIO
distp.expertos <- registros_de_plantasuy %>% 
  filter(nivel=="experimentado") %>% 
  group_by(distribucion_2) %>% count() %>% 
  ggplot(aes(x=distribucion_2, y=(n*100)/1610)) +
  geom_bar(width = 0.5, stat = "identity") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Expertos", x="", y="Porcentaje de registros (%)") +
  theme_grey()

distp.intermedios <- registros_de_plantasuy %>% 
  filter(nivel=="intermedio") %>% 
  group_by(distribucion_2) %>% count() %>% 
  ggplot(aes(x=distribucion_2, y=(n*100)/2030)) +
  geom_bar(width = 0.5, stat = "identity") + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Intermedios", x="Distribución", y="") +
  theme_grey()

distp.principiantes <- registros_de_plantasuy %>% 
  filter(nivel=="principiante") %>% 
  group_by(distribucion_2) %>% count() %>% 
  ggplot(aes(x=distribucion_2, y=(n*100)/1006)) +
  geom_bar(width = 0.5, stat = "identity") + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Principiantes", x="", y="") +
  theme_grey()

distp.expertos + distp.intermedios + distp.principiantes


## ESTADOS DE CONSERVACION
ecp.expertos <- registros_de_plantasuy %>% 
  filter(nivel=="experimentado") %>% filter(status_global != "EW") %>% 
  group_by(status_global) %>% count() %>% 
  ggplot(aes(x=status_global, y=(n*100)/1610)) +
  geom_bar(width = 0.5, stat = "identity") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Expertos", x="", y="Porcentaje de registros (%)") +
  theme_grey()

ecp.intermedios <- registros_de_plantasuy %>% 
  filter(nivel=="intermedio") %>% filter(status_global != "EW") %>%
  group_by(status_global) %>% count() %>% 
  ggplot(aes(x=status_global, y=(n*100)/2030)) +
  geom_bar(width = 0.5, stat = "identity") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Intermedios", x="Estado de Conservacion", y="") +
  theme_grey()

ecp.principiantes <- registros_de_plantasuy %>% 
  filter(nivel=="principiante") %>% filter(status_global != "EW") %>%
  group_by(status_global) %>% count() %>% 
  ggplot(aes(x=status_global, y=(n*100)/1006)) +
  geom_bar(width = 0.5, stat = "identity") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(title = "Principiantes", x="", y="") +
  theme_grey()


ecp.expertos + ecp.intermedios + ecp.principiantes


# TABLAS RESULTADOS ------------------------------------------------------------

