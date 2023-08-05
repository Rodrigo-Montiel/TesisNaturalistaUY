# PAQUETES Y DATOS -------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(patchwork)
library(RColorBrewer)

registros_de_tetrapodosuy <- 
  read_csv("datos/Tablas finales/registros_de_tetrapodosuy.csv")
registros_de_plantasuy <- 
  read_csv("datos/Tablas finales/registros_de_plantasuy.csv")


# GRAFICOS PARA LA VISUALIZACION DE DATOS DE TETRAPODOS-------------------------

## GRUPOS
gruposT <- registros_de_tetrapodosuy %>% 
  group_by(nivel,clase) %>% count() %>% 
  ggplot(aes(x="", y=n, fill=clase)) +
  geom_bar(width = 0.5, stat = "identity", show.legend = T) + 
  labs(x="", y="Cantidad de registros", fill = "Clase") + 
  facet_grid(~nivel) + 
  scale_fill_brewer(palette = "YlOrRd") + 
  theme_bw()


## DISTRIBUCION
distribucionesT <- registros_de_tetrapodosuy %>% 
  group_by(nivel,distribucion_2) %>% count() %>% 
  ggplot(aes(x="", y=n, fill=distribucion_2)) +
  geom_bar(width = 0.5, stat = "identity", show.legend = T) + 
  labs(x="", y="Cantidad de registros", fill = "Distribución") + 
  facet_grid(~nivel) + 
  scale_fill_brewer(palette = "YlOrRd") + 
  theme_bw()


## TAMAÑO
tamañoT <- registros_de_tetrapodosuy %>% 
  group_by(nivel,tamaño) %>% count() %>% 
  ggplot(aes(x="", y=n, fill=tamaño)) +
  geom_bar(width = 0.5, stat = "identity", show.legend = T) + 
  labs(x="", y="Cantidad de registros", fill = "Tamaño") + 
  facet_grid(~nivel) + 
  scale_fill_brewer(palette = "YlOrRd") + 
  theme_bw()


## ESTADO DE CONSERVACIÓN
estadoT <- registros_de_tetrapodosuy %>% 
  mutate(status_global=factor
         (status_global,levels= c("LC","NT","VU","EN","CR","DD","NE"))) %>% 
  group_by(nivel,status_global) %>% count() %>% 
  ggplot(aes(x="", y=n, fill=status_global)) +
  geom_bar(width = 0.5, stat = "identity", show.legend = T) + 
  labs(x="", y="Cantidad de registros", fill = "Estado de Conservación") + 
  facet_grid(~nivel) + 
  scale_fill_brewer(palette = "YlOrRd") + 
  theme_bw()


# GRAFICOS PARA LA VISUALIZACION DE PLANTAS ------------------------------------

## GRUPOS
gruposP <- registros_de_plantasuy %>% 
  group_by(nivel,familia) %>% count() %>% 
  ggplot(aes(x="", y=n, fill=familia)) +
  geom_bar(width = 0.5, stat = "identity", show.legend = T) + 
  labs(x="", y="Cantidad de registros", fill = "Familia") + 
  facet_grid(~nivel) + 
  scale_fill_manual(values = c("#D0D1E6","#A6BDDB","#3690C0","#034E7B")) + 
  theme_bw()


## DISTRIBUCION
distribucionesT <- registros_de_plantasuy %>% filter(!is.na(distribucion_2)) %>% 
  group_by(nivel,distribucion_2) %>% count() %>% 
  ggplot(aes(x="", y=n, fill=distribucion_2)) +
  geom_bar(width = 0.5, stat = "identity", show.legend = T) + 
  labs(x="", y="Cantidad de registros", fill = "Distribución") + 
  facet_grid(~nivel) + 
  scale_fill_manual(values = c("#A6BDDB","#3690C0","#034E7B")) + 
  theme_bw()


## HABITO
tamañoT <- registros_de_plantasuy %>% filter(Habito1 != "NE") %>% 
  group_by(nivel,Habito1) %>% count() %>% 
  ggplot(aes(x="", y=n, fill=Habito1)) +
  geom_bar(width = 0.5, stat = "identity", show.legend = T) + 
  labs(x="", y="Cantidad de registros", fill = "Habito") + 
  facet_grid(~nivel) + 
  scale_fill_brewer(palette = "Blues") + 
  theme_bw()


## ESTADO DE CONSERVACIÓN
estadoT <- registros_de_plantasuy %>% filter(status_global != "EW") %>% 
  mutate(status_global=factor
         (status_global,levels=c("LC","NT","VU","EN","CR","DD","NE"))) %>%
  group_by(nivel,status_global) %>% count() %>% 
  ggplot(aes(x="", y=n, fill=status_global)) +
  geom_bar(width = 0.5, stat = "identity", show.legend = T) + 
  labs(x="", y="Cantidad de registros", fill = "Estado de Conservación") + 
  facet_grid(~nivel) + 
  scale_fill_brewer(palette = "Blues") + 
  theme_bw()