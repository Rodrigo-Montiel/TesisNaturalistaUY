# PAQUETES Y DATOS -------------------------------------------------------------
library(tmap)
library(sf)
sf::sf_use_s2(FALSE)
library(tidyverse)
library(patchwork)
library(stringr)
library(lubridate)
library(forcats)

NatUY <- read.csv("datos/Observaciones_27-10-22.csv")

# ANALISIS ---------------------------------------------------------------------

## Cantidad de registros

Cantidad_Registros <- nrow(NatUY)       


## Registros identificados a nivel de especies

Nivel_Especie <- NatUY %>% st_drop_geometry() %>%  
  group_by(scientific_name) %>% 
  filter(!is.na(scientific_name)) %>% 
  filter(str_count(scientific_name, "\\S+") ==2 ) %>% 
  count() %>% arrange(desc(n))

Identificaciones_Nivel_Especie <- nrow(Nivel_Especie)


## Grado de investigacion

GI <- NatUY %>% st_drop_geometry() %>% filter(quality_grade == "research") %>% 
  group_by(quality_grade)


Grado_de_Investigacion <- nrow(GI)


Tabla1 <- data.frame(Cantidad_Registros, Grado_de_Investigacion,
                     Identificaciones_Nivel_Especie)



#LISTADO DE ESPECIES: GI + NIVEL ESPECIE

listado_especies <- NatUY %>% st_drop_geometry() %>% 
  select(quality_grade, place_admin1_name, taxon_species_name,
         taxon_kingdom_name, taxon_phylum_name, 
         taxon_class_name,taxon_order_name, taxon_family_name, 
         taxon_genus_name) %>% filter(quality_grade == "research" & 
                                        !is.na(taxon_species_name) & 
                                        taxon_species_name!="") %>% 
  distinct(taxon_species_name, .keep_all=T)


saveRDS(listado_especies,"datos/listado_especies")



###Grafico

grafico_especies <- listado_especies %>%  
  filter(taxon_kingdom_name=='Animalia' | taxon_kingdom_name=='Fungi' | 
           taxon_kingdom_name=='Plantae') %>% 
  group_by(taxon_kingdom_name, taxon_class_name) %>% count() %>% 
  arrange(desc(n)) %>% head(15) %>%  filter(!is.na(taxon_class_name)) %>% 
  ggplot(.,aes(x=n, y=taxon_class_name, 
                fill=taxon_class_name)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x='Number of species', y= '', fill = '') + theme_bw() +
  scale_x_continuous() 

plot(grafico_especies)



##Para Tetrápodos

Tetrapodos <- listado_especies %>% 
  filter(taxon_class_name == "Aves" | taxon_class_name == "Amphibia" | 
           taxon_class_name == "Mammalia" | taxon_class_name == "Reptilia")

write.csv(Tetrapodos,"datos/Lista_Tetrapodos.csv")


##Para Plantas

Magno <- listado_especies %>%  
  filter(taxon_class_name=='Magnoliopsida') %>% 
  group_by(taxon_order_name, taxon_family_name) %>% count() %>% 
  arrange(desc(n)) %>% head(20) %>%  filter(!is.na(taxon_family_name)) %>% 
  ggplot(., aes(x=n, y=taxon_family_name, fill=taxon_family_name)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x='Number of Observations', y= '', fill = '') + theme_bw() +
  scale_x_continuous()

Plantas <- listado_especies %>% 
  filter(taxon_family_name == "Fabaceae" | taxon_family_name == "Cactaceas" | 
           taxon_family_name == "Asteraceae"|taxon_family_name == "Solanaceas")

write.csv(Plantas,"datos/Lista_Plantas.csv")
