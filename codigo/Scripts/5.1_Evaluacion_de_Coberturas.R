# PAQUETES Y DATOS--------------------------------------------------------------
library(tmap)
library(sf)
sf::sf_use_s2(FALSE)
library(tidyverse)
library(patchwork)
library(stringr)
library(lubridate)
library(plotly)

NatUY <- read_rds('datos/natuysf.rds')
Uruguay <- geouy::load_geouy("Dptos")
UY <- st_union(Uruguay) %>% st_cast()


# PROCESAMIENTO PREVIO DE DATOS-------------------------------------------------

## Convertir NatUY a sf (Esta tambien en el script 2.0)
NatUY_sf <- NatUY %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  st_set_crs(4326) %>% 
  st_transform(32721)

## Filtrado de NatUY - Listado_especies: GI + Nivel Especie
listado_especies <- NatUY_sf %>% 
  select(observed_on,quality_grade, place_admin1_name, 
         taxon_species_name,taxon_kingdom_name, taxon_phylum_name, 
         taxon_class_name,taxon_order_name, taxon_family_name, 
         taxon_genus_name,scientific_name) %>% 
  filter(quality_grade == "research" & !is.na(taxon_species_name) & 
           taxon_species_name!="")


## ANALISIS COBERTURA ESPACIAL--------------------------------------------------

### Grilla para Uruguay
Grilla_UY <- st_make_grid(st_bbox(UY), 
                          cellsize = 25000, square = FALSE, 
                          crs= st_crs(Uruguay)) %>% 
  st_intersection(UY) %>% st_sf(gridID=1:length(.), geometry= .)


NatUY_grilla <- st_join(Grilla_UY, listado_especies) %>% 
  group_by(gridID) %>% 
  summarise(Abundancia=n(),
            Riqueza=n_distinct(scientific_name)) %>% st_cast()

## Cobertura de Cantidad de Registros
plot_Abundancia <- ggplot() +
  geom_sf(data=NatUY_grilla, aes(fill=(Abundancia))) +
  ggtitle("N°de Registros en el pais") +
  scale_fill_fermenter(palette ='YlGnBu', direction = 1) + 
  theme_bw()

## Cobertura de cantidad de especies distintas registradas
plot_Riqueza <- ggplot() +
  geom_sf(data=NatUY_grilla, aes(fill=log(Riqueza))) + 
  ggtitle("Riqueza de especies registradas") + 
  scale_fill_fermenter(palette ='YlOrBr', direction = 1) + 
  theme_bw()


plot_Abundancia + plot_Riqueza


# ANALISIS COBERTURA TEMPORAL---------------------------------------------------

###Grafica de puntos
Temporal_point <- listado_especies %>% st_drop_geometry() %>% 
  filter(year(observed_on)>=2010) %>% 
  filter(!is.na(taxon_phylum_name)) %>% 
  filter(taxon_kingdom_name=='Plantae' | taxon_kingdom_name=='Animalia' | 
           taxon_kingdom_name=='Fungi') %>% 
  ggplot(aes(x=observed_on, y=taxon_phylum_name, color=taxon_kingdom_name)) +
  geom_point(show.legend = FALSE) +
  facet_grid(taxon_kingdom_name~., 
             scales = "free", space= 'free',drop=TRUE) +
  theme_bw() +
  theme(strip.text.y = element_text(size=10, angle=0)) +
  labs(title = 'Cobertura temporal de los principales filos registrados', 
       x='Año', y='', color = '')


## Grafica de lineas
Temporal_line <- listado_especies %>% st_drop_geometry() %>%
  filter(year(observed_on)>=2010) %>% 
  filter(!is.na(taxon_phylum_name)) %>% 
  filter(taxon_kingdom_name=='Plantae' | taxon_kingdom_name=='Animalia' | 
           taxon_kingdom_name=='Fungi') %>% 
  group_by(year(observed_on), taxon_kingdom_name) %>% count() %>%
  ggplot(aes(x=`year(observed_on)`, y= n, color=taxon_kingdom_name,), 
         group = taxon_kingdom_name) +
  geom_line(show.legend = TRUE) +
  theme_bw() +
  theme(strip.text.y = element_text(size=10, angle=0)) +
  labs(title = 'Cobertura temporal de los principales Reinos', 
       x='Años', y='Registros', color = '')

### Registro temporal de Cordados
Temporal_C <- listado_especies %>% st_drop_geometry() %>%
  filter(year(observed_on)>=2010) %>% 
  filter(!is.na(taxon_phylum_name)) %>% 
  filter(taxon_phylum_name=='Chordata') %>% 
  group_by(year(observed_on), taxon_class_name) %>% count() %>%
  ggplot(aes(x=`year(observed_on)`, y= n, color=taxon_class_name,)) +
  geom_line(show.legend = TRUE) +
  theme_bw() +
  theme(strip.text.y = element_text(size=10, angle=0)) +
  labs(title = 'Cobertura temporal de los Cordados', 
       x='Años', y='Registros', color = 'Clases')

### Registro temporal de Artrópodos
Temporal_A <- listado_especies %>% st_drop_geometry() %>%
  filter(year(observed_on)>=2010) %>% 
  filter(!is.na(taxon_phylum_name)) %>% 
  filter(taxon_phylum_name=='Arthropoda') %>% 
  group_by(year(observed_on), taxon_class_name) %>% count() %>%
  ggplot(aes(x=`year(observed_on)`, y= n, color=taxon_class_name,)) +
  geom_line(show.legend = TRUE) +
  theme_bw() +
  theme(strip.text.y = element_text(size=10, angle=0)) +
  labs(title = 'Cobertura temporal de los Artropodos', 
       x='Años', y='Registros', color = 'Clases')


# ANALISIS COBERTURA TAXONÓMICA-------------------------------------------------

## Animalia
Taxon_Animalia <- listado_especies %>% 
  filter(taxon_kingdom_name=="Animalia") %>% 
  group_by(taxon_kingdom_name, taxon_phylum_name,taxon_class_name) %>% 
  count() %>% 
  ggplot(aes(x='', y=n, fill= taxon_phylum_name)) +
  geom_bar(width = 0.5, stat = "identity", show.legend = T) + 
  labs(title = "Cobertura taxonómica de los registros de Animales",
       x="", y="Cantidad de registros",
       fill = "Filos") +
  scale_fill_brewer(palette="YlOrRd")

### Filo Chordata
Taxon_Chordata <- listado_especies %>% 
  filter(taxon_phylum_name=="Chordata") %>% 
  group_by(taxon_phylum_name,taxon_class_name) %>% 
  count() %>% 
  ggplot(aes(x='', y=n, fill=taxon_class_name)) +
  geom_bar(width = 0.5, stat = "identity", show.legend = T) + 
  labs(title = "Cobertura taxonómica de los registros de Cordados",
       x="", y="Cantidad de registros",
       fill = "Clases")
  
### Filo Arthropoda
Taxon_Arthropoda <- listado_especies %>% 
  filter(taxon_phylum_name=="Arthropoda") %>% 
  group_by(taxon_phylum_name,taxon_class_name) %>% 
  count() %>% 
  ggplot(aes(x='', y=n, fill=taxon_class_name)) +
  geom_bar(width = 0.5, stat = "identity", show.legend = T) + 
  labs(title = "Cobertura taxonómica de los registros de Artropodos",
       x="", y="Cantidad de registros",
       fill = "Clases")


## Plantae
Taxon_Plantae <- listado_especies %>% 
  filter(taxon_kingdom_name=="Plantae") %>% 
  group_by(taxon_kingdom_name, taxon_phylum_name,taxon_class_name) %>% 
  count() %>% 
  ggplot(aes(x='', y=n, fill= taxon_phylum_name)) +
  geom_bar(width = 0.5, stat = "identity", show.legend = T) + 
  labs(title = "Cobertura taxonómica de los registros de Plantas",
       x="", y="Cantidad de registros",
       fill = "Filos") +
  scale_fill_brewer(palette="PuBu")

### Tracheophytas
Taxon_Arthropoda <- listado_especies %>% 
  filter(taxon_phylum_name=="Tracheophyta") %>% 
  group_by(taxon_phylum_name,taxon_class_name) %>% 
  count() %>% 
  ggplot(aes(x='', y=n, fill=taxon_class_name)) +
  geom_bar(width = 0.5, stat = "identity", show.legend = T) + 
  labs(title = "Cobertura taxonómica de los registros de Traqueofitas",
       x="", y="Cantidad de registros",
       fill = "Clases")+
  scale_fill_brewer(palette="GnBu")


##Fungi
Taxon_Fungi <- listado_especies %>% 
  filter(taxon_kingdom_name=="Fungi") %>% 
  group_by(taxon_kingdom_name, taxon_phylum_name,taxon_class_name) %>% 
  count() %>% 
  ggplot(aes(x='', y=n, fill= taxon_phylum_name)) +
  geom_bar(width = 0.5, stat = "identity", show.legend = T) + 
  labs(title = "Cobertura taxonómica de los registros de Hongos",
       x="", y="Cantidad de registros",
       fill = "Filos") +
  scale_fill_brewer(palette="Greens")


## Top 10 Filos mas registrados
Taxon_Filos <- listado_especies %>% 
  filter(taxon_kingdom_name=='Animalia' | 
           taxon_kingdom_name=='Fungi' | 
           taxon_kingdom_name=='Plantae') %>% 
  group_by(taxon_kingdom_name, taxon_phylum_name) %>% 
  count() %>% arrange(desc(n)) %>% 
  filter(!is.na(taxon_phylum_name)) %>% head(10) %>%  
  ggplot(., aes(x=n, y= fct_reorder(taxon_phylum_name,n), 
                fill=taxon_kingdom_name)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  labs(x='Registros', y= '', fill = 'Reino') +
  theme_bw() + scale_x_continuous()


## Top 10 Clases mas registradas
Taxon_Clases <- listado_especies %>%  
  filter(taxon_kingdom_name=='Animalia' | taxon_kingdom_name=='Fungi' | 
           taxon_kingdom_name=='Plantae') %>% 
  group_by(taxon_kingdom_name, taxon_phylum_name, taxon_class_name) %>% 
  count() %>% arrange(desc(n)) %>% head(10) %>% 
  filter(!is.na(taxon_class_name)) %>% 
  ggplot(.,aes(x=n, y=fct_reorder(taxon_class_name,n), 
               fill=taxon_kingdom_name)) +
  geom_bar(stat = "identity", show.legend = T) +
  labs(x='N° de especies', y= '', fill = 'Filo') + theme_bw() +
  scale_x_continuous()