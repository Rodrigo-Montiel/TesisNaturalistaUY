# PAQUETES Y DATOS--------------------------------------------------------------
library(tidyverse)
library(sf)
sf::sf_use_s2(FALSE)
library(ggplot2)
library(patchwork)
library(lubridate)
library(geouy)


NatUY <- read_csv("datos/Tablas/NatUY.csv")
Uruguay <- geouy::load_geouy("Dptos")
UY <- st_union(Uruguay) %>% st_cast()


# PROCESAMIENTO PREVIO DE DATOS-------------------------------------------------

## Convertir NatUY en un Simple Feature
NatUY_sf <- NatUY %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  st_set_crs(4326) %>% 
  st_transform(32721)

## Filtrado de registros: GI + Nivel Especie
listado_especies <- NatUY %>% 
  select(observed_on,quality_grade, place_admin1_name, 
         taxon_species_name,taxon_kingdom_name, taxon_phylum_name, 
         taxon_class_name,taxon_order_name, taxon_family_name, 
         taxon_genus_name,scientific_name) %>% 
  filter(quality_grade == "research" & !is.na(taxon_species_name) & 
           taxon_species_name!="")


saveRDS(NatUY_sf, "datos/Tablas/natuysf.rds")
saveRDS(listado_especies, "datos/Tablas/listado_especies.rds")


# ANALISIS COBERTURA ESPACIAL--------------------------------------------------

### Creamos una grilla para Uruguay
Grilla_UY <- st_make_grid(st_bbox(UY), 
                          cellsize = 25000, square = FALSE, 
                          crs= st_crs(Uruguay)) %>% 
  st_intersection(UY) %>% st_sf(gridID=1:length(.), geometry= .)


### Unimos la grilla con la tabla de especies filtrada 
NatUY_grilla <- st_join(Grilla_UY, listado_especies) %>% 
  group_by(gridID) %>% 
  summarise(registros=n(),
            especies=n_distinct(scientific_name)) %>% st_cast()


## Cantidad de Registros
plot_registros <- ggplot() +
  geom_sf(data=NatUY_grilla, aes(fill=log10(registros)),show.legend = T) +
  labs(fill="Acumulación de Registros") +
  scale_fill_gradient2(high = "#034E7B", low = "#D0D1E6") + 
  theme_bw()

## Cantidad de especies distintas registradas
plot_riqueza <- ggplot() +
  geom_sf(data=NatUY_grilla, aes(fill=log10(especies)),show.legend = T) + 
  labs(fill="Riqueza de registros") + 
  scale_fill_gradient2(high = "#990000", low = "#FDD49E") + 
  theme_bw()

plot_registros + plot_riqueza


## Cobertura por departamentos
cobertura_dep <- listado_especies %>% st_drop_geometry() %>% 
  group_by(place_admin1_name) %>% 
  summarise(registros=n(),especies=n_distinct(scientific_name)) %>% 
  arrange(desc(registros))


# ANALISIS COBERTURA TEMPORAL---------------------------------------------------

Temporal_Reinos <- listado_especies %>% st_drop_geometry() %>%
  filter(year(observed_on)>=2010) %>% 
  filter(!is.na(taxon_phylum_name)) %>% 
  filter(taxon_kingdom_name=='Plantae' | taxon_kingdom_name=='Animalia' | 
           taxon_kingdom_name=='Fungi') %>% 
  group_by(year(observed_on), taxon_kingdom_name, taxon_phylum_name) %>% 
  count() %>% 
  ggplot(aes(x=`year(observed_on)`, y= n, color=taxon_phylum_name), 
         group = taxon_kingdom_name) + 
  facet_wrap(taxon_kingdom_name~.,scales = "free",drop=TRUE, 
             ncol = 1, nrow = 3) +
  geom_line(size=1, show.legend = FALSE) + geom_point() +
  labs(x='Años', y='Registros', color = "Filos") +
  theme_bw()

### Registro temporal de Animalia por clases
Temporal_A <- listado_especies %>% st_drop_geometry() %>%
  filter(year(observed_on)>=2010) %>% 
  filter(!is.na(taxon_phylum_name)) %>% 
  filter(taxon_kingdom_name=='Animalia') %>% 
  group_by(year(observed_on), taxon_kingdom_name, taxon_phylum_name,
           taxon_class_name) %>% count() %>%
  ggplot(aes(x=`year(observed_on)`, y= n, color=taxon_phylum_name,)) + 
  geom_line(size=1, show.legend = TRUE) + geom_point() +
  scale_x_continuous(breaks= scales::pretty_breaks()) +
  facet_wrap(taxon_class_name~.,scales = "free",drop=TRUE, nrow = 3) +
  labs(x='Años', y='Registros', color = 'filo') +
  theme_bw()

### Registro temporal del Reino Plantae por clases
Temporal_P <- listado_especies %>% st_drop_geometry() %>%
  filter(year(observed_on)>=2010) %>% 
  filter(!is.na(taxon_phylum_name)) %>% 
  filter(taxon_kingdom_name=='Plantae') %>% 
  group_by(year(observed_on), taxon_kingdom_name, taxon_phylum_name, 
           taxon_class_name) %>% count() %>% 
  ggplot(aes(x=`year(observed_on)`, y= n, color=taxon_phylum_name,)) + 
  geom_line(size=1, show.legend = TRUE) + geom_point() +
  scale_x_continuous(breaks= scales::pretty_breaks()) +
  facet_wrap(taxon_class_name~.,scales = "free",drop=TRUE) +
  labs(x='Años', y='Registros', color = 'Filo') +
  theme_bw()

### Registro temporal del reino Fungi por clases
Temporal_F <- listado_especies %>% st_drop_geometry() %>%
  filter(year(observed_on)>=2010) %>% 
  filter(!is.na(taxon_phylum_name)) %>% 
  filter(taxon_kingdom_name=='Fungi') %>% 
  group_by(year(observed_on), taxon_kingdom_name, taxon_phylum_name,
           taxon_class_name) %>% count() %>% 
  ggplot(aes(x=`year(observed_on)`, y= n, color=taxon_phylum_name,)) + 
  geom_line(size=1, show.legend = TRUE) + geom_point() +
  scale_x_continuous(breaks= scales::pretty_breaks()) +
  facet_wrap(taxon_class_name~.,scales = "free",drop=TRUE) +
  labs(x='Años', y='Registros', color = 'filo') +
  theme_bw()


# ANALISIS COBERTURA TAXONÓMICA-------------------------------------------------

## 5 FILOS MAS REGISTRADOS
Taxon_Filos <- listado_especies %>% 
  filter(taxon_kingdom_name=='Animalia' | 
           taxon_kingdom_name=='Fungi' | 
           taxon_kingdom_name=='Plantae') %>% 
  group_by(taxon_kingdom_name, taxon_phylum_name) %>% 
  count() %>% arrange(desc(n)) %>% 
  filter(!is.na(taxon_phylum_name)) %>% head(5) %>%  
  ggplot(., aes(x=n, y= fct_reorder(taxon_phylum_name,n), 
                fill=taxon_kingdom_name)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  labs(title="Filos mas registrados",x='Registros', y= '', fill = 'Reino') +
  theme_bw() + scale_x_continuous()


## 10 CLASES MAS REGISTRADAS
Taxon_Clases <- listado_especies2 %>%  
  filter(taxon_kingdom_name=='Animalia' | taxon_kingdom_name=='Fungi' | 
           taxon_kingdom_name=='Plantae') %>% 
  group_by(taxon_kingdom_name, taxon_phylum_name, taxon_class_name) %>% 
  count() %>% arrange(desc(n)) %>% head(10) %>% 
  filter(!is.na(taxon_class_name)) %>% 
  ggplot(.,aes(x=n, y=fct_reorder(taxon_class_name,n), 
               fill=taxon_kingdom_name)) +
  geom_bar(stat = "identity", show.legend = T) +
  labs(title="Clases mas registradas", 
       x='Registros', y= '', fill = 'Reino') + theme_bw() +
  scale_x_continuous()


# TABLA DE REINOS

Tabla_reinos <- NatUY %>% st_drop_geometry() %>%  
  filter(taxon_kingdom_name=='Animalia' | 
           taxon_kingdom_name=='Fungi' | 
           taxon_kingdom_name=='Plantae') %>% 
  filter(str_count(scientific_name, "\\S+") ==2 ) %>% 
  group_by(taxon_kingdom_name) %>% 
  summarise("Número de observaciones"= n(),
            "% Observaciones GI"= sum(quality_grade == "research" & !is.na(taxon_species_name) & 
                                        taxon_species_name!="")/ n()*100, 
            "% Observaciones necesitan ID"=  sum(quality_grade =="needs_id")/
              n()*100,
            "Número de especies" = length(unique(scientific_name)))
