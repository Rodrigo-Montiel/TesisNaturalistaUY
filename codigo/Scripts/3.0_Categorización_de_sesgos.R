# PAQUETES Y DATOS -------------------------------------------------------------
library(tmap)
library(sf)
sf::sf_use_s2(FALSE)
library(tidyverse)
library(patchwork)
library(stringr)
library(lubridate)

NatUY <- read_rds('datos/natuysf.rds')
Uruguay <- geouy::load_geouy("Dptos")
UY <- st_union(Uruguay) %>% st_cast()



## Grilla para Uruguay
Grilla_UY <- st_make_grid(st_bbox(UY), 
                          cellsize = 25000, square = FALSE, 
                          crs= st_crs(Uruguay)) %>% 
  st_intersection(UY) %>% st_sf(gridID=1:length(.), geometry= .)


## Convertir NatUY a sf
NatUY_sf <- NatUY %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  st_set_crs(4326) %>% 
  st_transform(32721)


## Para trabajar en los sesgos sin geometria
NatUY_dataset <- NatUY %>% st_drop_geometry() %>% 
  select(id, observed_on, user_id, user_login, created_at, updated_at, 
         quality_grade, num_identification_agreements, 
         num_identification_disagreements, positional_accuracy, 
         coordinates_obscured, place_admin1_name, scientific_name,
         taxon_kingdom_name, taxon_phylum_name, taxon_class_name, 
         taxon_order_name, taxon_family_name, taxon_genus_name)


# SESGOS------------------------------------------------------------------------

## Sesgo Espacial

NatUY_grilla <- st_join(Grilla_UY, NatUY_sf ) %>% 
  group_by(gridID) %>% 
  summarise(Abundancia=n(),
            Riqueza=n_distinct(scientific_name)) %>% st_cast()

plot_Abundancia <- ggplot() +
  geom_sf(data=NatUY_grilla, aes(fill=log(Abundancia)), show.legend = TRUE) +
  ggtitle("N°de Registros en el pais") +
  scale_fill_fermenter(palette ='YlGnBu', direction = 1) + 
  theme_bw()

plot_Riqueza <- ggplot() +
  geom_sf(data=NatUY_grilla, aes(fill=log(Riqueza)), show.legend = TRUE) + 
  ggtitle("Riqueza de especies registradas") + 
  scale_fill_fermenter(palette ='YlOrBr', direction = 1) + 
  theme_bw()


plot_Abundancia + plot_Riqueza


## Sesgo Temporal

NatUY_Temporal <- NatUY_dataset %>% 
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

### Ordenes de ANIMALIA con más de 50 registros

Temporal_Animalia <- NatUY_dataset %>% 
  filter(year(observed_on)>=2010) %>% 
  filter(!is.na(taxon_order_name)) %>% 
  filter(taxon_kingdom_name=='Animalia') %>% group_by(scientific_name) %>% 
  filter(n()>50) %>% ungroup() %>% 
  ggplot(aes(x=observed_on, y=taxon_order_name, color=taxon_class_name)) +
  geom_point(show.legend = FALSE) +
  facet_grid(taxon_class_name~., scales = "free", space= 'free_y', switch='x' ,
             drop=TRUE) +
  theme_bw() +
  theme(strip.text.y = element_text(size=10, angle=0)) +
  labs(title = 'Cobertura temporal de los ordenes del Reino Animalia con más de 50 registros en NaturalistaUY', 
       x='Año', y='Orden', color = '')

### Ordenes de PLANTAE con más de 50 registros

Temporal_Plantae <- NatUY_dataset %>% 
  filter(year(observed_on)>=2010) %>% 
  filter(!is.na(taxon_order_name)) %>% 
  filter(taxon_kingdom_name=='Plantae') %>% group_by(scientific_name) %>% 
  filter(n()>50) %>% ungroup() %>% 
  ggplot(aes(x=observed_on, y=taxon_order_name, color=taxon_class_name)) +
  geom_point(show.legend = FALSE) +
  facet_grid(taxon_class_name~., scales = "free", space= 'free_y', switch='x' ,
             drop=TRUE) +
  theme_bw() +
  theme(strip.text.y = element_text(size=10, angle=0)) +
  labs(title = 'Cobertura temporal de los ordenes del Reino Plantae con más de 50 registros en NaturalistaUY', 
       x='Año', y='Orden', color = '')



## Sesgo Taxonomico

NatUY_Taxones <- NatUY_dataset %>% 
  filter(taxon_kingdom_name=='Plantae'| taxon_kingdom_name=='Animalia'|
           taxon_kingdom_name=='Fungi') %>% 
  group_by(taxon_kingdom_name, taxon_phylum_name) %>% 
  count() %>% 
  ggplot(aes(x='', y=n, fill=taxon_phylum_name)) +
  geom_bar(width = 0.5, stat = "identity", show.legend = T) + 
  labs(title = "Cobertura taxonómica de los registros de NatutalistaUY",
       x="", y="Cantidad de registros",
       fill = "Filos") + facet_grid(~taxon_kingdom_name)

  ###Top 10 filos mas registrados
  NatUY_TopTaxones <- NatUY_dataset %>% 
    filter(taxon_kingdom_name=='Animalia' | taxon_kingdom_name=='Fungi' | 
           taxon_kingdom_name=='Plantae') %>% 
    group_by(taxon_kingdom_name, taxon_phylum_name) %>% count() %>% 
    arrange(desc(n)) %>% filter(!is.na(taxon_phylum_name)) %>% head(10) %>%  
    ggplot(., aes(x=n, y=taxon_phylum_name, fill=taxon_phylum_name)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    labs(x='Number of Observations', y= '', fill = '') +
    theme_bw() +
    scale_x_continuous()
  