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

lista_dataset <- NatUY %>% st_drop_geometry() %>% 
  select(quality_grade, place_admin1_name, scientific_name,
         taxon_kingdom_name, taxon_phylum_name, 
         taxon_class_name,taxon_order_name, taxon_family_name, 
         taxon_genus_name) %>% filter(quality_grade == "research")



listado_especies <- lista_dataset %>% filter(!is.na(scientific_name)) %>% 
  filter(str_count(scientific_name, "\\S+") ==2 ) %>% 
  group_by(scientific_name)



listado_taxones <- listado_especies %>%  
  filter(taxon_kingdom_name=='Animalia' | taxon_kingdom_name=='Fungi' | 
           taxon_kingdom_name=='Plantae') %>% 
  group_by(taxon_kingdom_name, taxon_class_name) %>% count() %>% 
  arrange(desc(n)) %>% head(10) %>%  filter(!is.na(taxon_class_name)) %>% 
  ggplot(., aes(x=n, y=taxon_class_name, fill=taxon_class_name)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x='Number of Observations', y= '', fill = '') + theme_bw() +
  scale_x_continuous() + scale_fill_brewer(palette ='Spectral')

plot(listado_taxones)

