#CONTRIBUCIONES NATUY - CANTIDAD DE ESPECIES------------------------------------

## Cantidad de Especies registradas
Especies1 <- NatUY %>% st_drop_geometry() %>%
  group_by(scientific_name) %>% 
  filter(!is.na(scientific_name)) %>% 
  filter(str_count(scientific_name, "\\S+") ==2 ) %>% 
  count(n_distinct(scientific_name)) %>% arrange(desc(n)) %>% nrow()


#### Cantidad de Especies registradas sin str_count
Especies2 <- NatUY %>% st_drop_geometry() %>%
  group_by(scientific_name) %>% 
  filter(!is.na(scientific_name)) %>% 
  filter(scientific_name %in% unique(scientific_name)) %>% 
  count(n_distinct(scientific_name)) %>% arrange(desc(n)) %>% nrow()


# LISTADO DE ESPECIES-----------------------------------------------------------
## Filtrado de registros: GI + Nivel Especie
listado_especies <- NatUY %>% 
  select(observed_on,quality_grade, place_admin1_name, 
         taxon_species_name,taxon_kingdom_name, taxon_phylum_name, 
         taxon_class_name,taxon_order_name, taxon_family_name, 
         taxon_genus_name,scientific_name) %>% 
  filter(quality_grade == "research" & !is.na(taxon_species_name) & 
           taxon_species_name!="")

## Filtrado de registros: GI + Nivel Especie
listado_especies2 <- NatUY %>% 
  select(observed_on,quality_grade, place_admin1_name, 
         taxon_species_name,taxon_kingdom_name, taxon_phylum_name, 
         taxon_class_name,taxon_order_name, taxon_family_name, 
         taxon_genus_name,scientific_name) %>% 
  filter(quality_grade == "research" & !is.na(taxon_species_name) & 
           taxon_species_name!="") %>% 
  filter(str_count(scientific_name, "\\S+") ==2 )