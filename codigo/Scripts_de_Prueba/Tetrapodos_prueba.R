##Uniendo Tablas

lista_tetrapodos <- read.csv("datos/Lista_Tetrapodos_Actualizada.csv")
conservation_tetrapodos <- read.csv("datos/tetrapods_conservation_status.csv") 

registros_tetrapodos <- NatUY %>% st_drop_geometry() %>%  
  group_by(scientific_name) %>% 
  filter(!is.na(scientific_name)) %>% 
  filter(str_count(scientific_name, "\\S+") ==2 ) %>% 
  count() %>% arrange(desc(n)) %>% rename
("taxon_species_name" = "scientific_name")


Semitetrapodos <- left_join(lista_tetrapodos,conservation_tetrapodos)
tetrapodos_final <- left_join(Semitetrapodos, registros_tetrapodos)

write.csv(tetrapodos_final, "datos/tetrapodos_final.csv")


## Procesamiento de tablas

tetrapodos_caracteres <- tetrapodos_final %>% mutate
(Size..cm.=str_replace(Size..cm., ',', '.')) %>% 
  select(especie=taxon_species_name, clase=taxon_class_name, 
         familia=taxon_family_name, distribucion=Distribution,
         largo_cm=Size..cm., status_global=IUCNglobal,
         status_regional=IUCNregional) %>% as_tibble() %>% 
  mutate(largo_cm=as.numeric(largo_cm))

saveRDS(tetrapodos_caracteres,"datos/tetrapodos_caracteres.rds")


## Graficos

