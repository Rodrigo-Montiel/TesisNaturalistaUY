lista_tetrapodos <- read.csv("datos/Lista_Tetrapodos_Actualizada.csv")
conservation_tetrapodos <- read.csv("datos/tetrapods_conservation_status.csv") 

registros_tetrapodos <- NatUY %>% st_drop_geometry() %>%  
  group_by(scientific_name) %>% 
  filter(!is.na(scientific_name)) %>% 
  filter(str_count(scientific_name, "\\S+") ==2 ) %>% 
  count() %>% arrange(desc(n)) %>% rename("taxon_species_name" = "scientific_name")


Semitetrapodos <- left_join(lista_tetrapodos,conservation_tetrapodos)
tetrapodos_final <- left_join(Semitetrapodos, registros_tetrapodos)

write.csv(tetrapodos_final, "datos/tetrapodos_final.csv")


##Graficos

tetrapodos_final %>% group_by(Distribution) %>% count() %>% ggplot() + 
  geom_histogram(aes(x=Distribution))
