n_tetrapodos <- NatUY %>% st_drop_geometry() %>% 
  select(quality_grade, taxon_species_name, 
         taxon_kingdom_name, taxon_phylum_name,
         taxon_class_name,taxon_order_name, 
         taxon_family_name,taxon_genus_name) %>% 
  filter(taxon_class_name == "Aves" | taxon_class_name == "Amphibia" | 
           taxon_class_name == "Mammalia" | taxon_class_name == "Reptilia") %>% 
  filter(quality_grade == "research" & !is.na(taxon_species_name)) %>% 
  group_by(taxon_species_name) %>% count() %>% arrange(desc(n))

  