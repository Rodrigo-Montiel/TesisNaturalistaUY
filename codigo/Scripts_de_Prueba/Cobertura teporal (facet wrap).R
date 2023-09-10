# FACET WRAP

#Plantas x clase
listado_especies %>% st_drop_geometry() %>%
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


#Animales x clase
listado_especies %>% st_drop_geometry() %>%
  filter(year(observed_on)>=2010) %>% 
  filter(!is.na(taxon_phylum_name)) %>% 
  filter(taxon_kingdom_name=='Animalia') %>% 
  group_by(year(observed_on), taxon_kingdom_name, taxon_phylum_name,
           taxon_class_name) %>% count() %>% 
  ggplot(aes(x=`year(observed_on)`, y= n, color=taxon_phylum_name,)) + 
  geom_line(size=1, show.legend = TRUE) + geom_point() +
  scale_x_continuous(breaks= scales::pretty_breaks()) +
  facet_wrap(taxon_class_name~.,scales = "free",drop=TRUE) +
  labs(x='Años', y='Registros', color = 'filo') +
  theme_bw()


#Hongos
listado_especies %>% st_drop_geometry() %>%
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


