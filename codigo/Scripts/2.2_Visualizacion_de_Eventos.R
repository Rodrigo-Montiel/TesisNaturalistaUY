#Los 5 taxas mas observados en San Jose

SanJose_taxones <- NatUY_SanJose %>% 
  filter(taxon_kingdom_name=='Animalia' | taxon_kingdom_name=='Fungi' | taxon_kingdom_name=='Plantae') %>% 
  group_by(taxon_kingdom_name, taxon_class_name) %>% count() %>% 
  arrange(desc(n)) %>% head(5) %>%  
  ggplot(., aes(x=n, y=fct_reorder(taxon_class_name, taxon_kingdom_name), fill=fct_reorder(taxon_class_name, taxon_kingdom_name))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x='Number of Observations', y= '', fill = '') +
  theme_bw() +
  scale_x_continuous() +
  scale_fill_brewer(palette ='Spectral') +
  #theme(text=element_text(family='Calibri')) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

plot(SanJose_taxones)



#Los 5 taxas mas observados en Bella Union

BellaUnion_taxones <- NatUY_BellaUnion %>% 
  filter(taxon_kingdom_name=='Animalia' | taxon_kingdom_name=='Fungi' | taxon_kingdom_name=='Plantae') %>% 
  group_by(taxon_kingdom_name, taxon_class_name) %>% count() %>% 
  arrange(desc(n)) %>% head(5) %>%  
  ggplot(., aes(x=n, y=fct_reorder(taxon_class_name, taxon_kingdom_name), fill=fct_reorder(taxon_class_name, taxon_kingdom_name))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x='Number of Observations', y= '', fill = '') +
  theme_bw() +
  scale_x_continuous() +
  scale_fill_brewer(palette ='Spectral') +
  #theme(text=element_text(family='Calibri')) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

plot(BellaUnion_taxones)



# Los 5 taxas mas observados en Paysandú

Paysandu_taxones <- NatUY_Paysandu %>% 
  filter(taxon_kingdom_name=='Animalia' | taxon_kingdom_name=='Fungi' | taxon_kingdom_name=='Plantae') %>% 
  group_by(taxon_kingdom_name, taxon_class_name) %>% count() %>% 
  arrange(desc(n)) %>% head(5) %>%  
  ggplot(., aes(x=n, y=fct_reorder(taxon_class_name, taxon_kingdom_name), fill=fct_reorder(taxon_class_name, taxon_kingdom_name))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x='Number of Observations', y= '', fill = '') +
  theme_bw() +
  scale_x_continuous() +
  scale_fill_brewer(palette ='Spectral') +
  #theme(text=element_text(family='Calibri')) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

plot(Paysandu_taxones)

