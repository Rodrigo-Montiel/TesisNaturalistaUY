library(lubridate)
library(extrafont)
library(forcats)
library(sf)
library(stringi)
library(tidyverse)

NatUY %>% arrange(desc(created_at)) %>% select(created_at)

#Numero de registros con GI y creados este año

NatUY22 <- NatUY %>% 
  filter(quality_grade == "research" & 
           between(observed_on, as.Date("2022-01-01"), 
                   as.Date("2022-12-31")))


## Mapa del numero de observaciones para cada departamento

left_join(Uruguay %>% mutate(place_admin1_name=tolower(nombre)), 
         NatUY %>% 
           group_by(place_admin1_name) %>% count() %>% 
           mutate(place_admin1_name=stri_trans_general(tolower(place_admin1_name), id = 'Latin-ASCII')), 
         by='place_admin1_name') %>% 
  ggplot() + 
  geom_sf(aes(fill=n)) +
  labs(x='', y= '', fill = 'Number of\nObservations') +
  theme_bw() +
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 10, type = "continuous")) +
  #theme(text=element_text(family='Calibri')) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 5)))


## Los 10 taxas con mas observaciones
NatUY %>% 
  filter(taxon_kingdom_name=='Animalia' | taxon_kingdom_name=='Fungi' | taxon_kingdom_name=='Plantae') %>% 
  group_by(taxon_kingdom_name, taxon_class_name) %>% count() %>% 
  arrange(desc(n)) %>% head(10) %>% 
  ggplot(., aes(x=n, y=fct_reorder(taxon_class_name, taxon_kingdom_name), fill=fct_reorder(taxon_class_name, taxon_kingdom_name))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x='Number of Observations', y= '', fill = '') +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 15000, 1000)) +
  scale_fill_brewer(palette ='Spectral') +
  #theme(text=element_text(family='Calibri')) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))
