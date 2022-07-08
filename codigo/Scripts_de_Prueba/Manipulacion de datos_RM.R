library(tidyverse)

NatUY <- read_csv("datos/observations-248320.csv")

NatUY %>% 
  filter(taxon_class_name == "Amphibia") %>% 
  select(common_name, )

# filtrar los anfibios que no son Rhinella (AND &) (OR |)

NatUY %>% 
  filter(taxon_class_name == "Amphibia" & taxon_genus_name != "Rhinella") %>% 
  select(common_name, taxon_class_name)

# Agrupar los datos (group_by)

NatUY %>% 
  group_by(place_admin1_name) %>% 
  count() %>% 
  arrange(desc(n))

#Resumir la cantidad de registros por departamento

NatUY %>% 
group_by(place_admin1_name) %>% 
  count() %>% 
  summarise(n/count(NatUY)*100) %>% 
  arrange(desc(n))

install.packages("geouy")
library(geouy)
Uruguaydpto <- geouy::load_geouy("Dptos", folder = "datos")



