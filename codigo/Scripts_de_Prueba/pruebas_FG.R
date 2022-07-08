library(tidyverse)

NatUY <- read_csv('datos/observations-248320.csv')

# Ver que esto no es lo mismo
a = 1 # asignando 1 al objeto a
a == 1 # es a igual a 1

NatUY %>% 
  filter(taxon_class_name == 'Amphibia') %>% # lo hace sobre el valor de la filas por columnas
  select(scientific_name, observed_on) # la hace sobre la columnas

# filtrar los anfibios que no son Rhinella (AND &, OR |)
NatUY %>% 
  filter(taxon_class_name == 'Amphibia' & taxon_genus_name!='Rhinella') # AND

NatUY %>% 
  filter(taxon_class_name == 'Amphibia' | taxon_class_name == 'Aves') # OR

# Para ver qué está haciendo el código puedo usar View() o print.data.frame()

# Cantidad de registros por clase taxonómica
NatUY %>% 
  group_by(taxon_class_name) %>% 
  count()

# cantidad de registros por departamento
NatUY %>% 
  group_by(place_admin1_name) %>% 
  count() %>% 
  arrange(desc(n))

# canrtidad de registros totales
NatUY %>% 
  count()

# porcentaje de registros por departamento
NatUY %>% 
  group_by(place_admin1_name) %>% 
  count() %>% 
  summarise(porcentaje=n/count(NatUY)*100) %>% 
  arrange(desc(porcentaje))

# Descargar la capa de departamentos de Uruguay (https://github.com/RichDeto/geouy)
Uruguay <- geouy::load_geouy('Dptos', folder = 'datos')

ggplot(data=Uruguay) + 
  geom_sf(aes(fill=areakm2)) + theme_bw()

NatUY %>% 
  group_by(place_admin1_name) %>% 
  count() %>% 
  ggplot(aes(x=place_admin1_name, y=n)) + 
  geom_bar(stat='identity')

