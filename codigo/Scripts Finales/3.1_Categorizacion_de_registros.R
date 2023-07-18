# PAQUETES Y DATOS--------------------------------------------------------------
library(tidyverse)
library(sf)
sf::sf_use_s2(FALSE)
library(lubridate)


listado_especies <- 
  readRDS("~/GitHub/TesisNaturalistaUY/datos/listado_especies.rds")

# FILTRADO DE ESPECIES----------------------------------------------------------

## Vamos a conformar una lista con todas las especies de interes registradas 
## dentro de tetrapodos y otra lista con las especies de Traqueofitas 

Tetrapodos <- listado_especies %>% 
  filter(taxon_class_name == "Aves" | taxon_class_name == "Amphibia" | 
           taxon_class_name == "Mammalia" | taxon_class_name == "Reptilia")

write.csv(Tetrapodos,"datos/Lista_Tetrapodos.csv")

Plantas <- listado_especies %>% 
  filter(taxon_family_name == "Fabaceae" | taxon_family_name == "Cactaceae" | 
           taxon_family_name == "Asteraceae"|taxon_family_name == "Solanaceae")

write.csv(Plantas,"datos/Lista_Plantas_Actualizada.csv")