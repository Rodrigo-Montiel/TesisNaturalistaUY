###PLANTAS
library(dplyr)
library(tmap)
library(sf)
sf::sf_use_s2(FALSE)
library(tidyverse)

plants_conservation_status <- read_csv("datos/plants_conservation_status.csv")
especies_prioritarias_vasculares <- read_csv("datos/especies_prioritarias_vasculares.csv")

plants_prioritarias <- especies_prioritarias_vasculares %>% filter(
Familia =="Asteraceae"| Familia == "Cactaceae" | Familia == "Fabaceae" | 
  Familia == "Solanaceae")

write.csv(plants_prioritarias, "datos/plantas_prioritarias.csv")
