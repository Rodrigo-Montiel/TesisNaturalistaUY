# PAQUETES Y DATOS--------------------------------------------------------------
library(tidyverse)
library(sf)
sf::sf_use_s2(FALSE)
library(lubridate)


listado_especies <- 
  readRDS("~/GitHub/TesisNaturalistaUY/datos/Tablas/listado_especies.rds")

# FILTRADO DE ESPECIES CASUALES, INTRODUCIDAS y SUBESPECIES---------------------

## Encontramos especies "casuales", especies introducidas y sub-especies 
## dentro de los registros. Vamos a eliminarlas:


## 1) (str_count(scientific_name, "\\S+") ==2) nos permite seleccionar aquellos
## registros que en el campo scientific_name cuenten con dos palabras

listado_especies <- 
  listado_especies %>%  filter(str_count(scientific_name, "\\S+") == 2) 


## 2) Creamos un objeto con todos los nombres científicos de las especies
## domesticas/introducidas que queremos eliminar

especies_eliminar <- c("Anas platyrhynchos", "Anser anser", "Canis familiaris", 
                       "Equus asinus", "Equs caballus", "Felis catus", 
                       "Corvus splendens", "Melopsittacus undulatus", 
                       "Cervus elaphus", "Oryctolagus cuniculus", 
                       "Agapornis personatus","Amazona aestiva",
                       "Acacia baileyna", "Acacia podalytiifolia", 
                       "Albizia julibrissin", "Bauhinia variegata", 
                       "Biden aurea", "Brugmansia arborea", 
                       "Brugmansia suaveolens", "Calendula offcinalis", 
                       "Carduus nutans", "Cosmos bipinnatus", 
                       "Cosmos sulphureus", "Cotula nigellifolia", 
                       "Dimorphotheca exklonis", "Epiphyllumm oxypetalum",
                       "Euryops chrysanthemoides", "Gazania rigens", 
                       "Glycine max", "Helenium amarum", "Helianthus annuus", 
                       "Lactuca sativa", "Lespedeza bicolor", 
                       "Nicotiana tabacum","Opuntia ficus-indica", 
                       "Physalis peruviana","Pseudogynoxys chenopodioides", 
                       "Santolia chamaexyparissus", "Selenicereus undatus", 
                       "Senecio angulatus", "Senecio tamoides", 
                       "Senegalia picachenis", "Solanum aethiopicum",
                       "Solanum nigrum", "Solanum tuberosum", 
                       "Taraxacum erythrospermum", "Tragopogon porrifolius",
                       "Vachellia karroo", "Wisteria sinensis",
                       "Youngia japonica","Zinnia elegans")


## 3) filtramos la tabla de datos original de las especies casuales

listado_especies <- 
  listado_especies[!(listado_especies$scientific_name %in% especies_eliminar), ]


# SELECCION DE GRUPOS-----------------------------------------------------------

## Vamos a conformar una lista con todas las especies de interés registradas 
## dentro de tetrápodos y otra lista con las especies de Traqueofitas 

Tetrapodos <- listado_especies %>% 
  filter(taxon_class_name == "Aves" | taxon_class_name == "Amphibia" | 
           taxon_class_name == "Mammalia" | taxon_class_name == "Reptilia")

write.csv(Tetrapodos,"datos/Tablas/Lista_Tetrapodos.csv")


Plantas <- listado_especies %>% 
  filter(taxon_family_name == "Fabaceae" | taxon_family_name == "Cactaceae" | 
           taxon_family_name == "Asteraceae"|taxon_family_name == "Solanaceae")

write.csv(Plantas,"datos/Tablas/Lista_Plantas.csv")