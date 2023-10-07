# PAQUETES Y DATOS--------------------------------------------------------------
library(tidyverse)
library(sf)
sf::sf_use_s2(FALSE)
library(dplyr)

NatUY <- read_csv('datos/Tablas/Observaciones_27-10-22.csv')

# FILTRADO DE ESPECIES CASUALES, INTRODUCIDAS y SUBESPECIES---------------------

## De la categorización de registros (4_categorizacion_de_registros.R) nos 
## encontramos con especies "casuales", especies introducidas y subespecies 
## dentro de los regstros. Vamos a eliminarlas:


## 1) str_count(scientific_name, "\\S+") ==2 nos permite seleccionar aquellos
## registros que en el campo scientific_name cuenten con dos palabras

NatUY <- NatUY %>%  filter(str_count(scientific_name, "\\S+") == 2) 


## 2) Creamos un objeto con todos los nombres científicos de las especies
## que queremos eliminar

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

NatUY <- NatUY[!(NatUY$scientific_name %in% especies_eliminar), ]



write.csv(NatUY,"datos/Tablas/NatUY.csv")


# CONTRIBUCIONES NATURALISTAUY-------------------------------------------------

## Cantidad de registros
nrow(NatUY)

## Cantidad de usuarios
NatUY %>% st_drop_geometry() %>% group_by(user_id) %>% 
  count() %>% arrange(desc(n)) %>% nrow()


## Registros con Grado de investigación
NatUY %>% st_drop_geometry() %>% filter(quality_grade == "research") %>% 
  group_by(quality_grade) %>% nrow()

## Cantidad de Especies registradas
NatUY %>% st_drop_geometry() %>%
  group_by(scientific_name) %>% 
  filter(!is.na(scientific_name)) %>% 
  filter(str_count(scientific_name, "\\S+") ==2 ) %>% 
  count() %>% arrange(desc(n)) %>% nrow()





# REGISTROS POR REINOS----------------------------------------------------------

Tabla_reinos <- NatUY %>% st_drop_geometry() %>%  
  filter(str_count(scientific_name, "\\S+") ==2 ) %>% 
  group_by(taxon_kingdom_name) %>% 
  summarise("Número de observaciones"= n(), 
            "% Observaciones GI"= sum(quality_grade == "research" & 
                                        !is.na(taxon_species_name) & 
                                        taxon_species_name!="")/ n()*100, 
            "% Observaciones necesitan ID"=  sum(quality_grade =="needs_id")/
              n()*100,
            "Número de especies" = length(unique(scientific_name)))
