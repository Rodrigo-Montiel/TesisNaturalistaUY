#####################################
# Author: Florencia Grattarola
# Date: 2022-12-07
# Description: Código para extraer los estados de conservación de especies en NaturalistaUY
#####################################

# Librerías

library(rredlist)
library(tidyverse)

#####################################
# Función get_IUCN_data 
# Código de (https://github.com/bienflorencia/rBiodiversidata/blob/62b3dbbc9a491b01e3de9837091c774813c36c0e/Data%20Cleaning%20and%20Standardisation%20Scripts/retrieve_IUCN_data.R)

get_IUCN_data <- function(speciesList){
  IUCN_status <- data.frame(species = character(), 
                            status = character(), 
                            trend = character(), stringsAsFactors=FALSE)
  for(sp in speciesList){
    UICN_search <- rl_search(name = sp)
    if (length(UICN_search$result) == 0){
      IUCN_status_sp <- data.frame(species = sp, 
                                   status = NA, 
                                   trend = NA, stringsAsFactors=FALSE)
      IUCN_status <- rbind(IUCN_status, IUCN_status_sp)
      cat(sp,'----- CHECK\n')
    }
    else {
      IUCN_status_sp <- data.frame(species = UICN_search$result$scientific_name, 
                                   status = UICN_search$result$category, 
                                   trend = UICN_search$result$population_trend, stringsAsFactors=FALSE)
      IUCN_status <- rbind(IUCN_status, IUCN_status_sp)
      cat(sp,'----- OK\n')
    }
  }
  return(IUCN_status)
}

# Función check_species_names
# Código de (https://github.com/bienflorencia/rBiodiversidata/blob/62b3dbbc9a491b01e3de9837091c774813c36c0e/Data%20Cleaning%20and%20Standardisation%20Scripts/check_species_names.R)

#####################################
# Test
# Species List

canids <- c('Cerdocyon thous', 'Lycalopex gymnocercus', 'Chrysocyon brachyurus', 'AnyWrong species')
canids_IUCN_data <- get_IUCN_data(canids)


##########################################################################
#####################################
# Run
# NaturalistaUY species List TETRAPODS

especies_tetrapodos_NatUY <- readRDS('datos/Lista_Tetrapodos')
tetrapodos_IUCN_data <- get_IUCN_data(especies_tetrapodos_NatUY$taxon_species_name)
tetrapodos_IUCN_data <- tetrapodos_IUCN_data %>% mutate(status=ifelse(status=='NA', NA, status))

tetrapodos_IUCN_data %>% 
  filter(is.na(status)) %>% nrow()

nrow(tetrapodos_IUCN_data)

#############

# Complemento con datos de Biodiversidata

biodiversidata_tetrapods_status <- read_csv('../../Biodiversidata/species_Biodiversidata_1.0.0_national_IUCN.csv')

tetrapods_conservation_status <- left_join(especies_tetrapodos_NatUY %>% mutate(species=taxon_species_name), 
                                          biodiversidata_tetrapods_status %>% select(-class), by='species') %>%
  left_join(., tetrapodos_IUCN_data) %>% 
  mutate(IUCNglobal=ifelse(is.na(IUCNglobal), status, IUCNglobal)) %>% 
  select(-status) %>% rename(trendGlobal=trend) %>% 
  as_tibble()
  
# write_csv(tetrapods_conservation_status, 'datos/tetrapods_conservation_status.csv')
tetrapods_conservation_status <- read_csv('datos/tetrapods_conservation_status.csv')
# Queda revisar aquellos sin estado de conservación en NaturalistaUY y en IUCN Red List para saber si finalmente son 'NE': Not Evaluated.
# En caso que no se encuentren, todos los `is.na(IUCNglobal)` y los `is.na(IUCNregional)` deberían ponerse como 'NE'.

##########################################################################
#####################################
# Run
# NaturalistaUY species List PLANTAS

especies_plantas_NatUY <- readRDS('datos/Lista_Plantas')
plantas_IUCN_data <- get_IUCN_data(especies_plantas_NatUY$taxon_species_name)


nrow(plantas_IUCN_data)
plantas_IUCN_data %>% 
  filter(is.na(status)) %>% nrow()

biodiversidata_plants_status <- read_csv('datos/vascularPlants_biodiversidata.csv')
snap_vasculares_prioritarias <- read_csv('datos/especies_prioritarias_vasculares.csv') %>% janitor::clean_names()

# left_join(especies_plantas_NatUY %>% mutate(species=taxon_species_name), 
#           biodiversidata_plants_status %>% select(species,establishmentMeans), by='species') %>% 
#   left_join(., tetrapodos_IUCN_data)

plants_conservation_status <- left_join(especies_plantas_NatUY %>% mutate(species=taxon_species_name), 
          snap_vasculares_prioritarias %>% select(species=nombre_cientifico,estado_de_conservacion, nativa), by='species') %>% 
  left_join(., biodiversidata_plants_status %>% select(species,establishmentMeans)) %>% 
  left_join(., plantas_IUCN_data) %>% 
  rename(SNAPregional=estado_de_conservacion, SNAPestablecimiento=nativa, 
         BiodiversidataEstablecimiento=establishmentMeans, IUCNglobal=status, trendGlobal=trend) %>% as_tibble()

write_csv(plants_conservation_status, 'datos/plants_conservation_status.csv')


# Queda revisar aquellos sin estado de conservación en NaturalistaUY y en IUCN Red List para saber si finalmente son 'NE': Not Evaluated.
# En caso que no se encuentren, todos los `is.na(IUCNglobal)` y los `is.na(IUCNregional)` deberían ponerse como 'NE'.


