#####################################
# Author: Florencia Grattarola
# Date: 2022-12-10
# Description: Código para extraer la cantidad de observaciones en iNat para cada user de NaturalistaUY
#####################################

# Ejemplo
#
# curl -X GET --header 'Accept: application/json' 'https://api.inaturalist.org/v1/observations/observers?user_login=flo_grattarola'
# 
# {
#   "total_results": 1,
#   "page": 1,
#   "per_page": 500,
#   "results": [
#     {
#       "user_id": 736016,
#       "observation_count": 3649,
#       "species_count": 1246,
#       "user": {
#         "id": 736016,
#         "login": "flo_grattarola",
#         "spam": false,
#         "suspended": false,
#         "created_at": "2017-12-15T15:54:34+00:00",
#         "login_autocomplete": "flo_grattarola",
#         "login_exact": "flo_grattarola",
#         "name": "Florencia Grattarola",
#         "name_autocomplete": "Florencia Grattarola",
#         "orcid": "https://orcid.org/0000-0001-8282-5732",
#         "icon": "https://static.inaturalist.org/attachments/users/icons/736016/thumb.jpeg?1513353273",
#         "observations_count": 3649,
#         "identifications_count": 5779,
#         "journal_posts_count": 1,
#         "activity_count": 9429,
#         "species_count": 1419,
#         "universal_search_rank": 3649,
#         "roles": [
#           "curator"
#         ],
#         "site_id": 28,
#         "icon_url": "https://static.inaturalist.org/attachments/users/icons/736016/medium.jpeg?1513353273"
#       }
#     }
#   ]
# }


#####################################
# Librerías

library(httr)
library(jsonlite)
library(tidyverse)

#####################################
# Datos

NatUY_users <- readRDS('datos/usuarios_dataset.rds')

#####################################
# Función usando la API de iNat https://api.inaturalist.org/v1/docs/

get_observers_num_observations <- function(user_login_list){
  observers_num_observations <- tibble(user_id = numeric(), 
                                       observation_count = numeric(), 
                                       species_count = numeric(), 
                                       user_login = character(),
                                       user_created_at = lubridate::ymd_hms(), 
                                       user_name = character())
  num_results <- 1
  for (user_login in user_login_list) {
    if ((num_results %% 10) + 10 == 10) { 
      Sys.sleep(10) # La API necista un delay porque si no da error. Cada 10 users, el código para 10 segundos
    }
    call <- paste0("https://api.inaturalist.org/v1/observations/observers?user_login=", user_login)
    get_json_call <- GET(url = call) %>%
      content(as = "text") %>%
      fromJSON(flatten = TRUE)
    if (is.null(get_json_call)) {
      observer_num_observations <- tibble(user_id = NA, 
                                         observation_count = NA, 
                                         species_count = NA, 
                                         user_login = user_login,
                                         user_created_at = NA, 
                                         user_name = NA)
      observers_num_observations <- rbind(observers_num_observations, observer_num_observations)
      cat('usuario:', user_login, '--> NOT FOUND', '\n')
    }
    else {
      results <- as_tibble(get_json_call$results) 
      observer_num_observations <- tibble(user_id = results$user_id, 
                                           observation_count = results$observation_count, 
                                           species_count = results$species_count, 
                                           user_login = results$user.login,
                                           user_created_at = results$user.created_at, 
                                           user_name = results$user.name)
      
      observers_num_observations <- rbind(observers_num_observations, observer_num_observations)
      cat(num_results, 'usuario:', user_login, '--> DONE', '\n')
    }
    num_results <- nrow(observers_num_observations) + 1
  }
  return(observers_num_observations)
}

observers_num_observations <- get_observers_num_observations(NatUY_users$user_login)

NatUY_users_UY <- left_join(as_tibble(NatUY_users), 
          observers_num_observations %>% mutate(user_name=ifelse(user_name=='', NA, user_name))) %>% 
  mutate(primer_registro=lubridate::as_datetime(primer_registro),
         ultimo_registro=lubridate::as_datetime(ultimo_registro),
         user_created_at=lubridate::as_datetime(user_created_at))

write_csv(NatUY_users_UY, 'NatUY_users_UY.csv')

NatUY_users_UY %>% 
  mutate(proporcion_NatUY_iNat = round(registros*100/observation_count, 3),
         esUruguaye = ifelse(proporcion_NatUY_iNat>30, 'si', 'no')) %>% 
  group_by(esUruguaye) %>% count()

#   esUruguaye     n
#   <chr>      <int>
# 1 no           500
# 2 si          1277
# 3 NA            11
