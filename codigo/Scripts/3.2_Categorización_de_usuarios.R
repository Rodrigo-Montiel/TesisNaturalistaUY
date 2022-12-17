# PAQUETES Y DATOS -------------------------------------------------------------
library(tmap)
library(sf)
sf::sf_use_s2(FALSE)
library(tidyverse)
library(patchwork)
library(stringr)
library(lubridate)
library(httr)
library(jsonlite)

NatUY <- read_rds('datos/natuysf.rds')
NatUY_dataset <- readRDS('datos/usuarios_dataset.rds')


#ANALISIS DE USUARIOS ---------------------------------------------------

## Cantidad de usuarios

Usuarios <- NatUY %>% st_drop_geometry() %>% 
  group_by(user_id) %>% 
  count() %>% arrange(desc(n))

Cantidad_Usuarios <- nrow(Usuarios)


## Categorias de usuarios

usuarios_login <- NatUY %>% st_drop_geometry() %>% 
  select(user_id, user_login) %>% group_by(user_id) %>% distinct()


usuarios_dataset <- NatUY %>% st_drop_geometry() %>% 
  select(user_login,user_id, observed_on, created_at) %>% 
  group_by(user_login) %>% 
  summarise(primer_registro = min(created_at), 
            ultimo_registro = max(created_at), 
            registros = n(), tiempo_activo = 
              difftime(ultimo_registro,primer_registro, 
                       units = "days")+1, 
            registros_x_tiempo = registros/as.numeric(tiempo_activo)) %>% 
  mutate(categoria_usuario = ifelse(registros>=1000 & tiempo_activo>=365 &
                                      registros_x_tiempo>=0.6, "experimentado",
                                    ifelse(registros>=50 & tiempo_activo>90 & 
                                             registros_x_tiempo>0.2,
                                           "intermedio", "principiante")))


saveRDS(usuarios_dataset, "datos/usuarios_dataset.rds")

 
### Cantidad de usuarios por categorias:
usuarios_dataset %>% group_by(categoria_usuario) %>% count()
 

  
## Nacionalidad de los usuarios
### Función usando la API de iNat https://api.inaturalist.org/v1/docs/
 
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
 
 observers_num_observations <- get_observers_num_observations(usuarios_dataset$user_login)
 

NatUY_users <- left_join(as_tibble(usuarios_dataset), 
                            observers_num_observations %>% mutate(user_name=ifelse(user_name=='', NA, user_name))) %>% 
  mutate(primer_registro=lubridate::as_datetime(primer_registro),
         ultimo_registro=lubridate::as_datetime(ultimo_registro),
         user_created_at=lubridate::as_datetime(user_created_at))

write_csv(NatUY_users, 'NatUY_users.csv')


### Uruguayxs en los registros
NatUY_users %>% 
  mutate(proporcion_NatUY_iNat = round(registros*100/observation_count, 3),
         esUruguaye = ifelse(proporcion_NatUY_iNat>30 , 'si', 'no')) %>% 
  filter(esUruguaye == "si") %>% 
  group_by(esUruguaye) %>% count() 



## Gráficos

usuarios_registros <- usuarios_dataset %>% filter(tiempo_activo>=8) %>% 
  ggplot(aes(registros_tiempo)) + geom_histogram(binwidth = 1) + 
  scale_x_continuous()