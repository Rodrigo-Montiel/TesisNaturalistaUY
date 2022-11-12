# PAQUETES Y DATOS--------------------------------------------------------------

usuarios_dataset <- readRDS("datos/usuarios_dataset.rds")

# MUESTRAS----------------------------------------------------------------------

##Categoria: Principiantes
usuarios_dataset %>% filter(categoria_usuario=="principiante" & 
                              ultimo_registro<(today()-365)) %>% 
  pull(user_login) %>% sample(., size=100)

##Categoria: Intermedios
usuarios_dataset %>% filter(categoria_usuario=="intermedio" & 
                              ultimo_registro<(today()-365)) %>% 
  pull(user_login) %>% sample(., size=20)

##Categoria: Experimentados
usuarios_dataset %>% filter(categoria_usuario=="experimentados" & 
                              ultimo_registro<(today()-365)) %>% 
  pull(user_login) %>% sample(., size=5)