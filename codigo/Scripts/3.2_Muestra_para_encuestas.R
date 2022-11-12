### Sample de usuarios para las encuestas
usuarios_dataset %>% filter(categoria_usuario=="principiante" & 
                              ultimo_registro<(today()-365)) %>% 
  pull(user_login) %>% sample(., size=100)