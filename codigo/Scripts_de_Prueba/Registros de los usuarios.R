library(tidyverse)

NatUY <- read_rds("datos/NatUY.rds")

Usuarios <- NatUY %>% group_by(user_id) %>% 
  count() %>% arrange(desc(n))

ggplot(Usuarios) +
  geom_histogram(aes(x = user_id), col= "white") + coord_flip()

nrow(Usuarios)    # Cantidad de usuarios


# Registros de los 500 primeros usuarios

Usuarios2 <- NatUY %>% 
  group_by(user_id, taxon_kingdom_name, taxon_class_name) %>%
  count() %>% arrange(desc(n)) %>% head(500) %>% 
  filter(!is.na(taxon_class_name))
  
  ggplot(Usuarios2) +
  geom_bar(aes(x = taxon_class_name, fill=taxon_kingdom_name), 
           show.legend = T,) + 
    theme(legend.position = "top") +
    labs(x= "Clases", y = "Observaciones", fill = "Reino") +
    coord_flip()

                   
  