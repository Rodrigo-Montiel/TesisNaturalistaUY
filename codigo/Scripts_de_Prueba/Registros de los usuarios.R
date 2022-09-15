library(tidyverse)
library(forcats)

NatUY <- read_rds("datos/NatUY.rds")


# Cantidad de registros por usuario

Usuarios <- NatUY %>% group_by(user_id) %>% 
  count() %>% arrange(desc(n))

nrow(Usuarios)    # Cantidad de usuarios

ggplot(Usuarios) +
  geom_histogram(aes(x = n), col= "white")



# Registros de los 500 primeros usuarios

Usuarios2 <- NatUY %>% 
  group_by(user_id, taxon_kingdom_name, taxon_class_name) %>%
  count() %>% arrange(desc(n)) %>% head(500) %>% 
  filter(!is.na(taxon_class_name))
  
  ggplot(Usuarios2) +
  geom_bar(aes(x = fct_rev(fct_infreq(taxon_class_name)), fill=taxon_kingdom_name), 
           show.legend = T,) + 
    theme(legend.position = "top") +
    labs(x= "Clases", y = "Observaciones", fill = "Reino") + 
    coord_flip()
                   