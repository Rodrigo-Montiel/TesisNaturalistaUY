library(janitor)
library(tidyverse)

registros_de_tetrapodosuy <- read_csv('datos/registros_de_tetrapodosuy.csv')


registros_de_tetrapodosuy %>% group_by(usuario)


## Ranking vs largo_cm
ggplot(registros_de_tetrapodosuy, aes(x=ranking, y=largo_cm, color = nivel)) +
  geom_point() + facet_wrap(~clase) +
  labs(x='Observer ranking',
       y='Largo (cm)') +
  theme_bw()

## Ranking vs distribucion
ggplot(registros_de_tetrapodosuy, aes(x=ranking, y=distribucion, color = nivel)) +
  geom_point() + #facet_wrap(~clase) +
  labs(x='Observer ranking',
       y='Cantidad de departamentos') +
  theme_bw()
