
library(janitor)
library(tidyverse)

registros_de_tetrapodos <- read_csv('datos/registros_de_tetrapodos.csv')

registros_de_tetrapodos %>% 
  filter(!is.na(user_id) & !is.na(status_global)) %>% 
  mutate(status_global=as.factor(status_global), 
         status_regional=as.factor(status_regional),
         nivel=as.factor(nivel)) %>% 
  group_by(nivel) %>% 
  summarise(observadores=n_distinct(user_id),
            observaciones=n(),
            largo_cm_mean=mean(largo_cm, na.rm=T),
            distribucion_mean=mean(distribucion, na.rm=T))

# ranking vs estatus global
ggplot(registros_de_tetrapodos %>% filter(!is.na(user_id)& !is.na(status_global)), 
       aes(x=fct_relevel(status_global,'LC','NT','VU', 'EN', 'CR','DD', 'NE'), 
           y=ranking)) +
  geom_boxplot() +
  facet_wrap(~clase, scales = 'free') +
  labs(x='Global IUCN Conservation Status',
       y='Observer ranking') +
  theme_bw()

# ranking vs estatus regional
ggplot(registros_de_tetrapodos %>% filter(!is.na(user_id)), 
       aes(x=fct_relevel(status_regional,'LC','NT','VU', 'EN', 'CR','DD', 'NE'), 
           y=ranking)) +
  geom_boxplot() +
  facet_wrap(~nivel, scales = 'free') +
  labs(x='National IUCN Conservation Status',
       y='Observer ranking') +
  theme_bw()

# ranking vs estatus global
ggplot(registros_de_tetrapodos %>% filter(!is.na(user_id)), 
       aes(x=largo_cm)) +
  geom_histogram() +
  labs(x='Largo (cm)',
       y='Observer ranking') +
  theme_bw()

ggplot(registros_de_tetrapodos %>% filter(!is.na(user_id)), 
       aes(x=ranking, 
           y=largo_cm)) +
  geom_boxplot() +
  facet_wrap(~nivel) +
  labs(x='Observer ranking',
       y='Largo (cm)') +
  theme_bw()
