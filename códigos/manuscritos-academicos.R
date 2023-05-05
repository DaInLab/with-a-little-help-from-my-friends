

# base de dados dos Pinguins de Palmer
library(tidyverse, quietly = TRUE)
library(palmerpenguins)
library(gt, quietly = TRUE)
data(package='palmerpenguins')
penguins %>% head() %>%
  gt()

penguins %>% names()

penguins %>%
  group_by(species) %>%
  select(bill_length_mm,bill_depth_mm) %>%
  summarise(across(.cols = everything(), .fns = list(Media = mean, Desvio = sd),
                   na.rm=TRUE,
                   .names = "{col}_{fn}")) %>% gt()

penguins %>%
  ggplot(aes(x=bill_length_mm, y=bill_depth_mm, color = species)) +
  geom_point(alpha=.5, size = 2.2) +
  facet_wrap(~ island)

