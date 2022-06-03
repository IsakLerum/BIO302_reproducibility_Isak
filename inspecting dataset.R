#### Loading Library ####
library(readr)
library(readxl)
library(tidyverse)
library(tidylog)
library(dplyr)
library(usethis)
library(ggplot2)


#### Loading data ####
ants <- read_excel("data/doi_10.5061_dryad.rs8vc__v1/Ant data for EE paper and Dryad.xlsx")
ants <- ants |> 
  janitor::clean_names()
ants <- ants |>
  select(site, presence_absence, species, forest_age, forest_chrono_class)
ants |> 
  distinct(species)


#### transforming data ####
richness_per_chrono_class <- ants |> 
  group_by(site, forest_age, presence_absence, forest_chrono_class) |> 
  count() |>
  filter(presence_absence == 1) |>
  group_by(forest_chrono_class) |>
  summarise(richness_mean = mean(n), richness_sd = sd(n), nn = n()) |> 
  mutate(forest_age = c(0, 3, 8, 13, 19, 50))
richness_per_chrono_class


#### making figure ####
richnes_vs_forest_age <-ggplot(richness_per_chrono_class, aes(forest_age, richness_mean)) +
  geom_pointrange(aes(ymin = richness_mean - richness_sd, ymax = richness_mean + richness_sd)) +
  geom_line() +
  labs(y = "Species richness", x = "Forest age")
richnes_vs_forest_age
