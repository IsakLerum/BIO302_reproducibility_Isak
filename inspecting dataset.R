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
  view()

richness_per_site <- ants |> 
  group_by(site, forest_age, presence_absence, forest_chrono_class) |> 
  count() |> 
  filter(presence_absence == 1)
richness_per_site

forest_ages_mean <- richness_per_site |> 
  group_by(forest_chrono_class) |> 
  summarise(mean(n), sd(n), nn = n())
forest_ages_mean

richnes_vs_forest_age <-ggplot(richness_per_site, aes(forest_age, n)) +
  geom_point() +
  geom_line()
richnes_vs_forest_age
