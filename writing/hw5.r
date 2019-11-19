knitr::opts_chunk$set(echo = FALSE)
library(tidyverse)
library(tigris)
library(sf)
library(ggplot2)
library(maps)

# read in data
denver <- read_csv("../data/homicide-data.csv") %>% 
  filter(city == "Denver") %>% 
  select(lat, lon, disposition, victim_race)

# filter by race
denver_race <- denver %>% 
  group_by(victim_race) %>% 
  mutate(count = n()) %>% 
  arrange(desc(count)) %>% 
  ungroup() %>% 
  filter(victim_race == c("Black", "White", "Hispanic"))
denver_race

denver_zip <- zctas(cb = TRUE, starts_with = 
                      c("802"), class = "sf")
plot(denver_zip)

race <- denver_zip

# create map
denver_crs <- denver_race %>% 
  filter(!is.na(lat)) %>% 
  st_as_sf(coords = c("lon", "lat")) %>% 
  st_set_crs(4269)
denver_crs

# create plot 
zip_map <- ggplot() +
  geom_sf(data = denver_zip, color = "lightgray") +
  geom_sf(data = denver_crs, aes(color = factor(victim_race)))
zip_map


