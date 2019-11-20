knitr::opts_chunk$set(echo = FALSE)
library(tidyverse)
library(tigris)
library(sf)
library(ggplot2)
library(maps)

# read in data
denver <- read_csv("./data/homicide-data.csv") %>% 
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

# create map
denver_crs <- denver_disp %>% 
  filter(!is.na(lat)) %>% 
  st_as_sf(coords = c("lon", "lat")) %>% 
  st_set_crs(4269)
denver_crs

# create plot 
zip_map <- ggplot() +
  geom_sf(data = denver_zip, color = "lightgray") +
  geom_sf(data = denver_crs, aes(color = factor(victim_race), 
                                 shape = disposition)) +
            facet_wrap(~disposition, ncol =1)
zip_map

# facet into solved and unsolved
denver_disp <- denver_race %>% 
  mutate(disposition = factor(disposition, levels = c("Closed without arrest", 
                                                      "Closed by arrest", 
                                                      "Open/No arrest"),
                              labels = c("solved", "solved", "unsolved")))
denver_disp
  mutate(fct_lump(disposition, n = 2)) %>% # convert dispositions to factors
  count(disposition)
  mutate(is_unsolved = !(disposition == "Closed by arrest"),
         is_solved = (disposition == "Closed by arrest"))




