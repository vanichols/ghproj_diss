# Created:    aug 16 2021
# purpose: map of sites
#
# notes: 
# last edited:   

rm(list = ls())
#devtools::install_github("femiguez/nlraa")

#--getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
curdir <- paste(getwd())

library(tidysawyer2) 
library(tidyverse)
library(saapsim)
library(fancycut)
library(patchwork)
library(scales)
library(maps)
library(ggrepel)

theme_set(theme_bw())

source("talk-palette.R")


# map ---------------------------------------------------------------------

iail <- 
  map_data("state") %>% 
  filter(region %in% c("iowa", "illinois"))

ilia_siteinfo

ilia_yields %>% 
  select(site, year) %>% 
  distinct()

ilia_yields %>% 
  select(year) %>% 
  distinct()

ggplot() + 
  geom_polygon(data = iail, 
               aes(x = long, y = lat, group = group), color = "white", fill = "gray80") +
  geom_point(data = ilia_siteinfo,
             aes(x = long, y = lat), fill = "dodgerblue", pch = 21) + 
  labs(x = "Longitude", 
       y = "Latitude",
       title = "14 sites, 1999-2016",
       subtitle = "179 site-years") + 
  coord_quickmap() +
  #coord_cartesian() + 
  theme(legend.justification = c(0, 0),
        legend.position = c(0.1, 0.1),
        legend.background = element_rect(color = "black"))

ggsave("fig_map-of-sites.png", height = 3.5,
       width = 7.5)
