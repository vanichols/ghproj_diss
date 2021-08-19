# author: gina
# created: 8/19/2021
# purpose: map of population changes
# last updated: 

rm(list = ls())

library(tidyverse)
library(maps)


source("random_figs/talk-palette.R")

theme_set(theme_bw())

mytheme <- 
  theme(legend.justification = c(1,1),
        legend.position = c(1,1),
        legend.background = element_blank(),
        legend.text = element_text(size = rel(1.5)),
        strip.text = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.3)),
        axis.title = element_text(size = rel(1.5)))



yld_lab <- (expression(atop("Corn Yield", paste("(Mg "~ha^-1*")"))))



# data --------------------------------------------------------------------
  
ia <- 
  map_data("state") %>% 
  filter(region %in% c("iowa"))


dat <- read_csv("random_figs/Total_City_Population_by_Year-with-estimate.csv")
fips <- read_csv("random_figs/cities-fips.csv") %>% 
  rename("City" = 2)


f_dat <- 
  dat %>% 
  left_join(fips) %>% 
  filter(!is.na(latitude)) %>% 
  mutate(year = lubridate::year(Date)) %>% 
  filter(year %in% c(1990, 2020)) %>% 
  select(City, county, latitude, longitude, year, Population) %>% 
  pivot_wider(names_from = year, values_from = Population) %>% 
  janitor::clean_names() %>% 
  mutate(pop_change = x2020 - x1990,
         pop_change_pct = pop_change/x1990,
         neg = ifelse(pop_change < 0, "Declining", "Growing")) %>% 
  rename("long" = longitude,
         "lat" = latitude)


#--dots by pop size
ggplot() + 
  geom_polygon(data = ia, 
               aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_point(data = f_dat,
             aes(x = long, y = lat, color = neg, size = pop_change))

f_dat %>% 
  mutate(cat = 
           case_when(
    Population =< 50000 & Population >= 10000 ~ "Micropolitan")),
    (Population > 50000) ~ "Metropolitan",
    (Population < 10000) ~ "Rural",
    TRUE ~ NA))


  ggplot(aes())


#--dots by pop change %
ggplot() + 
  geom_polygon(data = ia, 
               aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_point(data = f_dat,
             aes(x = long, y = lat, color = neg, size = pop_change_pct))

