# author: gina
# created: 8/19/2021
# purpose: map of public land
# last updated: 

rm(list = ls())

library(tidyverse)
library(maps)
library(usmap)





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




# data --------------------------------------------------------------------
  
usa <- 
  map_data("state") 

usa %>% 
  ggplot(aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", color = "black")


dat <- 
  read_csv("random_figs/public-land-ownership.csv") %>% 
  left_join(
    tibble(state = state.abb,
           state_name = state.name)
    
  ) %>% 
  select_at(vars(-starts_with("X"))) %>% 
  mutate_if(is.character, str_to_lower)


# maps --------------------------------------------------------------------

#--all usa
dat %>% 
  left_join(usa %>% rename("state_name" = region)) %>% 
  ggplot(aes(long, lat, group = group, fill = pct)) + 
  geom_polygon(color = "black") +
  scale_fill_viridis_c()



usa1 <- 
  map_data("state") %>% 
  filter(region %in% c("iowa",
                       "illinois",
                       "minnesota",
                       "wisconsin",
                       "nebraska",
                       "missouri"))


dat %>% 
  left_join(usa1 %>% rename("state_name" = region)) %>% 
  filter(!is.na(long)) %>% 
  ggplot(aes(long, lat, group = group, fill = pct)) + 
  geom_polygon(color = "black") +
  scale_fill_viridis_c()


# lolly? ------------------------------------------------------------------

ax_clr <- 
  dat %>% 
  select(state_name) %>% 
  mutate(1:n()) %>% 
  filter(state_name == "iowa")

#--higlight iowa
one1 <- rep("black", 1)
one2 <- rep("black", 50-2)

#--highlight iowa and colorado
two1 <- rep("gray80", 1)
two2 <- rep("gray80", 39)
two3 <- rep("gray80", 8)

dat %>% 
  filter(state == "ia")

dat %>% 
  filter(state == "co")



# highlight ---------------------------------------------------------------


dat %>% 
  mutate(pct = pct/100) %>% 
  mutate_if(is.character, str_to_title) %>% 
  ggplot(aes(reorder(state_name, pct), pct)) + 
  geom_point(aes(fill = (state_name %in% c("Iowa", "Colorado")),
                 size = pct), pch = 21) + 
  geom_segment(aes(x = state_name, xend = state_name,
                   y = 0, yend = pct),
               color = "gray60") +
  geom_point(aes(fill = (state_name %in% c("Iowa", "Colorado")),
                 size = pct), pch = 21) + 
  geom_text(aes(x = "Colorado", y = 0.5), 
            label = "40%", check_overlap = T, size = 7, fontface = "italic") +
  geom_text(aes(x = "Iowa", y = 0.1), 
            label = "1%", check_overlap = T, size = 7, fontface = "italic") +
  coord_flip() + 
  scale_fill_manual(values = c("black", clr_red)) + 
  guides(color = F, size = F, fill = F) +
  theme(
    #axis.text.y = element_text(color = c(one1, clr_red, one2)) #--highlight iowa
    axis.text.y = element_text(color = c(two1, clr_red, two2, clr_red, two3)) #--ia and co
  ) + 
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = NULL,
       y = "Percentage of State Land that is Publicly Owned",
       caption = "Based on 1995 data from the National Wilderness Institute\nhttp://https://www.nrcm.org/documents/publiclandownership.pdf") + 
  bigtheme + 
  theme(axis.text.y = element_text(size = rel(0.8)))
  
ggsave("random_figs/fig_pub-land2.png")



# all same ----------------------------------------------------------------


dat %>% 
  mutate(pct = pct/100) %>% 
  mutate_if(is.character, str_to_title) %>% 
  ggplot(aes(reorder(state_name, pct), pct)) + 
  geom_point(aes(fill = (state_name %in% c("Iowa", "Colorado")),
                 size = pct), pch = 21) + 
  geom_segment(aes(x = state_name, xend = state_name,
                   y = 0, yend = pct),
               color = "gray60") +
  geom_point(aes(fill = (state_name %in% c("Iowa", "Colorado")),
                 size = pct), pch = 21) + 
  # geom_text(aes(x = "Colorado", y = 0.5), 
  #           label = "40%", check_overlap = T, size = 7, fontface = "italic") +
  # geom_text(aes(x = "Iowa", y = 0.1), 
  #           label = "1%", check_overlap = T, size = 7, fontface = "italic") +
  coord_flip() + 
  scale_fill_manual(values = c("black", 
                               #clr_red
                               "black")) + 
  guides(color = F, size = F, fill = F) +
  # theme(
  #   #axis.text.y = element_text(color = c(one1, clr_red, one2)) #--highlight iowa
  #   axis.text.y = element_text(color = c(two1, clr_red, two2, clr_red, two3)) #--ia and co
  # ) + 
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = NULL,
       y = "Percentage of State Land that is Publicly Owned",
       caption = "Based on 1995 data from the National Wilderness Institute\nhttp://https://www.nrcm.org/documents/publiclandownership.pdf") + 
  bigtheme + 
  theme(axis.text.y = element_text(size = rel(0.8)))

ggsave("random_figs/fig_pub-land1.png")
