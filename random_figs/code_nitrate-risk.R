# author: gina
# created; 8/15/2021
# purpose: possibly make risk fig for dissertation talk
# last updated: 

rm(list = ls())

library(tidyverse)

#--getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
curdir <- paste(getwd())

source("talk-palette.R")

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

prds <- 
  read_csv("fc_leach-preds-eu.csv") 

nl_prds <- 
  prds %>% 
  filter((rotation == "cc" & nrate_kgha == 200)|
         (rotation == "cs" & nrate_kgha == 150))

nl_prds2 <- 
  prds %>% 
  filter((rotation == "cc" & nrate_kgha == 150)|
           (rotation == "cs" & nrate_kgha == 150))


# fig ---------------------------------------------------------------------

f_dat <- 
  nl_prds %>%
  distinct() %>% 
  select(yearF, site_id, rotation, preds) %>% 
  pivot_wider(names_from = rotation, values_from = preds) %>% 
  filter(yearF != 2018)

cc_dat <- 
  f_dat %>% 
  select(-cs) %>% 
  arrange(cc) %>% 
  mutate(n = 1:n(),
         n = 1-n/max(n))

cs_dat <- 
  f_dat %>% 
  select(-cc) %>% 
  arrange(cs) %>% 
  mutate(n = 1:n(),
         n = 1-n/max(n))

f_dat2 <- 
  cc_dat %>% 
  mutate(rot = "cc") %>% 
  rename("leach" = cc) %>% 
  bind_rows(
    cs_dat %>% 
      mutate(rot = "sc") %>% 
      rename("leach" = cs)
    )

f_dat2 %>% 
  select(site_id) %>% 
  distinct()

# fig ---------------------------------------------------------------------

nit_lab <- (expression(atop("Nitrate leaching", paste("(kg N "~ha^-1*")"))))

f_dat2 %>% 
  ggplot(aes(n, leach, color = rot) ) + 
  geom_line(size = 3) +
  scale_color_manual(values = c("cc" = "gold",
                                "sc" = "green4"), 
                     labels = c("cc" = "Continuous corn",
                                "sc" = "Rotated corn")) +
  labs(x = "Percentage of site years",
       y = nit_lab,
       color = NULL) +
  scale_x_continuous(labels = scales::label_percent()) +
  mytheme



f_dat2 %>% 
  mutate(rot_nice = ifelse(rot == "cc", "Continuous corn", "Rotated corn")) %>% 
  ggplot(aes(n, leach, fill = rot) ) + 
  geom_area(size = 3, color = "black") +
  scale_fill_manual(values = c("cc" = "gold",
                                "sc" = "green4"), 
                     labels = c("cc" = "Continuous corn",
                                "sc" = "Rotated corn")) +
  labs(x = "Percentage of site years",
       y = nit_lab,
       color = NULL,
       fill = NULL) +
  scale_x_continuous(labels = scales::label_percent()) +
  mytheme + 
  facet_grid(.~rot_nice)

ggsave("fig_nitrate-risk.png", width = 11, height = 5.6)


# fig 2 ---------------------------------------------------------------------

f_dat2 <- 
  nl_prds %>%
  distinct() %>% 
  select(yearF, site_id, rotation, preds) %>% 
  pivot_wider(names_from = rotation, values_from = preds) %>% 
  filter(yearF != 2018)

cc_dat2 <- 
  f_dat2 %>% 
  select(-cs) %>% 
  arrange(cc) %>% 
  mutate(n = 1:n(),
         n = 1-n/max(n))

cs_dat2 <- 
  f_dat2 %>% 
  select(-cc) %>% 
  arrange(cs) %>% 
  mutate(n = 1:n(),
         n = 1-n/max(n))

f_dat22 <- 
  cc_dat2 %>% 
  mutate(rot = "cc") %>% 
  rename("leach" = cc) %>% 
  bind_rows(
    cs_dat2 %>% 
      mutate(rot = "sc") %>% 
      rename("leach" = cs)
  )

f_dat22 %>% 
  select(site_id) %>% 
  distinct()

# fig ---------------------------------------------------------------------

nit_lab <- (expression(atop("Nitrate leaching", paste("(kg N "~ha^-1*")"))))


f_dat22 %>% 
  mutate(rot_nice = ifelse(rot == "cc", "Continuous corn", "Rotated corn")) %>% 
  ggplot(aes(n, leach, fill = rot) ) + 
  geom_area(size = 3, color = "black") +
  scale_fill_manual(values = c("cc" = "gold",
                               "sc" = "green4"), 
                    labels = c("cc" = "Continuous corn",
                               "sc" = "Rotated corn")) +
  labs(x = "Percentage of site years",
       y = nit_lab,
       color = NULL,
       fill = NULL) +
  scale_x_continuous(labels = scales::label_percent()) +
  mytheme + 
  facet_grid(.~rot_nice)

ggsave("fig_nitrate-risk.png", width = 11, height = 5.6)
