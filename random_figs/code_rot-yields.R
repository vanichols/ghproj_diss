# author: gina
# created: 8/16/2021
# purpose: yield examples
# last updated: 8/19/2021 (now it's on github)

rm(list = ls())

library(tidyverse)



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

dat <-
  tibble(rot = c("Continuous\nCorn", "2-year\nRotation", "4-year\nRotation"),
       yld = c(9, 10, 10.5)) %>% 
  mutate(rot = fct_inorder(rot),
         rot = fct_rev(rot)) 


# fig ---------------------------------------------------------------------

bu1 <- expression("180 bu "~ac^-1*"")
bu2 <- expression("200 bu "~ac^-1*"")
bu3 <- expression("210 bu "~ac^-1*"")

mg1 <- expression("9 Mg "~ha^-1*"")
mg2 <- expression("10 Mg "~ha^-1*"")
mg3 <- expression("10.5 Mg "~ha^-1*"")


# bu and Mg ---------------------------------------------------------------

dat %>% 
  ggplot(aes(rot, yld)) + 
  geom_col(aes(fill = rot), color = "black", width = 0.6) + 
  scale_fill_manual(values = c(clr_div, clr_rot, clr_cc)) +
  #--bu/ac
  geom_text(aes(x = "Continuous\nCorn",
                y = 8.5),
            label = bu1, 
            parse = T, check_overlap = T, size = 6, color = "gray60") +
  geom_text(aes(x = "2-year\nRotation",
                y = 9.5),
            label = bu2, 
            parse = T, check_overlap = T, size = 6, color = "gray60") +
  geom_text(aes(x = "4-year\nRotation",
                y = 10),
            label = bu3, 
            parse = T, check_overlap = T, size = 6, color = "gray60") +
  #--mg/ha
  geom_text(aes(x = "Continuous\nCorn",
                y = 9.5),
            label = mg1, 
            parse = T, check_overlap = T, size = 7) +
  geom_text(aes(x = "2-year\nRotation",
                y = 10.5),
            label = mg2, 
            parse = T, check_overlap = T, size = 7) +
  geom_text(aes(x = "4-year\nRotation",
                y = 11),
            label = mg3, 
            parse = T, check_overlap = T, size = 7) +
  guides(fill = F) + 
  labs(y = yld_lab, 
       x = NULL) +
  mytheme +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = rel(1.5)))


ggsave("random_figs/fig_rot-ylds-bu-Mg.png", width = 8.8, height = 6.7)  

