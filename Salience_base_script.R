library(tidyverse)
library(quanteda)
library(ggpubr)

df <- read.csv("dataset_mentions.csv")


##############################################################################
## comparing proportions of pure left, pure right and both mentions mentions##
##############################################################################


## aggregate yearly proportions of pure left, pure right and both mentions

prop_agg <- df %>% 
  group_by(year) %>% 
  summarise(prop_agg_right = sum(pure_right)/( sum(pure_right) + sum(pure_left) + sum (both)),
            prop_agg_left =  sum(pure_left)/( sum(pure_right) + sum(pure_left) + sum (both)),
            prop_agg_both = sum(both)/(sum(pure_right) + sum(pure_left) + sum(both)),
            .groups = "drop")

#party level proportions

prop_party <- df %>% 
  group_by(year, party) %>% 
  summarise(prop_party_right = sum(pure_right)/( sum(pure_right) + sum(pure_left) + sum (both)),
            prop_party_left = sum(pure_left)/( sum(pure_right) + sum(pure_left) + sum (both)),
            prop_party_both = sum(both)/(sum(pure_right) + sum(pure_left) + sum(both)),
            .groups = "drop")

#comparing party level proportions to the aggregate yearly proportions

comparison_df <- prop_party %>% 
  left_join(prop_agg, by = "year")


###################################################################
## dataframe for simple overall salience plotting of the categories 

yearly <- df %>% 
  group_by(year) %>% 
  summarise(prop_left = sum(pure_left)/n(),
            prop_right = sum(pure_right)/n(),
            prop_both = sum(both)/n(),
            .groups = "drop")

   ## pivot the dataset to get type of mention (pure left, pure right, both) into one variables

yearly_long <- yearly %>% 
  pivot_longer(
    cols = prop_left:prop_both,
    names_to = "type",
    values_to = "value"
  )

    ##  factor for readable category names in graphing
yearly_long$type <- factor(yearly_long$type,
                           levels = c("prop_both", "prop_left", "prop_right"),
                           labels = c("both", "pure left", "pure right"))







########################
### Graphing  ##########
#########################


## de-meneaned right mentions

comparison_df %>% 
  filter(party %in% c("ODS", "ČSSD", "KDU-ČSL", "KSČM")) %>% 
  ggplot(aes(x = year, y = prop_party_right - prop_agg_right, color = party)) +
  geom_smooth(method = "loess", span = 0.66, se =F) +
  scale_x_continuous(breaks = seq(1993, 2023, by =5)) + 
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.7, color = "black") +
  theme_pubr() + 
  labs(x = "year", y = "de-meaned salience", title = "Right mentions") + 
  scale_color_manual(values = c("ODS" = "blue", "ČSSD" = "orange", "KDU-ČSL" = "yellow", "KSČM" = "red"))


ggsave("plots/right_comp_smooth.jpg", width = 6, height = 4)


## de-meneaned left mentions

comparison_df %>% 
  filter(party %in% c("ODS", "ČSSD", "KDU-ČSL", "KSČM")) %>% 
  ggplot(aes(x = year, y = prop_party_left - prop_agg_left, color = party)) +
  geom_smooth(method = "loess", span = 0.66, se =F) +
  scale_x_continuous(breaks = seq(1993, 2023, by =5)) + 
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.7, color = "black") +
  theme_pubr() + 
  labs(x = "year", y = "de-meaned salience", title = "Left mentions") + 
  scale_color_manual(values = c("ODS" = "blue", "ČSSD" = "orange", "KDU-ČSL" = "yellow", "KSČM" = "red"))


ggsave("plots/left_comp_smooth.jpg", width = 6, height = 4)


#### overall salience plotting

ggplot(yearly_long, aes(x = year, y = value, color = type)) + 
  geom_smooth(method = "loess", span = 0.2, se = F) + 
  theme_pubr() +
  labs(y = "% of yearly speeches", x = "", title = "")

ggsave("plots/overall.jpg", width = 6, height = 4)


