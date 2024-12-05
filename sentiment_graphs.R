library(tidyverse)
library(ggplot2)
library(ggpubr)



left_df <- read.csv("debate_sentiment_left.csv")
right_df <- read.csv("debate_sentiment_right.csv")


# TOP 09 recode
left_df <- left_df %>% 
  mutate(party_2 = case_when(
    party == "TOP09" ~ "TOP 09",
    TRUE ~ party  # Keep other values unchanged
  ))

left_df[left_df$party %in% c("TOP09", "TOP 09"), c("party", "party_2")]

right_df <- right_df %>% 
  mutate(party_2 = case_when(
    party == "TOP09" ~ "TOP 09",
    TRUE ~ party  # Keep other values unchanged
  ))

left_df <- left_df %>%
  select(!party) %>%
  rename(party = party_2)

right_df <- right_df %>%
  select(!party) %>%
  rename(party = party_2)


#data manipulation

left_df1 <- left_df %>% 
  mutate(neg = ifelse(label == "negative", 1, 0)) %>%
  group_by(party) %>%
  summarise(prop_neg_left = sum(neg/n()),
            .groups = "drop")

right_df1 <- right_df %>% 
  mutate(neg = ifelse(label == "negative", 1, 0)) %>%
  group_by(party) %>%
  summarise(prop_neg_right = sum(neg/n()),
            .groups = "drop")


comparison_df <- left_df1 %>%
  left_join(right_df1, by = "party") %>% 
  mutate(agg_left = mean(prop_neg_left, na.rm=T),
         agg_right = mean(prop_neg_right, na.rm=T)) %>%
  mutate(demean_right = prop_neg_right - agg_right, 
         demean_left = prop_neg_left - agg_left)


comparison_df_long <- comparison_df %>%
  pivot_longer(demean_right:demean_left,
               names_to = "type",
               values_to = "value")

#factor for category names

comparison_df_long1 <- comparison_df_long %>%
                      mutate(type = factor(type, 
                          levels = c("demean_right", "demean_left"),
                          labels = c("Right", "Left")))

table(comparison_df_long$type, comparison_df_long1$type)




## demeaned bar plots 

comparison_df_long1 %>%
  filter(party %in% c("ODS", "ČSSD", "KDU-ČSL", "KSČM")) %>% 
  ggplot(aes(x = party, y = value, fill = type)) + 
  geom_bar(stat="identity", 
           position=position_dodge(),
           width = 0.75)+
  geom_text(aes(label = round(value, digits = 3)), 
            position = position_dodge(width = 0.7),
            vjust = 1.4,
            color = "black", 
            size = 3.5) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=c('royalblue1','red1'))+
  labs(x = "party", y = "de-meaned share of negative mentions")+
  theme_pubr() +
  theme(legend.position = "right", legend.title  = element_blank())

ggsave("plots/demean_sentiment_old_parties_neg.jpg", width = 10, height = 6)
  
  
  
comparison_df_long1 %>%
    filter(party %in% c("ANO", "Piráti", "VV", "SPD", "Úsvit", "TOP 09")) %>% 
    ggplot(aes(x = party, y = value, fill = type)) + 
    geom_bar(stat="identity", 
             position=position_dodge(),
             width = 0.75)+
    geom_text(aes(label = round(value, digits = 3)), 
            position = position_dodge(width = 0.9),
            vjust = 1.4,
            color = "black", 
            size = 3.5) +
    geom_hline(yintercept = 0) +
    scale_fill_manual(values=c('royalblue1','red1'))+
    labs(x = "party", y = "de-meaned share of negative mentions")+
    theme_pubr()+
    theme(legend.position = "right", legend.title  = element_blank())

ggsave("plots/demean_sentiment_new_parties_neg.jpg", width = 10, height = 6)




comparison_df_long1 %>%
  filter(party %in% c("ODS", "KDU-ČSL", "ODA", "TOP 09")) %>% 
  ggplot(aes(x = party, y = value, fill = type)) + 
  geom_bar(stat="identity", 
           position=position_dodge(),
           width = 0.75)+
  geom_text(aes(label = round(value, digits = 3)), 
            position = position_dodge(width = 0.9),
            vjust = 1.6,
            color = "black", 
            size = 3.5) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=c('royalblue1','red1'))+
  labs(x = "party", y = "de-meaned share of negative mentions")+
  theme_pubr()+
  theme(legend.position = "right", legend.title  = element_blank())

ggsave("plots/demean_sentiment_rightists_neg.jpg", width = 10, height = 6)


unique(comparison_df_long1$party)

comparison_df_long1 %>%
  filter(party %in% c("ČSSD", "KSČM", "Piráti")) %>% 
  ggplot(aes(x = party, y = value, fill = type)) + 
  geom_bar(stat="identity", 
           position=position_dodge(),
           width = 0.75)+
  geom_text(aes(label = round(value, digits = 3)), 
            position = position_dodge(width = 0.9),
            vjust = 1.6,
            color = "black", 
            size = 3.5) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=c('royalblue1','red1'))+
  labs(x = "party", y = "de-meaned share of negative mentions")+
  theme_pubr()+
  theme(legend.position = "right", legend.title  = element_blank())

ggsave("plots/demean_sentiment_leftists_neg.jpg", width = 10, height = 6)


  
    
###############################################
## proportion of negative mentions by parties##
###############################################

left_sentiment_old_parties_neg <- left_df %>% 
  mutate(label = ifelse(label == "negative", 1, 0)) %>% 
  group_by(party) %>%
  summarise(sum_neg = sum(label)/n()) %>% 
  filter(party %in% c("ODS", "ČSSD", "KDU-ČSL", "KSČM")) %>% 
  ggplot(aes(x = sum_neg, y = fct_reorder(party, sum_neg), fill = party)) + 
  geom_col(show.legend = F) +
  scale_fill_manual(values = c("ODS" = "blue", "ČSSD" = "orange", "KDU-ČSL" = "yellow", "KSČM" = "red")) +
  xlab("proportion of mentions with negative sentiment") +
  ylab ("party") +
  theme_pubr()
left_sentiment_old_parties_neg

ggsave("plots/left_sentiment_old_parties_neg.jpg", width = 8, height = 6)

left_sentiment_new_parties_neg <- left_df %>% 
  mutate(label = ifelse(label == "negative", 1, 0)) %>% 
  group_by(party) %>%
  summarise(sum_neg = sum(label)/n()) %>% 
  filter(party %in% c("ANO", "Piráti", "VV", "SPD", "Úsvit")) %>% 
  ggplot(aes(x = sum_neg, y = fct_reorder(party, sum_neg), fill = party)) + 
  geom_col(show.legend = F) +
  scale_fill_manual(values = c("ANO" = "blue", "Piráti" = "black", "VV" = "red", "SPD" = "brown", "Úsvit" = "purple")) +
  xlab("proportion of mentions with negative sentiment") +
  ylab ("party") +
  theme_pubr()
left_sentiment_new_parties_neg

ggsave("plots/left_sentiment_new_parties_neg.jpg", width = 8, height = 6)

right_sentiment_old_parties_neg <- right_df %>% 
  mutate(label = ifelse(label == "negative", 1, 0)) %>% 
  group_by(party) %>%
  summarise(sum_neg = sum(label)/n()) %>% 
  filter(party %in% c("ODS", "ČSSD", "KDU-ČSL", "KSČM")) %>% 
  ggplot(aes(x = sum_neg, y = fct_reorder(party, sum_neg), fill = party)) + 
  geom_col(show.legend = F) +
  scale_fill_manual(values = c("ODS" = "blue", "ČSSD" = "orange", "KDU-ČSL" = "yellow", "KSČM" = "red")) +
  xlab("proportion of mentions with negative sentiment") +
  ylab ("party") +
  theme_pubr()
right_sentiment_old_parties_neg

ggsave("plots/right_sentiment_old_parties_neg.jpg", width = 8, height = 6)

right_sentiment_new_parties_neg <- right_df %>% 
  mutate(label = ifelse(label == "negative", 1, 0)) %>% 
  group_by(party) %>%
  summarise(sum_neg = sum(label)/n()) %>% 
  filter(party %in% c("ANO", "Piráti", "VV", "SPD", "Úsvit")) %>% 
  ggplot(aes(x = sum_neg, y = fct_reorder(party, sum_neg), fill = party)) + 
  geom_col(show.legend = F) +
  scale_fill_manual(values = c("ANO" = "blue", "Piráti" = "black", "VV" = "red", "SPD" = "brown", "Úsvit" = "purple")) +
  xlab("proportion of mentions with negative sentiment") +
  ylab ("party") +
  theme_pubr()
right_sentiment_new_parties_neg

ggsave("plots/right_sentiment_new_parties_neg.jpg", width = 8, height = 6)
