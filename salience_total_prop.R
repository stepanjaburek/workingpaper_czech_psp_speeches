############################## 
# Setup
library(tidyverse)
library(ggpubr)


df <- read.csv("dataset_mentions.csv")

df$left_right <- ifelse(
  (df$pure_right == 1| df$pure_left == 1|df$both==1),
  1, 0
)
df$all_ideol <- ifelse(
  (df$pure_right == 1| df$pure_left == 1|df$both==1|df$soc==1|df$lib==1|df$con==1),
  1, 0
)

#####################
# recode TOP 09
df <- df %>% 
  mutate(party_2 = case_when(
    party == "TOP09" ~ "TOP 09",
    TRUE ~ party  
  ))
df[df$party %in% c("TOP09", "TOP 09", "ODS"), c("party", "party_2")]

df <- df %>%
  select(!party) %>%
  rename(party = party_2)

########################################
# Total proportions Left vs Right
#########################################x
Ns <- df %>% 
  group_by(party) %>% 
  summarise(n_left = sum(pure_left),
            n_right = sum(pure_right),
            n_both = sum(both)) 

traditional<-Ns %>% filter(party %in% c("ODS", "ČSSD", "KSČM", "KDU-ČSL"))
traditional <- traditional %>%
  mutate(
    total = n_left + n_right,
    left_prop = n_left / total * 100,
    right_prop = n_right / total * 100
  )

ggplot(traditional, aes(x = party, y = 100, fill = "Right")) +
  geom_bar(stat = "identity") +
  geom_bar(aes(y = left_prop, fill = "Left"), stat = "identity") +
  geom_text(aes(y = (100 + left_prop)/2, 
                label = sprintf("%.1f", right_prop)),
            color = "black", size=7) +
  geom_text(aes(y = left_prop/2, 
                label = sprintf("%.1f", left_prop)),
            color = "black", size=7) +
  geom_text(aes(y = 105, 
                label = paste("n =", total+n_both)), 
            color = "black", size=6) +
  geom_hline(yintercept = 50, color = "black", linetype = "dashed", alpha = 1) +
  scale_y_continuous(breaks = seq(0, 110, by = 25)) +
  scale_fill_manual(values = c("Left" = "red1", "Right" = "royalblue")) +
  labs(x = "Party",
       y = "Proportion of mentions",
       fill = "Mention Type"
  ) +
  theme_pubr() +
  theme(legend.position = "right", 
        legend.title = element_blank(),
        legend.text = element_text(size = 30),    
        legend.key.size = unit(2, 'cm'),  
        axis.title = element_text(size = 30),    
        axis.text = element_text(size = 30)) +   
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(0, 110))


traditional_long <- traditional %>%
  select(party, n_left, n_right) %>%
  pivot_longer(cols = c(n_left, n_right),
               names_to = "type",
               values_to = "count") %>%
  mutate(type = ifelse(type == "n_left", "Left", "Right"))


ggplot(traditional_long, 
       aes(x = party, y = count, fill = type)) +
  geom_bar(stat = "identity", 
           position = "dodge") +
  geom_text(aes(label = count),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 7) +
  scale_fill_manual(values = c("Left" = "red1", "Right" = "royalblue")) +
  labs(x = "Party",
       y = "Number of mentions") +
  theme_pubr() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 30),
        legend.key.size = unit(2, 'cm'),
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 30))

# c("Left" = "#F44336", "Right" = "#2196F3")) 

newparties<-Ns %>% filter(party %in% c("ANO", "Piráti","SPD","TOP 09","Úsvit", "VV" ))
newparties <- newparties %>%
  mutate(
    total = n_left + n_right,
    left_prop = n_left / total * 100,
    right_prop = n_right / total * 100
  )

ggplot(newparties, aes(x = party, y = 100, fill = "Right")) +
  geom_bar(stat = "identity") +
  geom_bar(aes(y = left_prop, fill = "Left"), stat = "identity") +
  geom_text(aes(y = (100 + left_prop)/2, 
                label = sprintf("%.1f", right_prop)),
            color = "black", size=7) +
  geom_text(aes(y = left_prop/2, 
                label = sprintf("%.1f", left_prop)),
            color = "black", size=7) +
  geom_text(aes(y = 105, 
                label = paste("n =", total+n_both)), scale_y_continuous(breaks = seq(0, 110, by = 25)) +
            color = "black", size=6) +
  geom_hline(yintercept = 50, color = "black", linetype = "dashed", alpha = 1) +
  scale_y_continuous(breaks = seq(0, 110, by = 25)) +
  scale_fill_manual(values = c("Left" = "red1", "Right" = "royalblue")) +
  labs(x = "Party",
       y = "Proportion of mentions",
       fill = "Mention Type"
  ) +
  theme_pubr() +
  theme(legend.position = "right", 
        legend.title = element_blank(),
        legend.text = element_text(size = 30),     
        legend.key.size = unit(2, 'cm'),  
        axis.title = element_text(size = 30),    
        axis.text = element_text(size = 30)) +   
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(0, 110))

newparties_long <- newparties %>%
  select(party, n_left, n_right) %>%
  pivot_longer(cols = c(n_left, n_right),
               names_to = "type",
               values_to = "count") %>%
  mutate(type = ifelse(type == "n_left", "Left", "Right"))

ggplot(newparties_long, 
       aes(x = party, y = count, fill = type)) +
  geom_bar(stat = "identity", 
           position = "dodge") +
  geom_text(aes(label = count),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 7) +
  scale_fill_manual(values = c("Left" = "red1", "Right" = "royalblue")) +
  labs(x = "Party",
       y = "Number of mentions") +
  theme_pubr() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 30),
        legend.key.size = unit(2, 'cm'),
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 30))


################################################
# Total proportions w/ socialism, liberalism and conservatism
df$only_right <- ifelse(
  (df$pure_right == 1 | df$lib == 1 | df$con == 1) &
    (df$pure_left == 0 & df$soc == 0  ),
  1, 0
)

df$only_left <- ifelse(
  (df$pure_left == 1 | df$soc == 1 ) &
    (df$pure_right == 0 & df$lib == 0 & df$con == 0),
  1, 0
)


As<-df %>% 
  group_by(party) %>% 
  summarise(n_left = sum(only_left),
            n_right = sum(only_right),
            n_both = sum(both)) 

traditional_2<-As %>% filter(party %in% c("ODS", "ČSSD", "KSČM", "KDU-ČSL"))

traditional_2 <- traditional_2 %>%
  mutate(total = n_left + n_right,
         left_prop = n_left / total * 100,
         right_prop = n_right / total * 100)


ggplot(traditional_2, aes(x = party, y = 100, fill = "Right")) +
  geom_bar(stat = "identity") +
  geom_bar(aes(y = left_prop, fill = "Left"), stat = "identity") +
  geom_text(aes(y = (100 + left_prop)/2, 
                label = sprintf("%.1f", right_prop)),
            color = "black", size=7) +
  geom_text(aes(y = left_prop/2, 
                label = sprintf("%.1f", left_prop)),
            color = "black", size=7) +
  geom_text(aes(y = 105, 
                label = paste("n =", total+n_both)), 
            color = "black", size=6) +
  geom_hline(yintercept = 50, color = "black", linetype = "dashed", alpha = 1) +
  scale_y_continuous(breaks = seq(0, 110, by = 25)) +
  scale_fill_manual(values = c("Left" = "red1", "Right" = "royalblue")) +
  labs(x = "Party",
       y = "Proportion of mentions",
       fill = "Mention Type"
  ) +
  theme_pubr() +
  theme(legend.position = "right", 
        legend.title = element_blank(),
        legend.text = element_text(size = 30),     
        legend.key.size = unit(2, 'cm'),  
        axis.title = element_text(size = 30),    
        axis.text = element_text(size = 30)) +   
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(0, 110))

# Reshape data to long format
traditional_2_long <- traditional_2 %>%
  select(party, n_left, n_right) %>%
  pivot_longer(cols = c(n_left, n_right),
               names_to = "type",
               values_to = "count") %>%
  mutate(type = ifelse(type == "n_left", "Left", "Right"))

# Create plot
ggplot(traditional_2_long, 
       aes(x = party, y = count, fill = type)) +
  geom_bar(stat = "identity", 
           position = "dodge") +
  geom_text(aes(label = count),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 7) +
  scale_fill_manual(values = c("Left" = "red1", "Right" = "royalblue")) +
  labs(x = "Party",
       y = "Number of mentions") +
  theme_pubr() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 30),
        legend.key.size = unit(2, 'cm'),
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 30))

newparties_2<-As %>% filter(party %in% c("ANO", "Piráti","SPD","TOP 09","Úsvit", "VV" ))
newparties_2 <- newparties_2 %>%
  mutate(
    total = n_left + n_right,
    left_prop = n_left / total * 100,
    right_prop = n_right / total * 100
  )

ggplot(newparties_2, aes(x = party, y = 100, fill = "Right")) +
  geom_bar(stat = "identity") +
  geom_bar(aes(y = left_prop, fill = "Left"), stat = "identity") +
  geom_text(aes(y = (100 + left_prop)/2, 
                label = sprintf("%.1f", right_prop)),
            color = "black", size=7) +
  geom_text(aes(y = left_prop/2, 
                label = sprintf("%.1f", left_prop)),
            color = "black", size=7) +
  geom_text(aes(y = 105, 
                label = paste("n =", total+n_both)), 
            color = "black", size=6) +
  geom_hline(yintercept = 50, color = "black", linetype = "dashed", alpha = 1) +
  scale_y_continuous(breaks = seq(0, 110, by = 25)) +
  scale_fill_manual(values = c("Left" = "red1", "Right" = "royalblue")) +
  labs(x = "Party",
       y = "Proportion of mentions",
       fill = "Mention Type"
  ) +
  theme_pubr() +
  theme(legend.position = "right", 
        legend.title = element_blank(),
        legend.text = element_text(size = 30),    
        legend.key.size = unit(2, 'cm'),  
        axis.title = element_text(size = 30),    
        axis.text = element_text(size = 30)) +   
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(0, 110))
  
  #scale_fill_manual(values = c("Left" = "#F44336", "Right" = "#2196F3")) +
  

#######################################################################x

# Create only_right column
df$only_right <- ifelse(
  (df$pure_right == 1 | df$lib == 1 | df$con == 1) &
    (df$pure_left == 0 & df$soc == 0 & df$kom == 0 ),
  1, 0
)

# Create only_left column
df$only_left <- ifelse(
  (df$pure_left == 1 | df$soc == 1 | df$kom == 1 ) &
    (df$pure_right == 0 & df$lib == 0 & df$con == 0),
  1, 0
)
