library(tidyverse)
library(ggplot2)
library(ggpubr)
###############
# Data
left<-read.csv("left_final.csv") 
right<-read.csv("right_final.csv")

#################xx
# Total sentiment
left_df <- left %>% 
  mutate(neg = ifelse(label == "negative", 1, 0)) %>%
  group_by(wing) %>%
  summarise(prop_neg_left = sum(neg/n()),
            .groups = "drop")

right_df <- right %>% 
  mutate(neg = ifelse(label == "negative", 1, 0)) %>%
  group_by(wing) %>%
  summarise(prop_neg_right = sum(neg/n()),
            .groups = "drop")

wing_sent<-cbind(left_df,right_df)

wing_sent <- tidyr::pivot_longer(
  wing_sent,
  cols = c(prop_neg_left, prop_neg_right),
  names_to = "type",
  values_to = "count"
)

wing_sent$type <- factor(wing_sent$type, 
                        levels = c("prop_neg_left", "prop_neg_right"),
                        labels = c("Left", "Right"))


wing_sent$count <- round(wing_sent$count * 100, 2)

ggplot(wing_sent, aes(x = wing, y = count, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = sprintf("%.1f%%", count)),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            family = "serif",
            size = 8) +
  scale_fill_manual(values = c("Left" = "#E34234", "Right" = "#4773B8")) +
  labs(x = "Analytical Category",
       y = "Proportion of Negative Mentions",
       fill = "Mention Type") +
  theme_classic() +
  theme(
    text = element_text(family = "serif"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 30, family = "serif"),
    legend.key.size = unit(2, 'cm'),
    axis.title = element_text(size = 30, family = "serif"),
    axis.text = element_text(size = 30, family = "serif"),
    axis.line = element_line(color = "black", size = 1),
    axis.ticks = element_line(color = "black", size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(plot.title = element_text(hjust = 0.5, family = "serif"))

########################################
# Total salience Left vs Right
#########################################x

left<-left %>% select(1:12,29)
right<-right%>% select(1:12,29)
regex<- "\\w*lev\\w*"
both<-rbind(left,right)
both$what<-str_detect(both$matched_word, regex)
salience <- both %>% 
  group_by(wing) %>% 
  summarise(n_left = sum(what==TRUE),
            n_right = sum(what==FALSE))

salience <- salience %>%
  mutate(
    total = n_left + n_right,
    left_prop = n_left / total * 100,
    right_prop = n_right / total * 100
  )

salience <- salience %>%
  mutate(
    total_speeches = c(75875, 56673, 83323), 
    left_sal = n_left / total_speeches * 100,
    right_sal = n_right / total_speeches * 100)

ggplot(salience, aes(x = wing, y = 100, fill = "Right")) +
  geom_bar(stat = "identity") +
  geom_bar(aes(y = left_prop, fill = "Left"), stat = "identity") +
  geom_text(aes(y = (100 + left_prop)/2, 
                label = sprintf("%.1f%%", right_prop)),
            family = "serif",
            color = "black", 
            size = 7) +
  geom_text(aes(y = left_prop/2, 
                label = sprintf("%.1f%%", left_prop)),
            family = "serif",
            color = "black", 
            size = 7) +
  geom_text(aes(y = 105, 
                label = paste("n =", total)), 
            family = "serif",
            color = "black", 
            size = 6) +
  geom_hline(yintercept = 50, 
             color = "black", 
             linetype = "dashed", 
             alpha = 1) +
  scale_y_continuous(breaks = seq(0, 110, by = 25)) +
  scale_fill_manual(values = c("Left" = "#E34234", "Right" = "#4773B8")) +
  labs(x = "Analytical Category",
       y = "Proportion of ideological mentions",
       fill = "Mention Type"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "serif"),
    legend.position = "right", 
    legend.title = element_blank(),
    legend.text = element_text(size = 30, family = "serif"),    
    legend.key.size = unit(2, 'cm'),  
    axis.title = element_text(size = 30, family = "serif"),    
    axis.text = element_text(size = 30, family = "serif"),
    axis.line = element_line(color = "black", size = 1),
    axis.ticks = element_line(color = "black", size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(plot.title = element_text(hjust = 0.5, family = "serif")) +
  coord_cartesian(ylim = c(0, 110))


salience <- tidyr::pivot_longer(
  salience,
  cols = c(left_sal, right_sal),
  names_to = "type",
  values_to = "count"
)

salience$type <- factor(salience$type, 
                           levels = c("left_sal", "right_sal"),
                           labels = c("Left", "Right"))

ggplot(salience, aes(x = wing, y = count, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = sprintf("%.1f%%",count)),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            family = "serif",
            size = 8) +
  scale_fill_manual(values = c("Left" = "#E34234", "Right" = "#4773B8")) +
  labs(x = "Analytical Category",
       y = "% of Speeches Mentioning",
       fill = "Mention Type") +
  theme_classic() +
  theme(
    text = element_text(family = "serif"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 30, family = "serif"),
    legend.key.size = unit(2, 'cm'),
    axis.title = element_text(size = 30, family = "serif"),
    axis.text = element_text(size = 30, family = "serif"),
    axis.line = element_line(color = "black", size = 1),
    axis.ticks = element_line(color = "black", size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(plot.title = element_text(hjust = 0.5, family = "serif"))


