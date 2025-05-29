library(tidyverse)
library(ggpubr)
library(patchwork)
library(readr)
library(fixest)
#install.packages("lme4")
library(lme4)
library(sjstats)
library(ggExtra)
library(lubridate)
library(coefplot)
library(ggbeeswarm)
library(RColorBrewer)


# Data prep (OR LOAD DFs BELOW) --------------------------------------------------------------

library(data.table)

df <- fread("data/dataset_mentions_controls2.csv")


# declare left-wing parties
communist <- c("LB", "KSČM")
cssd <- c("ČSSD", "ČSSD (NEZ)")

#merge Levý blok and KSČM
df_party <- df %>%
  mutate(
    party_dis = case_when(
      party %in% communist ~ "KSČM",
      party %in% cssd ~ "ČSSD",
      TRUE ~ party 
    )
  )

#  left and right salience by wings (OR LOAD DFs BELOW) -------------------

df_sal <- df %>%
  group_by(wing, year) %>%
  reframe(
   prop_right_raw = 
     sum(right_mentions >=1) / n(), # raw right salience
    prop_left_raw = 
     sum(left_mentions >= 1) / n(), # raw left salience
    prop_right = 
      sum(pure_right == 1)/ n(), # pure right salience
    prop_left = 
      sum(pure_left == 1)/ n(), # pure left salience,
    prop_right_both = 
      sum(pure_right == 1|both == 1)/ n(), # alternate operationalization of raw salience
    prop_left_both = 
      sum(pure_left == 1|both == 1)/ n(), # alternate operationalization of raw salience
  )


## save dataset to save computing power

#write.csv(df_sal, "data/dataset_raw_salience.csv")




# left and right salience by parties (OR LOAD DFs BELOW) ------------------


df_sal_party <- df_party %>%
  group_by(party_dis, year) %>%
  reframe(
    prop_right_raw = 
      sum(right_mentions >=1) / n(), # raw right salience
    prop_left_raw = 
      sum(left_mentions >= 1) / n(), # raw left salience
    prop_right = 
      sum(pure_right == 1)/ n(), # pure right salience
    prop_left = 
      sum(pure_left == 1)/ n(), # pure left salience,
    prop_right_both = 
      sum(pure_right == 1|both == 1)/ n(), # alternate operationalization of raw salience
    prop_left_both = 
      sum(pure_left == 1|both == 1)/ n() # alternate operationalization of raw salience
  )


## save dataset to save computing power

#write_csv(df_sal_party, "data/dataset_raw_salience_party.csv")


# monthly 

df_sal_party_monthly <- df_party %>%
  group_by(party_dis, month) %>%
  reframe(
    prop_right_raw = 
      sum(right_mentions >=1) / n(), # raw right salience
    prop_left_raw = 
      sum(left_mentions >= 1) / n(), # raw left salience
    prop_right = 
      sum(pure_right == 1)/ n(), # pure right salience
    prop_left = 
      sum(pure_left == 1)/ n(), # pure left salience,
    prop_right_both = 
      sum(pure_right == 1|both == 1)/ n(), # alternate operationalization of raw salience
    prop_left_both = 
      sum(pure_left == 1|both == 1)/ n() # alternate operationalization of raw salience
  )


## save dataset to save computing power

write_csv(df_sal_party_monthly, "data/dataset_raw_salience_party_monthly.csv")

# Load prepared DFs -------------------------------------------------------


df_sal <- read_csv("data/dataset_raw_salience.csv")

df_sal_party <- fread("data/dataset_raw_salience_party.csv")

df_sal_party_monthly <- fread("data/dataset_raw_salience_party_monthly.csv")




# declare left and right saliences

left_salience <-  c("prop_left", "prop_left_raw", "prop_left_both")
right_salience <-  c("prop_right", "prop_right_raw", "prop_right_both")


df_sal_long <- df_sal %>%  # df by wings
  pivot_longer(
    cols = prop_right_raw:prop_left_both,
    values_to = "salience",
    names_to = "term"
  ) %>% 
  mutate(    # self and opposition salience distinction
    relation = case_when(
      wing == "Left_Wing" & term %in% left_salience ~ "self salience",
      wing == "Right_Wing" & term %in% right_salience ~ "self salience",
      TRUE ~ "opposition salience"
    )
  )

   # relation variable always evaluates to opposition salience for new and other parties



## Are the differences significant?

# self salience

self_df <- df_sal_long %>%
  filter(
    wing %in% c("Left_Wing", "Right_Wing") &
    relation == "self salience"
  )

self_fe <- feols(salience ~ wing | year, self_df)
self_ols <- lm(salience ~ wing + year, self_df)

summary(self_fe)

# prediction plot
library(ggeffects)

pred1 <- ggpredict(self_ols, terms = c("year", "wing"))

pred_self <- ggplot(pred1, aes(x = x, y = predicted, color = group)) +
                     geom_line() +
                     geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
                     labs(x = "Year", y = "salience",
                          title = "predicted self salience") +
                     theme_classic2()  +
    scale_x_continuous(
    limits = c(1993,2023),
    breaks = seq(1993, 2023, by =5)) +
  theme(legend.position = "none")
  
                   

# opposition salience
opp_df <- df_sal_long %>%
  filter(
    wing %in% c("Left_Wing", "Right_Wing") &
      relation == "opposition salience"
  )

opp_fe <- feols(salience ~ wing | year, opp_df)


opp_ols <- lm(salience ~ wing + year, opp_df)


pred2 <- ggpredict(opp_ols, terms = c("year", "wing"))

pred_opp <- ggplot(pred2, aes(x = x, y = predicted, color = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(x = "Year", y = "",
       title = "predicted opposition salience") +
  theme_classic2()+
  scale_x_continuous(
    limits = c(1993,2023),
    breaks = seq(1993, 2023, by =5)
  )


pred_plots <- pred_self + pred_opp +
  plot_annotation(title = "Predicted self and opposition salience for left- 
                  and right-wing parties",
                  theme = theme(plot.title = element_text(hjust = 0.5))
  )



ggsave(plot = pred_plots, filename = "plots/pred_plots_self.jpg", 
       width = 10, height = 7,
       units = "in")



# Data prep for salience boxplots --------------------------------------------------


# declare party groups
communist <- c("LB", "KSČM")
cssd <- "ČSSD"
right_wing <- c("ODS", "KDU-ČSL", "US", "US-DEU", "SPR-RSČ", "ODA")

right_wing_ohneKDU <- c("ODS", "US", "US-DEU", "SPR-RSČ", "ODA")

ods_kdu <- c("ODS", "KDU-ČSL")

liberals <- c("US", "US-DEU", "ODA")

df_sal_party <- df_sal_party %>%
  mutate(party_dis_alt = case_when(
           party_dis %in% liberals ~ "ODA/US/DEU", 
    TRUE~ party_dis)
  )

df_sal_party_long <- df_sal_party %>%  # df by parties
  pivot_longer(
    cols = prop_right_raw:prop_left_both,
    values_to = "salience",
    names_to = "term"
  ) %>% 
  mutate(    # self and opposition salience distinction
    relation = case_when(
      party_dis %in% c(communist, cssd) & term %in% left_salience ~ "self salience",
      party_dis %in% right_wing & term %in% right_salience ~ "self salience",
      TRUE ~ "opposition salience"
    ),
    wing = case_when(
      party_dis %in% c(communist, cssd) ~ "Left wing",
      party_dis %in% c(right_wing) ~ "Right wing",
      TRUE ~ "New party"
    )
  )


df_sal_party_long_month <- df_sal_party_monthly %>%  # df by parties
  pivot_longer(
    cols = prop_right_raw:prop_left_both,
    values_to = "salience",
    names_to = "term"
  ) %>% 
  mutate(    # self and opposition salience distinction
    relation = case_when(
      party_dis %in% c(communist, cssd) & term %in% left_salience ~ "self salience",
      party_dis %in% right_wing & term %in% right_salience ~ "self salience",
      TRUE ~ "opposition salience"
    ),
    wing = case_when(
      party_dis %in% c(communist, cssd) ~ "Left wing",
      party_dis %in% c(right_wing) ~ "Right wing",
      TRUE ~ "New party"
    )
  )




self_df <- df_sal_party_long  %>%
  filter(party_dis %in% c(communist, cssd, right_wing) &
           relation == "self salience" &
           term %in% c("prop_left_raw", "prop_right_raw")
  ) %>%
  rename(self_salience = salience) %>%
  mutate(log_self = log(self_salience +0.0000000000001))

opp_df <- df_sal_party_long  %>%
  filter(party_dis %in% c(communist, cssd, right_wing) &
           relation == "opposition salience" &
           term %in% c("prop_left_raw", "prop_right_raw")
  ) %>%
  rename(opp_salience = salience) %>%
  mutate(log_opp = log(opp_salience+0.0000000000001))






# left - right wing box plots


self_plot_wing <- self_df %>%
  filter(party_dis %in% c(communist, cssd, right_wing)) %>%
  ggplot(aes(x = wing, y = self_salience, color = wing))+
  geom_beeswarm(alpha = 0.4)+
  geom_boxplot(alpha = 0, width = 0.3, notch = T)+
  theme_minimal()+
  theme(legend.position = "none") +
  labs(x = "party group", y = "salience", title = "self salience")+
  scale_color_brewer(palette = "Set1") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        title = element_text(size = 16))

opp_plot_wing <- opp_df %>%
  filter(party_dis %in% c(communist, cssd, right_wing)) %>%
  ggplot(aes(x = wing, y = opp_salience, color = wing))+
  geom_beeswarm(alpha = 0.4)+
  geom_boxplot(alpha = 0, width = 0.3, notch = T)+
  theme_minimal()+
  theme(legend.position = "none") +
  labs(x = "party group", y = "salience", title = "opposition salience")+
  scale_color_brewer(palette = "Set1")+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        title = element_text(size = 16))


box_wing <- self_plot_wing + opp_plot_wing 
box_wing 

ggsave(plot = box_wing, filename = "plots/box_wing_salience.jpg", 
       width = 10, height = 7,
       units = "in")




# dissagregated by parties


library(forcats)
self_plot_party <- self_df %>%
  filter(party_dis_alt %in% c("ČSSD", "KDU-ČSL", "KSČM", "ODS", "ODA/US/DEU")) %>%
  ggplot(aes(x = fct_reorder(party_dis_alt, wing), y = self_salience, color = party_dis_alt))+
  geom_beeswarm(alpha = 0.4)+
  geom_boxplot(alpha = 0, width = 0.3, notch = T)+
  theme_minimal()+
  theme(legend.position = "none") +
  labs(x = "", y = "salience", title = "self salience")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6))+
  scale_color_brewer(palette = "Set2") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        title = element_text(size = 16))


opp_plot_party <- opp_df %>%
  filter(party_dis_alt %in% c("ČSSD", "KDU-ČSL", "KSČM", "ODS", "ODA/US/DEU")) %>%
  ggplot(aes(x = fct_reorder(party_dis_alt, wing), y = opp_salience, color = party_dis_alt))+
  geom_beeswarm(alpha = 0.4)+
  geom_boxplot(alpha = 0, width = 0.3, notch = T)+
  theme_minimal()+
  theme(legend.position = "none") +
  labs(x = "", y = "salience", title = "opposition salience")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6))+
  scale_color_brewer(palette = "Set2") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        title = element_text(size = 16))


box_party <- self_plot_party + opp_plot_party

ggsave(plot = box_party, filename = "plots/box_party_salience2.jpg", 
       width = 10, height = 7,
       units = "in")

  


# PF 2026
self_df %>%
  filter(party_dis %in% c("ČSSD", "KDU-ČSL", "KSČM", "ODS")) %>%
  ggplot(aes(x = party_dis, y = self_salience, color = party_dis))+
  geom_beeswarm() +
  theme_void() +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0,0.4)) +
  theme(panel.background = element_rect(fill = "skyblue3"))+
  scale_color_manual(
    values = c("ČSSD" = "forestgreen","ODS"= "darkgreen", "KSČM" = "lightgreen","KDU-ČSL"= "green")
    )+ 
  annotate("text", x = 0.5, y = 0.3, label = "PF 2026")


ggsave("plots/PF2026.jpg", width = 5, height = 5)




# Data prep for plotting --------------------------------------------------




self_df_left <- df_sal_party_long  %>%
  filter(party_dis %in% c(communist, cssd) &
           relation == "self salience" &
           term %in% c("prop_left_raw", "prop_right_raw")
         )%>%
  rename(party = party_dis) %>%
  arrange(year, party) %>%
  mutate(paired = rep(1:(n()/2),each=2)) %>%
  ungroup()

opp_df_left <- df_sal_party_long  %>%
  filter(party_dis %in% c(communist, cssd) &
           relation == "opposition salience" &
           term %in% c("prop_left_raw", "prop_right_raw")
  ) %>%
  rename(party = party_dis) %>%
  arrange(year, party) %>%
  mutate(paired = rep(1:(n()/2),each=2)) %>%
  ungroup()


self_df_right <- df_sal_party_long  %>%
  filter(party_dis %in% c(right_wing) &
          relation == "self salience" &
           term %in% c("prop_left_raw", "prop_right_raw"))%>%
           rename(party = party_dis) 

opp_df_right <- df_sal_party_long  %>%
  filter(party_dis %in% c(right_wing) &
          relation == "opposition salience" &
           term %in% c("prop_left_raw", "prop_right_raw")) %>%
           rename(party = party_dis)%>%
  arrange(year, party) 


self_df_ods_kdu <- df_sal_party_long  %>%
  filter(party_dis %in% ods_kdu &
           relation == "self salience" &
           term %in% c("prop_left_raw", "prop_right_raw"))%>%
  rename(party = party_dis)  %>%
  arrange(year, party) %>%
  mutate(paired = rep(1:(n()/2),each=2)) %>%
  ungroup()

opp_df_ods_kdu <- df_sal_party_long  %>%
  filter(party_dis %in% ods_kdu &
           relation == "opposition salience" &
           term %in% c("prop_left_raw", "prop_right_raw")) %>%
  rename(party = party_dis)  %>%
  arrange(year, party) %>%
  mutate(paired = rep(1:(n()/2),each=2)) %>%
  ungroup()
  



# relation variable always evaluates to opposition salience for new and other parties



# Plotting by wings ----------------------------------------------------------------

# self salience plot with left parties dissagregated

self_raw_left <- df_sal_long %>% 
  filter(
    relation == "self salience" & 
      wing %in% c("Left_Wing", "Right_Wing") &
      term %in% c("prop_right_raw", "prop_left_raw")
  ) %>%
  ggplot(aes(x = year, y = salience)) + 
  geom_line(aes(color = wing),
                linewidth = 1.2,
                 alpha = 0.8) +
  geom_point(data = self_df_left,
            aes(x= year, y = salience,
                shape = party),
            size = 1.2
            )+
  geom_line(data = self_df_left, aes(group = paired), 
            color = "darkgrey")+
  labs(title = "self salience", y = "salience") + 
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(
    limits = c(1993,2023),
    breaks = seq(1993, 2023, by =5)
  )
  

opp_raw_left <- df_sal_long %>% 
  filter(relation == "opposition salience" & 
           wing %in% c("Left_Wing", "Right_Wing") &
           term %in% c("prop_right_raw", "prop_left_raw")
  ) %>% 
  ggplot(aes(x = year, y = salience)) + 
  geom_line(aes(
    color = wing),
    linewidth = 1.2,
    alpha = 0.8,
  ) +
  geom_point(data= opp_df_left, aes(x = year, y = salience, 
                                shape = party),
             size =1.2)+ 
  geom_line(data = opp_df_left, aes(group = paired), 
             color = "darkgrey")+
  labs(title = "opposition salience", y = "") + 
  theme_minimal()+
  scale_x_continuous(
    limits = c(1993,2023),
    breaks = seq(1993, 2023, by =5)
  ) 




# self salience plot with right parties dissagregated


self_raw_right <- df_sal_long %>% 
  filter(
    relation == "self salience" & 
      wing %in% c("Left_Wing", "Right_Wing") &
      term %in% c("prop_right_raw", "prop_left_raw")
  ) %>%
  ggplot(aes(x = year, y = salience)) + 
  geom_line(aes(color = wing),
            linewidth = 1.2,
            alpha = 0.8) +
  geom_point(data = self_df_ods_kdu,
             aes(x= year, y = salience,
                 shape = party),
             size = 1.2
  )+
  geom_line(data = self_df_ods_kdu, aes(group = paired), 
            color = "black")+
  labs(title = "self salience", y = "salience") + 
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(
    limits = c(1993,2023),
    breaks = seq(1993, 2023, by =5)
  )


opp_raw_right <- df_sal_long %>% 
  filter(relation == "opposition salience" & 
           wing %in% c("Left_Wing", "Right_Wing") &
           term %in% c("prop_right_raw", "prop_left_raw")
  ) %>% 
  ggplot(aes(x = year, y = salience)) + 
  geom_line(aes(
    color = wing),
    linewidth = 1.2,
    alpha = 0.8,
  ) +
  geom_point(data= opp_df_ods_kdu, aes(x = year, y = salience, 
                                     shape = party),
             size =1.2)+ 
  geom_line(data = opp_df_ods_kdu, aes(group = paired), 
            color = "black")+
  labs(title = "opposition salience", y = "") + 
  theme_minimal()+
  scale_x_continuous(
    limits = c(1993,2023),
    breaks = seq(1993, 2023, by =5)
  )


## combining the plots 
raw_plots_left <- self_raw_left + opp_raw_left +
  plot_annotation(title = "",
                  theme = theme(plot.title = element_text(hjust = 0.5))
  )


ggsave(plot = raw_plots_left, filename = "plots/raw_plots_left.jpg", 
       width = 10, height = 7,
       units = "in")






raw_plots_right <- self_raw_right + opp_raw_right +
  plot_annotation(title = "Including speeches that mention both left and right",
                  theme = theme(plot.title = element_text(hjust = 0.5))
  )


ggsave(plot = raw_plots_right, filename = "plots/raw_plots_right.jpg", 
       width = 10, height = 7,
       units = "in")






# Excluding various right-wing parties  -------------------------------------------------------

self_raw_exkdu <- df_sal_party_long %>% 
  filter(party_dis %in% c(communist, cssd, "ODS")) %>%
  filter(
    relation == "self salience" & 
      wing %in% c("Left wing", "Right wing") &
      term %in% c("prop_right_raw", "prop_left_raw")
  ) %>%
  ggplot(aes(x = year, y = salience)) + 
  geom_line(aes(color = wing),
            linewidth = 1.2,
            alpha = 0.8) +
  geom_point(data = self_df_left,
             aes(x= year, y = salience,
                 shape = party),
             size = 1.2
  )+
  geom_line(data = self_df_left, aes(group = paired), 
            color = "black")+
  labs(title = "self salience", y = "salience") + 
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(
    limits = c(1993,2023),
    breaks = seq(1993, 2023, by =5)
  )


opp_raw_exkdu <- df_sal_party_long %>% 
  filter(party_dis %in% c(communist, cssd, "ODS")) %>%
  filter(relation == "opposition salience" & 
           wing %in% c("Left wing", "Right wing") &
           term %in% c("prop_right_raw", "prop_left_raw")
  ) %>% 
  ggplot(aes(x = year, y = salience)) + 
  geom_line(aes(
    color = wing),
    linewidth = 1.2,
    alpha = 0.8,
  ) +
  geom_point(data= opp_df_left, aes(x = year, y = salience, 
                                       shape = party),
             size =1.2)+ 
  geom_line(data = opp_df_left, aes(group = paired), 
            color = "black")+
  labs(title = "opposition salience", y = "") + 
  theme_minimal()+
  scale_x_continuous(
    limits = c(1993,2023),
    breaks = seq(1993, 2023, by =5)
  )


raw_plots_ohne_kdu <- self_raw_exkdu + opp_raw_exkdu +
  plot_annotation(title = "Including speeches that mention both left and right",
                  theme = theme(plot.title = element_text(hjust = 0.5))
  )




# Parties dissagregated plot 
#CSSD - KSCM vs ODS - KDU-CSL comparison ---------------------------------------------------------



self_raw_right <- df_sal_long %>% 
  filter(
    relation == "self salience" & 
      wing %in% c("Left_Wing", "Right_Wing") &
      term %in% c("prop_right_raw", "prop_left_raw")
  ) %>%
  ggplot(aes(x = year, y = salience)) + 
  geom_line(aes(color = wing),
            linewidth = 1.2,
            alpha = 0.8) +
  geom_point(data= opp_df_ods_kdu, aes(x = year, y = salience,
                                       shape = party,
                                       color = wing),
             size =1.2)+ 
  geom_point(data = self_df_left,
             aes(x= year, y = salience,
                 shape = party,
                 color = wing),
             size = 1.2
  )+
  geom_line(data = self_df_left, aes(group = paired), 
            color = "darkgrey")+
  geom_line(data = opp_df_ods_kdu, aes(group = paired), 
            color = "lightblue",
            linetype = "dashed")+
  labs(title = "self salience", y = "salience") + 
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(
    limits = c(1993,2023),
    breaks = seq(1993, 2023, by =5)
  )


opp_raw_right <- df_sal_long %>% 
  filter(relation == "opposition salience" & 
           wing %in% c("Left_Wing", "Right_Wing") &
           term %in% c("prop_right_raw", "prop_left_raw")
  ) %>% 
  ggplot(aes(x = year, y = salience)) + 
  geom_line(aes(
    color = wing),
    linewidth = 1.2,
    alpha = 0.8,
  ) +
  geom_point(data= opp_df_ods_kdu, aes(x = year, y = salience,
                                       shape = party,
                                     color = wing),
             size =1.2)+ 
  geom_point(data = opp_df_left,
             aes(x= year, y = salience,
                 color = wing,
                 shape = party),
             size = 1.2
  )+
  geom_line(data = opp_df_left, aes(group = paired), 
            color = "darkgrey")+
  geom_line(data = opp_df_ods_kdu, aes(group = paired), 
            color = "lightblue",
            linetype = "dashed")+
  labs(title = "opposition salience", y = "") + 
  theme_minimal()+
  scale_x_continuous(
    limits = c(1993,2023),
    breaks = seq(1993, 2023, by =5)
  )



raw_plots_right_diss <- self_raw_right + opp_raw_right +
  plot_annotation(title = "Including speeches that mention both left and right",
                  theme = theme(plot.title = element_text(hjust = 0.5))
  )

ggsave(plot = raw_plots_right_diss, filename = "plots/raw_plots_right_diss.jpg", 
       width = 10, height = 7,
       units = "in")











# Plotting by parties -----------------------------------------------------






# over time salience with left dissagregated 
self_raw_party_left <- df_sal_party_long %>% 
  filter(
    relation == "self salience" & 
      party_dis %in% c("KSČM", "ČSSD", "Right wing") &
      term %in% c("prop_left_raw", "prop_right_raw")
  ) %>%
  ggplot(aes(x = year, y = salience)) + 
  geom_line(aes(
    color = party_dis,
    linetype = party_dis)
  ) +
  labs(title = "self salience", y = "salience") + 
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(
    limits = c(1993,2023),
    breaks = seq(1993, 2023, by =5)
  )


opp_raw_party_left <- df_sal_party_long %>% 
  filter(relation == "opposition salience" & 
           party_dis %in% c("KSČM", "ČSSD", "Right wing") &
           term %in% c("prop_right_raw", "prop_left_raw")
  ) %>% 
  ggplot(aes(x = year, y = salience)) + 
  geom_line(aes(
    color = party_dis,
    linetype = party_dis)
  ) + 
  labs(title = "opposition salience", y = "") + 
  theme_minimal()+
  scale_x_continuous(
    limits = c(1993,2023),
    breaks = seq(1993, 2023, by =5)
  )





raw_plots_party_left <- self_raw_party_left + opp_raw_party_left +
  plot_annotation(title = "Self and opposition salience for 
                  left-wing parties",
                  theme = theme(plot.title = element_text(hjust = 0.5))
  )



ggsave(plot = raw_plots_party_left, filename = "plots/raw_plots_party_left.jpg", 
       width = 10, height = 7,
       units = "in")




# over time salience with left dissagregated 


self_raw_party_right <- df_sal_party_long %>% 
  filter(
    relation == "self salience" & 
      party_dis %in% c("KSČM", "ČSSD", "Right wing") &
      term %in% c("prop_left_raw", "prop_right_raw")
  ) %>%
  ggplot(aes(x = year, y = salience)) + 
  geom_line(aes(
    color = party_dis,
    linetype = party_dis)
  ) +
  labs(title = "self salience", y = "salience") + 
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(
    limits = c(1993,2023),
    breaks = seq(1993, 2023, by =5)
  )


opp_raw_party_left <- df_sal_party_long %>% 
  filter(relation == "opposition salience" & 
           party_dis %in% c("KSČM", "ČSSD", "Right wing") &
           term %in% c("prop_right_raw", "prop_left_raw")
  ) %>% 
  ggplot(aes(x = year, y = salience)) + 
  geom_line(aes(
    color = party_dis,
    linetype = party_dis)
  ) + 
  labs(title = "opposition salience", y = "") + 
  theme_minimal()+
  scale_x_continuous(
    limits = c(1993,2023),
    breaks = seq(1993, 2023, by =5)
  )





raw_plots_party_left <- self_raw_party_left + opp_raw_party_left +
  plot_annotation(title = "Self and opposition salience for 
                  left-wing parties",
                  theme = theme(plot.title = element_text(hjust = 0.5))
  )



ggsave(plot = raw_plots_party_left, filename = "plots/raw_plots_party_left.jpg", 
       width = 10, height = 7,
       units = "in")







# smooth

self_raw_party_smooth <- df_sal_party_long %>% 
  filter(
    relation == "self salience" & 
      party %in% c("KSČM", "ČSSD") &
      term %in% c("prop_left_raw")
  ) %>%
  ggplot(aes(x = year, y = salience)) + 
  geom_smooth(aes(
    color = party), 
    method = "loess",
    span = 0.6
  ) +
  labs(title = "self salience", y = "salience") + 
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(
    limits = c(1993,2023),
    breaks = seq(1993, 2023, by =5)
  )


opp_raw_party_smooth <- df_sal_party_long %>% 
  filter(relation == "opposition salience" & 
           party %in% c("KSČM", "ČSSD") &
           term %in% c("prop_right_raw")
  ) %>% 
  ggplot(aes(x = year, y = salience)) + 
  geom_smooth(aes(
    color = party),
    method = "loess",
    span = 0.6
  ) + 
  labs(title = "opposition salience", y = "") + 
  theme_minimal()+
  scale_x_continuous(
    limits = c(1993,2023),
    breaks = seq(1993, 2023, by =5)
  )





raw_plots_party_smooth <- self_raw_party_smooth + opp_raw_party_smooth +
  plot_annotation(title = "Self and opposition salience for 
                  left-wing parties",
                  theme = theme(plot.title = element_text(hjust = 0.5))
  )



## overall salience 


average_sal <- df_sal_party_long %>%
  group_by(party_dis, relation) %>%
  reframe(
    average = mean(salience, na.rm = TRUE))


bar <- average_sal %>%
  filter(party_dis %in% c("KSČM", "ČSSD", "Right wing")) %>%
  ggplot(aes(x = party_dis, y = average, fill = relation)) +
  geom_bar(stat="identity",
          position=position_dodge(),
           width = 0.75) +
  theme_minimal() +
  labs(title = "overall salience")+
  theme(plot.title = element_text(hjust = 0.5)) 
  



ggsave(plot = bar, filename = "plots/bar_party.jpg", 
       width = 10, height = 7,
       units = "in")


## ICC  modeling
m1.self <- lmer(salience~1+(1|party), data = self_df_left)
performance::icc(m1.self)

m2.self <- lmer(salience~1+(1|party), data = self_df_right)
performance::icc(m2.self)


m3.opp <-  lmer(salience~1+(1|party), data = opp_df_left)
performance::icc(m3.opp)

m4.opp <- lmer(salience~1+(1|party), data = opp_df_right)

performance::icc(m4.opp)

