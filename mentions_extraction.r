library(tidyverse)
library(quanteda)
library(ggpubr)
#library(flexplot) # není nutné 
#library(ThemePark) # na barbie, oppenheimer grafíky

data<-read.csv("paper_data.csv")

data %>% 
  filter(year == 1998 & party == "ANO") %>%
  View()

data<-data %>% filter(chair==0) #bez předsedajícího

################################################
# VOCABULARIES

pattern_left <- paste0("\\blev(ic[eií]|icových|icov[áýé]|icovým|icovými|ičák|ičáck[ýáé])\\b|",
                       "\\w*levic\\w*")

pattern_right <- paste0("\\bprav(ic[eií]|icových|icov[áýé]|icovým|icovými|ičák|ičáck[ýáé])\\b|",
                        "\\w*pravic\\w*")
######################################

pattern_kom <- paste0("\\bkomunist([aéiů]|ka|ům|ická|ickou|ický|ické|ických|ickými)\\b|",
                      "\\bkomunismu(u|us)\\b|",
                      "\\w*komunis\\w*")

pattern_soc <- paste0("\\bsocialist([aéiů]|ka|ům|ická|ickou|ický|ické|ických|ickými)\\b|",
                      "\\bsocialism(u|us)\\b|",
                      "\\w*socialis\\w*")

pattern_lib <- paste0("\\bliber(áln[ěí]|ál|álka|álním[ui]|álním|álních|álního|ismus|álně)\\b")

pattern_con <- paste0("\\bkonzervat(ivní|ivec|ivním[ui]|ivního|ivních|ivismus|ivizmus|ismus|izmus)\\b")

#############################################################X
# MENTIONS EXTRACTION

data_new <- data

data_new$left_mentions <- str_count(data$text, regex(pattern_left))


data_new$right_mentions <- str_count(data$text, regex(pattern_right))


data_new$kom_mentions <- str_count(data$text, regex(pattern_kom))

data_new$soc_mentions <- str_count(data$text, regex(pattern_soc))

data_new$lib_mentions <- str_count(data$text, regex(pattern_lib))

data_new$con_mentions <- str_count(data$text, regex(pattern_con))


df_final <- data_new %>% 
  mutate(pure_left = case_when(left_mentions >= 1 & right_mentions ==0 ~ 1, #pure left mentions
                          TRUE ~ 0), 
         pure_right = case_when(right_mentions >= 1 & left_mentions ==0 ~ 1, # pure right mentions
                           TRUE ~ 0), 
         both = case_when(right_mentions >= 1 & left_mentions >=1 ~ 1,  # mentions of both left and right
                          TRUE ~ 0),
         kom = case_when(kom_mentions >= 1 ~ 1,   # raw kom mentions
                         TRUE ~ 0),
         soc = case_when(soc_mentions >= 1 ~ 1,  # raw soc mentions
                         TRUE ~ 0),
         con = case_when(con_mentions >= 1 ~ 1,  #raw con mentions
                         TRUE ~ 0),
         lib = case_when(lib_mentions >= 1 ~ 1,  # raw lib mentions
                         TRUE ~ 0)
            )


file_out <- "dataset_mentions.csv"

write.csv(df_final, file_out)












## comparing proportions of pure left, pure right and both mentions mentions


## aggregate yearly proportions of pure left, pure right and both mentions

prop_agg <- df_final %>% 
  group_by(year) %>% 
  summarise(prop_agg_right = sum(pure_right)/( sum(pure_right) + sum(pure_left) + sum (both)),
            prop_agg_left =  sum(pure_left)/( sum(pure_right) + sum(pure_left) + sum (both)),
            prop_agg_both = sum(both)/(sum(pure_right) + sum(pure_left) + sum(both)),
            .groups = "drop")

#party level proportions

prop_party <- df_final %>% 
  group_by(year, party) %>% 
  summarise(prop_party_right = sum(pure_right)/( sum(pure_right) + sum(pure_left) + sum (both)),
            prop_party_left = sum(pure_left)/( sum(pure_right) + sum(pure_left) + sum (both)),
            prop_party_both = sum(both)/(sum(pure_right) + sum(pure_left) + sum(both)),
            .groups = "drop")

#comparing party level proportions to the aggregate yearly proportions

comparison_df <- prop_party %>% 
  left_join(prop_agg, by = "year")


### Graphing

comparison_df %>% 
  filter(party %in% c("ODS", "ČSSD", "KDU-ČSL", "KSČM")) %>% 
  ggplot(aes(x = year, y = prop_party_right - prop_agg_right, color = party)) +
  geom_smooth(method = "loess", span = 0.66, se =F) +
  scale_x_continuous(breaks = seq(1993, 2023, by =5)) + 
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.7, color = "black") +
  theme_pubr() + 
  labs(x = "year", y = "de-meaned salience", title = "Right mentions") + 
  scale_color_manual(values = c("ODS" = "blue", "ČSSD" = "orange", "KDU-ČSL" = "yellow", "KSČM" = "red"))





