library(tidyverse)
library(quanteda)
library(flexplot)
library(ThemePark)

setwd("C:/Users/stepa/Downloads")
data<-read.csv("paper_data.csv")

################################################
# VOCABULARIES
pattern_left <- paste0("\\blev(ic[eií]|icových|icov[áýé]|icovým|icovými|ičák|ičáck[ýáé])\\b|", 
                       "\\bkomunist([aéiů]|ka|ům|ická|ickou|ický)\\b|",
                       "\\bsocialist([aéiů]|ka|ům|ická|ickou|ický)\\b|",
                       "\\bkomunismus\\b|",
                       "\\bsocialismus\\b")

pattern_right <- paste0("\\bprav(ic[eií]|icových|icov[áýé]|icovým|icovými|ičák|ičáck[ýáé])\\b|", 
                        "\\bliber(áln[ěí]|ál|álka|álním[ui]|álním|álních|álního|ismus|álně)\\b|",
                        "\\bkonzervat(ivní|ivec|ivním[ui]|ivního|ivních|ivismus|ivizmus|ismus|izmus)\\b")

#############################################################X
# MENTIONS EXTRACTION
data$left_mentions <- str_count(data$text, regex(pattern_left))
data$right_mentions <- str_count(data$text, regex(pattern_right))
data2$right_mentions <- str_count(data2$text, regex(pattern_right))

by_year_l <- aggregate(data$left_mentions, by = list(year = data$year), FUN = sum)
by_year_r <- aggregate(data$right_mentions, by = list(year = data$year), FUN = sum)
by_year_r2 <- aggregate(data2$right_mentions, by = list(year = data2$year), FUN = sum)
data2<-data %>% filter(chair==1)
4000/400000
left_data<-data %>% filter(left_mentions!=0)
by_year_r2<-by_year_r %>% filter(chair==1)
2729/470007*100
470007-247754

4305/470007
sum(by_year_r$x)
ggplot(by_year_l, aes(x = year, y = x, group = 1)) +
  geom_smooth(method = 'loess', 
              color  = barbie_theme_colors["medium"],
              fill   = barbie_theme_colors["light"])+
  geom_line(linewidth=0.7,color= barbie_theme_colors["dark"]) +
  geom_point(size=1,color = barbie_theme_colors["dark"]) +
  theme_barbie()+
  labs(title = "Zmínky levice v plenárních projevech poslanců",
       subtitle = "Poslanecká sněmovna  ČR  1993-2023",
       caption  = "Super important research at IPS",
       x = "Rok",
       y = "Zmínky levice") +
  theme(axis.text.x = element_text( vjust = 0.5, hjust = 1))
  scale_x_discrete()


by_year<-rbind(by_year_r,by_year_l)

by_year<-by_year %>% mutate(num=row_number())

by_year$leftright<-ifelse(by_year$num %in% 1:343, "Right",
                          ifelse (by_year$num %in% 344:686, "Left", ""))

ggplot(by_year, aes(x = month, y = x, color = leftright, group = leftright)) +
  geom_line(size = 1) +
  geom_point(size = 1) +
  theme_barbie()+
  geom_smooth(method="loess")+
  labs(
    title = "Total Mentions Over Time",
    x = "Year",
    y = "Number of Mentions",
    color = "Group"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) 

#####################################################
   # STRANY
df<-data %>% filter(chamber==9)

ODS<-df%>%filter(party=="ODS")
TOP09<-df%>%filter(party=="TOP 09")
KDU<-df%>%filter(party=="KDU-ČSL")
PIR<-df%>%filter(party=="Piráti")
STAN<-df%>%filter(party=="STAN")
ANO<-df%>%filter(party=="ANO")
SPD<-df%>%filter(party=="SPD")
KSČM<-df%>%filter(party=="KSČM")
ČSSD<-df%>%filter(party=="ČSSD")

ano_eu_by_month <- aggregate(left_mentions ~ month, data = ANO, FUN = sum)
ods_eu_by_month <- aggregate(left_mentions ~ month, data = ODS, FUN = sum)
spd_eu_by_month <- aggregate(left_mentions ~ month, data = SPD, FUN = sum)
top_eu_by_month <- aggregate(left_mentions ~ month, data = TOP09, FUN = sum)
stan_eu_by_month <- aggregate(left_mentions ~ month, data = STAN, FUN = sum)
pir_eu_by_month <- aggregate(left_mentions ~ month, data = PIR, FUN = sum)
kdu_eu_by_month <- aggregate(left_mentions ~ month, data = KDU, FUN = sum)
ksčm_eu_by_month <- aggregate(left_mentions ~ month, data = KSČM, FUN = sum)
čssd_eu_by_month <- aggregate(left_mentions ~ month, data = ČSSD, FUN = sum)

kdu_words_by_month <- aggregate(word_count ~ month, data = KDU, FUN = sum)
ano_words_by_month <- aggregate(word_count ~ month, data = ANO, FUN = sum)
ods_words_by_month <- aggregate(word_count ~ month, data = ODS, FUN = sum)
spd_words_by_month <- aggregate(word_count ~ month, data = SPD, FUN = sum)
top_words_by_month <- aggregate(word_count ~ month, data = TOP09, FUN = sum)
stan_words_by_month <- aggregate(word_count ~ month, data = STAN, FUN = sum)
pir_words_by_month <- aggregate(word_count ~ month, data = PIR, FUN = sum)

ano_speech_by_month <- aggregate(id ~ month, data = ANO, FUN = length)
ods_speech_by_month <- aggregate(id ~ month, data = ODS, FUN = length)
spd_speech_by_month <- aggregate(id ~ month, data = SPD, FUN = length)
top_speech_by_month <- aggregate(id ~ month, data = TOP09, FUN = length)
stan_speech_by_month <- aggregate(id ~ month, data = STAN, FUN = length)
pir_speech_by_month <- aggregate(id ~ month, data = PIR, FUN = length)
kdu_speech_by_month <- aggregate(id ~ month, data = KDU, FUN = length)
ksčm_speech_by_month <- aggregate(id ~ month, data = KSČM, FUN = length)
čssd_speech_by_month <- aggregate(id ~ month, data = ČSSD, FUN = length)
EF<-rbind(ano_eu_by_month,ods_eu_by_month,top_eu_by_month,stan_eu_by_month,pir_eu_by_month,kdu_eu_by_month,spd_eu_by_month,ksčm_eu_by_month, čssd_eu_by_month)
EFC<-rbind(ano_words_by_month,ods_words_by_month,top_words_by_month,stan_words_by_month,pir_words_by_month,kdu_words_by_month,spd_words_by_month)
EFK<-rbind(ano_speech_by_month,ods_speech_by_month,top_speech_by_month,stan_speech_by_month,pir_speech_by_month,kdu_speech_by_month,spd_speech_by_month,ksčm_speech_by_month, čssd_speech_by_month)
EF<-cbind(EF,EFK)