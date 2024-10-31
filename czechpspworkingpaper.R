library(tidyverse)
library(quanteda)
library(sentimentr)
library(ggthemes)
library(broom)
library(devtools)
library(ThemePark)
library(lme4)
library(flexplot)
library(ranger)
head(themepark_themes)


install.packages("remotes")
remotes::install_github("MatthewBJane/ThemePark")

dir="C:/Users/stepa/Downloads"
setwd("C:/Users/stepa/Downloads")
getwd()

df<-read.csv("CzechParlPlenaryUpdated_13.07.2024.csv")

#Definovat vzorec
pattern <-"\\blevice\\b|\\blevicov.\\b|\\blevičák\\b"

# Define pattern for Czech political left references
pattern <- paste0(
  "\\blev(ice|ý|á|é|ému|ých|icových|icové)\\b|",  # Forms of "levice" (left)
  "\\bsociáln[ěíé]\\s*demokrat(é|ická|ickou)\\b|",  # Social Democrats
  "\\bčssd\\b|",  # Abbreviation for Czech Social Democratic Party
  "\\bksčm\\b|",  # Communist Party of Bohemia and Moravia
  "\\bkomunist(é|ická|ickou|ů|i|ům)\\b|", # Forms of "komunistická" (communist)
  "\\bsocialist(é|ická|ickou|ů|i|ům)\\b|",
  "\\bkomunismus\\b|",
  "\\bsocialismus\\b|",
  "\\bprogresiv(ní|ismus|isté)\\b")  



# Progressive
  #"\\bzelení\\b|\\bstrana\\s+zelen(ých|é)\\b|",  # Green Party
  #"\\bliberáln[ěíé]\\s*(lev(ice|ý|á|é))\\b"  # Liberal left


# Count mentions (this may take a while)
dff$left_mentions <- str_count(dff$text, regex(pattern))

sum(df$left_mentions)

#Počet mentions (chvilku to trvá)
df$mentions <- str_count(df$text, regex(pattern))
df$month <- my(df$month)
df$month <- format(df$month, "%Y-%m")


dff<-dff %>% select(-8)

kwic_results <- kwic_analysis_updated(dff, pattern)

#Shluk za rok
by_year <- aggregate(df$mentions, by = list(year = df$year), FUN = sum)


speech_by_month <- aggregate(id ~ month, data = df, FUN = length)
by_month<-cbind(by_month,speech_by_month)
by_month<-by_month %>% select(-3)
by_month<- mutate(by_month,mentionspps=x/id)

speech_by_year <- aggregate(id ~ year, data = df, FUN = length)
by_year<-cbind(by_year,speech_by_year)
by_year<-by_year %>% select(-3)
by_year<- mutate(by_year,mentionspps=x/id)

# Create the time series plot
ggplot(by_year, aes(x = year, y = mentionspps, group=1)) +
  geom_line(color = "#2C3E50", size = 1) +
  geom_point(color = "#2C3E50", size = 1) +
  theme_minimal() +
  labs(
    title = "Evolution of Mentions Over Time",
    x = "Month",
    y = "Number of Mentions"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(by_year, aes(x = year, y = mentionspps, group = 1)) +
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
  theme(axis.text.x = element_text( vjust = 0.5, hjust = 1))+
  scale_x_discrete()
summary(by_year$x)
sum(df$left_mentions)
sly#scale_y_continuous(breaks = seq(0, 200, by = 1))

# see suggested colours in the palette
barbie_theme_colors
#>        text       panel      border     lighter       light      medium 
#> "#a62675ff" "#fdf6faff" "#d74ea2ff" "#f5d1e6ff" "#eeb4d7ff" "#d74ea2ff" 
#>        dark 
#> "#bf2986ff"

#Grafík
ggplot(hoch_by_year, aes(x = year, y = x, group = 1)) +
  geom_line(size=0.7, col= "chartreuse4") +
  geom_point( size = 2, col= "chartreuse4") +
  theme_solarized()+
  labs(x = "Rok", y = "Zmínky hochštaplerů", title = "Zmínky hochštaplerů v plenárních projevech PSP ČR; 1993-2023")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


library(ThemePark)
dff<-df %>% filter(year>2017)
dff<-dff %>% filter(year<2022)
dff<-dff %>% filter(month!="12/2021")
dff<-dff %>% filter(month!="11/2021")
dff<-dff %>% filter(month!="10/2021")

library(lubridate)

dff$month <- my(dff$month)
dff$month <- format(dff$month, "%Y-%m")
dff<-dff %>% select(-9)

ODS<-dff%>%filter(party=="ODS")
TOP09<-dff%>%filter(party=="TOP 09")
KDU<-dff%>%filter(party=="KDU-ČSL")
PIR<-dff%>%filter(party=="Piráti")
STAN<-dff%>%filter(party=="STAN")
ANO<-dff%>%filter(party=="ANO")
SPD<-dff%>%filter(party=="SPD")
KSČM<-dff%>%filter(party=="KSČM")
ČSSD<-dff%>%filter(party=="ČSSD")


ODS<-df%>%filter(party=="ODS")
KDU<-df%>%filter(party=="KDU-ČSL")
TOP09<-df%>%filter(party=="TOP 09")
ANO<-df%>%filter(party=="ANO")
KSČM<-df%>%filter(party=="KSČM")
ČSSD<-df%>%filter(party=="ČSSD")
ods_by_year <- aggregate(left_mentions ~ year, data = ODS, FUN = sum)
kdu_by_year <- aggregate(left_mentions ~ year, data = KDU, FUN = sum)
top_by_year <- aggregate(left_mentions ~ year, data = TOP09, FUN = sum)
ano_by_year <- aggregate(left_mentions ~ year, data = ANO, FUN = sum)
ksčm_by_year <- aggregate(left_mentions ~ year, data = KSČM, FUN = sum)
čssd_by_year <- aggregate(left_mentions ~ year, data = ČSSD, FUN = sum)

ef<-rbind(ods_by_year,kdu_by_year,top_by_year, ano_by_year, ksčm_by_year, čssd_by_year)
ef <- ef %>%
  mutate(id = row_number())
27*4
ef$party <- ifelse(ef$id %in% 1:8, "ODS",
                   ifelse(ef$id %in% 9:16, "KDU",
                          ifelse(ef$id %in% 17:24, "TOP 09",
                                 ifelse(ef$id %in% 25:32, "ANO",
                                        ifelse(ef$id %in% 33:40, "KSČM",
                                               ifelse(ef$id %in% 41:48, "ČSSD",""))))))

ef$gov <- ifelse(ef$id %in% 1:8, "0",
                   ifelse(ef$id %in% 9:12, "1",
                          ifelse(ef$id %in% 13:24, "0",
                                 ifelse(ef$id %in% 25:32, "1",
                                        ifelse(ef$id %in% 33:40, "0",
                                               ifelse(ef$id %in% 41:48, "1",""))))))

ef$galtan <- ifelse(ef$id %in% 1:8, 7.03,
                   ifelse(ef$id %in% 9:16, 7.78,
                          ifelse(ef$id %in% 17:24, 4.85,
                                 ifelse(ef$id %in% 25:32, 5.73,
                                        ifelse(ef$id %in% 33:40, 8.07,
                                               ifelse(ef$id %in% 41:48, 4.92,""))))))

ef$galtan <-as.numeric(ef$galtan)

# Center continuous predictors

ef$galtan_c <- scale(ef$galtan, center = TRUE, scale = FALSE)
ef$time <- as.numeric(factor(ef$year))
ef$time_c <- scale(ef$time, center = TRUE, scale = FALSE)

model <- glmer.nb(left_mentions ~ time_c + gov + galtan_c + party +
                (1 | party) + (1 | year),
              data = ef)
summary(model)

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

colnames(EF) <-c("month", "left_mentions", "bull","speeches")
EF <- EF %>%
  mutate(id = row_number())
EF<-EF %>% select(-3)
EF$party <- ifelse(EF$id %in% 1:44, "ANO",
                   ifelse(EF$id %in% 45:88, "ODS",
                          ifelse(EF$id %in% 89:132, "TOP 09",
                                 ifelse(EF$id %in% 133:176, "STAN",
                                        ifelse(EF$id %in% 177:220, "Piráti",
                                               ifelse(EF$id %in% 221:264, "KDU-ČSL",
                                                      ifelse(EF$id %in% 265:308, "SPD", 
                                                             ifelse(EF$id %in% 309:352, "KSČM",
                                                                    ifelse(EF$id %in% 353:396, "ČSSD","")))))))))

EF$government <- ifelse(EF$id %in% 1:44, "1",
                        ifelse(EF$id %in% 45:88, "0",
                               ifelse(EF$id %in% 89:132, "0",
                                      ifelse(EF$id %in% 133:176, "0",
                                             ifelse(EF$id %in% 177:220, "0",
                                                    ifelse(EF$id %in% 221:264, "0",
                                                           ifelse(EF$id %in% 265:308, "0", 
                                                                  ifelse(EF$id %in% 309:352, "0",
                                                                         ifelse(EF$id %in% 353:396, "1", "")))))))))

EF$leftright <- ifelse(EF$id %in% 1:44, 4.692307472229004,
                          ifelse(EF$id %in% 45:88, 7.777777671813965,
                                 ifelse(EF$id %in% 89:132, 7.407407283782959,
                                        ifelse(EF$id %in% 133:176, 6.269230842590332,
                                               ifelse(EF$id %in% 177:220, 4.28000020980835,
                                                      ifelse(EF$id %in% 221:264, 5.888888835906982,
                                                             ifelse(EF$id %in% 265:308, 8.84615421295166, 
                                                                    ifelse(EF$id %in% 309:352, 1.038461565971375,
                                                                           ifelse(EF$id %in% 353:396, 2.592592477798462,"")))))))))
EF$galtan <- ifelse(EF$id %in% 1:44, 5.730769157409668,
                       ifelse(EF$id %in% 45:88, 7.037036895751953,
                              ifelse(EF$id %in% 89:132, 4.851851940155029,
                                     ifelse(EF$id %in% 133:176, 4.07692289352417,
                                            ifelse(EF$id %in% 177:220, 1,
                                                   ifelse(EF$id %in% 221:264, 7.777777671813965,
                                                          ifelse(EF$id %in% 265:308, 9.370369911193848, 
                                                                 ifelse(EF$id %in% 309:352, 8.074073791503906,
                                                                        ifelse(EF$id %in% 353:396, 4.92307710647583,"")))))))))


EF$eu_pos <- ifelse(EF$id %in% 1:26, 4.481481552124023,
                       ifelse(EF$id %in% 27:52, 3.777777671813965,
                              ifelse(EF$id %in% 53:78, 6.666666507720947,
                                     ifelse(EF$id %in% 79:104, 6.518518447875977,
                                            ifelse(EF$id %in% 105:130, 6.115384578704834,
                                                   ifelse(EF$id %in% 131:156, 6.222222328186035,
                                                          ifelse(EF$id %in% 157:182, 1.481481432914734, 
                                                                 ifelse(EF$id %in% 309:352, "KSČM",
                                                                        ifelse(EF$id %in% 353:396, "ČSSD","")))))))))


EF<-mutate(EF, left_ment_pps = left_mentions/speeches)
EF<-EF %>% 
  mutate(left_mentions = ifelse(left_mentions == 0, 1, left_mentions))
ifelse(EF$left_mentions == 0,1,"")




ggplot(EF, aes(x = month, y = mentions, group = 1)) +
  geom_line() +
  facet_wrap(~ party, scales = "fixed") +  # Use 'fixed' to have the same y-axis limits across facets
  labs(title = "EU mentions by party",
       x = "Month/Year",
       y = "EU mentions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotate x-axis labels 90 degrees
        legend.position = "bottom") 
scale_y_continuous(limits = c(0, 400))




EF$party <- as.character(EF$party)
EF$leftright <-as.numeric(EF$leftright)
EF$galtan <-as.numeric(EF$galtan)

EF <- EF %>%
  group_by(party) %>%
  mutate(time = 1:n()) %>%
  ungroup()

# Center continuous predictors
EF$leftright_c <- scale(EF$leftright, center = TRUE, scale = FALSE)
EF$galtan_c <- scale(EF$galtan, center = TRUE, scale = FALSE)
EF$time_c <- scale(EF$time, center = TRUE, scale = FALSE)


control_opts <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

model <- lmer(left_mentions ~  time_c+government+party  +(1| party),   data = EF)
model<- lmer(mentions ~ time_c  + (1 +time| party) + (1 | month), data=big4b)
summary(model)

visualize(model, plot="model")
summary(model)

install.packages("emmeans")
library(emmeans)

party_means <- emmeans(model, ~ party)
print(party_means)

# Plot predicted values over time for each party
new_data <- expand.grid(
  time = seq(min(df$time), max(df$time), length.out = 100),
  party = unique(df$party),
  government = 0,  # Set to average or most common value
  leftright_c = 0,  # Set to average (since it's centered)
  galtan_c = 0  # Set to average (since it's centered)
)

new_data$month <- levels(factor(df$month))[new_data$time]
new_data$predicted <- predict(model, newdata = new_data, re.form = NA)

ggplot(new_data, aes(x = month, y = predicted, color = party, group = party)) +
  geom_line() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Predicted Left-Wing Mentions Over Time by Party",
       x = "Time", y = "Predicted Number of Mentions") +
  scale_x_discrete(breaks = new_data$month[seq(1, nrow(new_data), length.out = 10)])

# Effect of government status
government_effect <- emmeans(model, ~ time_c)
print(government_effect)

# Effect of left-right position
leftright_effect <- emtrends(model, ~ leftright_c, var = "leftright_c")
print(leftright_effect)

# Check for autocorrelation in residuals
acf(resid(model))

