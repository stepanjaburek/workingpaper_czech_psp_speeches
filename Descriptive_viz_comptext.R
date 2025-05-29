library(tidyverse)
# install.packages("devtools")
devtools::install_github("dustinfife/flexplot")
library(flexplot)

left <- read.csv("left_models.csv")
right <- read.csv("right_models.csv")
data<-read.csv("dataset_mentions_controls.csv")

# -------------------------------------------------
# Self salience

left_self_sal<-data %>% 
  filter(wing == "Left_Wing") %>% 
  group_by(year, party ) %>% 
  reframe(self_sum = sum(left_mentions>0),
          n = length(id),
          self_salience = (self_sum/n)*100) 
unique(left_self_sal$party)

right_self_sal<-data %>% 
  filter(wing == "Right_Wing") %>% 
  group_by(year, party) %>% 
  reframe(self_sum = sum(right_mentions>0),
          n = length(id),
          self_salience = (self_sum/n)*100)
unique(right_self_sal$party)

self_salience <- rbind(left_self_sal,right_self_sal )
self_salience <- self_salience %>% 
  group_by(party) %>%
  filter(n() >= 7) %>% # min 7 years in parliament
  ungroup() %>% 
  filter(n > 100) # min 100 overall speeches in a given year

unique(self_salience$party)

coplot(self_salience ~ year|party, type="l", data=self_salience) # panel plot
#library(gplots)
#plotmeans(self_salience ~ party, main="Heterogeineity across parties", data=selfsalience) # means
flexplot(self_salience ~ party, data=self_salience) # all party/year salience
flexplot(self_salience ~ year+party, data=self_salience, method = "lm")


# -------------------------------------------------
# Opposition salience

left_opp_sal<-data %>% 
  filter(wing == "Left_Wing") %>% 
  group_by(year, party ) %>% 
  reframe(opp_sum = sum(right_mentions>0),
          n = length(id),
          opp_salience = (opp_sum/n)*100) 
unique(left_opp_sal$party)

right_opp_sal<-data %>% 
  filter(wing == "Right_Wing") %>% 
  group_by(year, party) %>% 
  reframe(opp_sum = sum(left_mentions>0),
          n = length(id),
          opp_salience = (opp_sum/n)*100)
unique(right_opp_sal$party)

opp_salience <- rbind(left_opp_sal,right_opp_sal )
opp_salience <- opp_salience %>% 
  group_by(party) %>%
  filter(n() >= 7) %>%
  ungroup() %>% 
  filter(n > 100)

unique(opp_salience$party)

coplot(opp_salience ~ year|party, type="l", data=opp_salience)
#library(gplots)
#plotmeans(oppsalience ~ party, main="Heterogeineity across parties", data=oppsalience)
flexplot(opp_salience ~ party, data=opp_salience)
flexplot(opp_salience ~ year + party, data=opp_salience, method = "lm")


# ----------------------------------------------
# Self sentiment

left_self_sent<-left %>% 
  filter(wing == "Left_Wing") %>% 
  group_by( party ) %>% 
  reframe(self_sum = sum(sentiment==1),
          n = length(id),
          self_sentiment = (self_sum/n)*100) 
unique(left_self_sent$party)

right_self_sent<-right %>% 
  filter(wing == "Right_Wing") %>% 
  group_by( party) %>% 
  reframe(self_sum = sum(sentiment==1),
          n = length(id),
          self_sentiment = (self_sum/n)*100)
unique(right_self_sent$party)

self_sentiment <- rbind(left_self_sent,right_self_sent )
self_sentiment <- self_sentiment %>% 
  filter(party %in% c("ČSSD", "ODS", "KDU-ČSL", "KSČM"))

unique(self_sentiment$party)
flexplot(self_sentiment ~ party, data=self_sentiment) # all party/year salience

# ----------------------------------------------
# opp sentiment

left_opp_sent<-left %>% 
  filter(wing == "Right_Wing") %>% 
  group_by( party ) %>% 
  reframe(self_sum = sum(sentiment==1),
          n = length(id),
          opp_sentiment = (self_sum/n)*100) 
unique(left_opp_sent$party)

right_opp_sent<-right %>% 
  filter(wing == "Left_Wing") %>% 
  group_by( party) %>% 
  reframe(self_sum = sum(sentiment==1),
          n = length(id),
          opp_sentiment = (self_sum/n)*100)
unique(right_opp_sent$party)

opp_sentiment <- rbind(left_opp_sent,right_opp_sent )
opp_sentiment <- opp_sentiment %>% 
  filter(party %in% c("ČSSD", "ODS", "KDU-ČSL", "KSČM"))

unique(self_sentiment$party)

flexplot(opp_sentiment ~ party, data=opp_sentiment) # all party/year salience

