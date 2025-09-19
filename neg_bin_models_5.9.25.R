library(tidyverse)
library(glmmTMB)
library(ggeffects)
library(data.table)
library(marginaleffects)# optional
library(flexplot) # optional
library(texreg) # optional

#---------------------------------
data<-fread("ideology_data.csv")

data_month <- data %>%
  group_by(wing,month) %>% 
  reframe(
    n = n(),
    ideology_mentions = sum(left_mentions + right_mentions),
    left_mentions = sum(left_mentions),
    right_mentions = sum(right_mentions),
    in_ideology_mentions = sum(in_ideology_mentions),
    out_ideology_mentions = sum(out_ideology_mentions),
    election_period = first(election_period),
    lag_quarterly_inflation = first(lag_quarterly_inflation),
    lag_public_right = first(lag_public_right),
    lag_public_left = first(lag_public_left),
    lag_public_opinion = lag_public_left - lag_public_right,
    government = first(government),
    year = first(year),
    monthly_word_count = sum(word_count),
    wing = first(wing)
  ) %>% 
  filter(wing!= "Other" & wing != "New_Parties")

data_month <- na.omit(data_month)


data_month$right_wing <- ifelse(data_month$wing == "Right_Wing", 1,0)
data_month$right_wing <- as.factor(data_month$right_wing)
data_month$log_monthly_word_count = log(data_month$monthly_word_count)

mod.self <- glmmTMB(
 in_ideology_mentions ~ lag_quarterly_inflation +  lag_public_opinion + 
   election_period +  government +
   right_wing   +
   + offset(log_monthly_word_count),

  family = nbinom2(),
  data = data_month
)
summary(mod.self)


eff <- ggpredict(mod.self,  
  terms = list("right_wing" = seq(0, 1, 1)))

eff %>% plot()




mod.other <- glmmTMB(
 out_ideology_mentions ~ lag_quarterly_inflation +  lag_public_opinion + 
   election_period +  government +
   right_wing   +
   + offset(log_monthly_word_count),

  family = nbinom2(),
  data = data_month
)
summary(mod.other)


eff <- ggpredict(mod.other,  
  terms = list("right_wing" = seq(0, 1, 1)))

eff %>% plot()



install.packages("topmodels", repos="http://R-Forge.R-project.org")
library(topmodels)





mod.other <- glm.nb(
 out_ideology_mentions ~ lag_quarterly_inflation +  lag_public_opinion + 
   election_period +  government +
   right_wing   +
   + offset(log_monthly_word_count),
  data = data_month
)
#Rootogram
rootogram(mod.other,
          main = "Hanging rootogram of mod1.pois")


mod.self <- glm.nb(
 in_ideology_mentions ~ lag_quarterly_inflation +  lag_public_opinion + 
   election_period +  government +
   right_wing   +
   + offset(log_monthly_word_count),
  data = data_month
)
#Rootogram
rootogram(mod.self,
          main = "Hanging rootogram of mod1.pois")



#Predict outcomes
data_month$preds_predictors2 <- predict(mod.self, 
                                newdata = data_month,
                                #Note that we should not use the argument name here; but we want exponentiated outcomes
                                "response")

#Plot two overlayed histograms
data_month %>%
  ggplot() +
  #Observed outcomes
  geom_histogram(aes(x = in_ideology_mentions),
                 #transparent color
                 alpha=0.5)+
  #Predicted outcomes
  geom_histogram(aes(x = preds_predictors2),
                 #transparent red color
                 fill = "red", 
                 alpha =  0.3)+
  #ggtheme
  theme_minimal() +
  ggtitle("Distribution of observed vs. predicted outcomes") +
  ylab("Number of MEPs") +
  xlab("Number of reports (legislative proposals)")
