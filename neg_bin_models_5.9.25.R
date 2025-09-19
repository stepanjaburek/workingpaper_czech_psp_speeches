library(tidyverse)
library(glmmTMB)
library(ggeffects)
library(data.table)
library(marginaleffects)# optional
library(flexplot) # optional
library(texreg) # optional
library(MASS)

#---------------------------------
data<-fread("~/Documents/czech_psp_data/ideology_data.csv")


# group by party/month

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



data_month$right_wing <- ifelse(data_month$wing == "Right_Wing", 1,0) # binarize wing for visuals
data_month$right_wing <- as.factor(data_month$right_wing) # as factor
data_month$log_monthly_word_count = log(data_month$monthly_word_count) # prepare the offset variable

data_month_clean <- na.omit(data_month) # remove rows with missing data - we have missing Public Opinion before 2005 and missing Inflation before 1995





#---------------------------------------
# Self salience models
#.------------------

# full model, missing data

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


# clean model without variables with missing values
mod.self.2 <- glmmTMB(
 in_ideology_mentions ~  election_period +  government +
   right_wing   +
   + offset(log_monthly_word_count),

  family = nbinom2(),
  data = data_month
)
summary(mod.self.2)


eff <- ggpredict(mod.self.2,  
  terms = list("right_wing" = seq(0, 1, 1)))

eff %>% plot()


# model without public opinion but with inflation

mod.self.3 <- glmmTMB(
 in_ideology_mentions ~ lag_quarterly_inflation + election_period +  government +
   right_wing   +
   + offset(log_monthly_word_count),

  family = nbinom2(),
  data = data_month
)
summary(mod.self.3)


eff <- ggpredict(mod.self.3,  
  terms = list("right_wing" = seq(0, 1, 1))
)

eff %>% 
  plot()


#---------------------------------------
# Out salience models
#.------------------

# full model, missing data

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
  terms = list("right_wing" = seq(0, 1, 1))
)

eff %>% 
  plot()




# clean model without variables with missing values

mod.other.2 <- glmmTMB(
 out_ideology_mentions ~ election_period +  government +
   right_wing   +
   + offset(log_monthly_word_count),

  family = nbinom2(),
  data = data_month
)
summary(mod.other.2)


eff <- ggpredict(mod.other.2,  
  terms = list("right_wing" = seq(0, 1, 1))
)

eff %>% 
  plot()



# model without public opinion but with inflation

mod.other.3 <- glmmTMB(
 out_ideology_mentions ~ lag_quarterly_inflation + election_period +  government +
   right_wing   +
   + offset(log_monthly_word_count),

  family = nbinom2(),
  data = data_month
)
summary(mod.other.3)


eff <- ggpredict(mod.other.3,  
  terms = list("right_wing" = seq(0, 1, 1))
)

eff %>% 
  plot()
