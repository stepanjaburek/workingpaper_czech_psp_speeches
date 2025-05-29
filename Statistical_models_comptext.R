##############
# Setup
library(tidyverse)
library(lme4)
library(merTools)
library(fixest)
library(car)
library(lmtest)
library(ggeffects)
##############
# Data
#setwd("C:/Users/stepa/Working_Paper_FSV_MFF")
left <- read.csv("left_models.csv")
right <- read.csv("right_models.csv")
####################################
# Wranglind
# Factors and Numerics for modeling
left$wing <- as.factor(left$wing)
left$gov_ideology <- as.factor(left$gov_ideology)
left$party <- as.factor(left$party)
left$topic <- as.factor(left$topic)
left$gov_left <- as.factor(left$gov_left)
left$election_year <- as.factor(left$election_year)
left$label <- as.factor(left$label)

right$wing <- as.factor(right$wing)
right$party <- as.factor(right$party)
right$topic <- as.factor(right$topic)
right$gov_ideology <- as.factor(right$gov_ideology)
right$gov_right <- as.factor(right$gov_right)
right$election_year <- as.factor(right$election_year)
################################################
left$year_centered <- left$year - 2010 # centering the year
right$year_centered <- right$year - 2010

public_left_by_year <- left %>% # lag of public opinion
  dplyr::select(year, public_left) %>%
  distinct() %>%
  arrange(year)
lookup_values <- setNames(public_left_by_year$public_left, public_left_by_year$year)
left <- left %>%
  mutate(lag_public_left = lookup_values[as.character(year - 1)])

public_right_by_year <- right %>%
  dplyr::select(year, public_right) %>%
  distinct() %>%
  arrange(year)
lookup_values <- setNames(public_right_by_year$public_right, public_right_by_year$year)
right <- right %>%
  mutate(lag_public_right = lookup_values[as.character(year - 1)])

################################################
# MODELS LEFT
#############################################x
# basic logistic models for left and right negative sentiment
log_left <- glm(sentiment ~  gov_left + lag_public_left +
                  topic + election_year, # basic logistic, not accounting for clustering at all
                data = left,            # we assume complete independence in the data 
                family="binomial")
summary(log_left)

###################
# two way fixed effects, typický v ekonometrii
fe_left <- feglm(sentiment ~ year_centered + gov_left + election_year + topic + lag_public_left| 
                    party + speaker,    # fixed effects for cluster variabless
                  data = left,
                  family = "binomial")
summary(fe_left)

###################
# GLMMs 
# Multilevel/hierarchical model
glmm_left <- glmer(
  sentiment ~ year_centered + gov_left + lag_public_left + topic +  election_year + 
    (1|speaker) + (1|party), # random effects for cluster variables
  data = left,
  control=glmerControl(optimizer="bobyqa"),
  family="binomial")
summary(glmm_left)

##########################x
# Model comparison
model_comparison_left <- data.frame(
  Model = c("Multilevel model", "Fixed effects model", "Logistic pooling model"),
  AIC = c(AIC(glmm_left), AIC(fe_left),AIC(log_left)),
  BIC = c(BIC(glmm_left), BIC(fe_left),BIC(log_left))  
)
print(model_comparison_left) # nižší čísla, lepší model

################################################
# MODELS RIGHT
###################################
log_right <- glm(sentiment ~  gov_right  + lag_public_right + topic + election_year,
                 data = right, 
                 family="binomial")
summary(log_right)

#########################
fe_right <- feglm(sentiment ~ year_centered  + gov_right + election_year + topic + lag_public_right| 
                    party + speaker, # fixed effects for cluster variables
                  data = right,
                  family = "binomial")
summary(fe_right)

#################################
glmm_right <- glmer(
  sentiment ~ year_centered + gov_right + lag_public_right + topic + election_year  + 
    (1|speaker) +  (1|party), # random effects for cluster variables
  data = right,
  family="binomial")
summary(glmm_right)

glmm_right_slopes <- glmer(
  sentiment ~ year_centered + gov_right + lag_public_right + election_year  + 
    (1|speaker) +  (1+gov_right|party), # random slopes for right wing gov
  data = right,
  family="binomial")
summary(glmm_right_slopes)
#######################################xx
model_comparison_right <- data.frame(
  Model = c("Multilevel model","Multilevel model w/ slopes", "Fixed effects model", "Logistic pooling model"),
  AIC = c(AIC(glmm_right),AIC(glmm_right_slopes), AIC(fe_right),AIC(log_right)), 
  BIC = c(BIC(glmm_right),BIC(glmm_right_slopes),BIC(fe_right),BIC(log_right))
)
print(model_comparison_right)

##########################################################x
# EFFECT SIZES AND SO ON
# Moje sraní jen, zkuste proklikat a pak co děláte vy jinak
#######################################xx
# GLMMs 
# Multilevel/hierarchical/mixed models - all the same thing
glmm_left <- glmer(
  sentiment ~ year_centered + gov_left + public_left + topic +  election_year +# fixed effects
    (1|speaker)+(1|party), # random effects for cluster variables
  data = left,
  control=glmerControl(optimizer="bobyqa"),
  family="binomial")
summary(glmm_left)


ranef(glmm_left)$party # here we can look at the random effects of our cluster variables
ranef(glmm_left)$speaker # ranef tells us the deviation of each group from the average after controlling for the fixed effects
coef(glmm_left)$party # coef give us the actual estimates for each level
left_effects <- REsim(glmm_left, # we can look at the estimates from the model visually with explicit uncertainty
                      n.sims = 1000) # simulate 1000 times
plotREsim(left_effects)

ggeffect(glmm_left, 
         terms = "year_centered") %>% 
  plot
ggeffect(glmm_left,
         terms = c("public_left [all]")) %>% 
  plot()

ggeffect(glmm_left,
         terms = c("gov_left")) %>% 
  plot()

ggeffect(glmm_right, terms = "election_year") %>% 
  plot

# Get  effects coefficients
# Moje stupidní ruční počítání
effects <- fixef(glmm_left)
logoddsratio <- effects * 5 # The coefficient multiplied by a typical change, here we go for 5% increase in public opinion)
logoddsratio # chnage in log odds
marginal_effect <- (exp(logoddsratio)-1)*100 # to odds ratio and to a cumulative percentage change
marginal_effect

##############
# RIGHT mentions
glmm_right <- glmer(
  sentiment ~ year_centered + gov_right + public_right + topic + election_year  + # fixed effects
    (1|speaker) +  (1|party), # random effects for cluster variables
  data = right,
  family="binomial")
summary(glmm_right)

glmm_right_slopes <- glmer(
  sentiment ~ year_centered + gov_right + public_right + election_year  + # fixed effects
    (1|speaker) +  (1+gov_right|party), # random slopes for right wing gov
  data = right,
  family="binomial")
summary(glmm_right_slopes)

effects <- fixef(glmm_right)
logoddsratio <- effects   * 5 # The coefficient multiplied by a typical change, here we go for 5% increase in public opinion)
logoddsratio # chnage in log odds
marginal_effect <- (exp(logoddsratio)-1)*100 # to odds ratio and to a percentage change
marginal_effect

ranef(glmm_right)$party 
ranef(glmm_right)$speaker 
coef(glmm_right)$party 
right_effects <- REsim(glmm_right, # we can look at the estimates from the model visually with explicit uncertainty
                       n.sims = 1000)#Simulate 1000 times
plotREsim(right_effects)

ggeffect(glmm_right,
         terms = c("public_right [all]")) %>% 
  plot()

ggeffect(glmm_right,
         terms = c("gov_right")) %>% 
  plot()



##############################################xxxxxxx
library(stargazer)
library(modelsummary)

modelsummary(list("Pooled Logistic" = log_left, "FE Logistic" = fe_left, "Multilevel Logistic" = glmm_left),
             title = "Negative Mentions of the Left from Individual Level Data",
             stars = TRUE,
             gof_map = c("nobs", "AIC", "BIC", "logLik"),
             coef_map = c("year_centered" = "Year", 
                          "gov_left1" = "Left Government",
                          "election_year1" = "Election Year",
                          "topicforeign_policy_issues" = "Foreign Policy",
                          "topichistorical_issues" = "Historical Issues",
                          "topicinstitutional_issues" = "Institutional Issues",
                          "topicsocial/cultural_issues" = "Social/Cultural Issues",
                          "lag_public_left" = "Public Left (lagged)"),
             fmt = 2,
             output = "left_sentiment_glmms_2.tex")
c
modelsummary(list("Pooled Logistic" = log_right, "FE Logistic " = fe_right, "Multilevel Logistic" = glmm_right),
             title = "Negative Mentions of the right from Individual Level Data",
             stars = TRUE,
             gof_map = c("nobs", "AIC", "BIC", "logLik"),
             coef_map = c("year_centered" = "Year", 
                          "gov_right1" = "right Government",
                          "election_year1" = "Election Year",
                          "topicforeign_policy_issues" = "Foreign Policy",
                          "topichistorical_issues" = "Historical Issues",
                          "topicinstitutional_issues" = "Institutional Issues",
                          "topicsocial/cultural_issues" = "Social/Cultural Issues",
                          "lag_public_right" = "Public right (lagged)"),
             fmt = 2,
             output = "right_sentiment_glmms_2.tex")

stargazer(log_left, fe_left, glmm_left,
          #column.labels = c("Left Mentions"),
          #column.separate = c(4, 4),
          model.numbers = T,
          type = "text", 
          title = "Negative Mentions of the Left from Individual Level Data",
          font.size = "large",
          no.space = TRUE,
          column.sep.width = "4pt",
          omit.stat = c("f", "ser", "adj.rsq"),
          single.row = F,
          digits = 2,
          out = "left_sentiment_glmms_2.tex")

stargazer(log_right, glmm_right,glmm_right_slopes, 
          #column.labels = c(Right Mentions"),
          #column.separate = c(2, 2),
          model.numbers = T,
          type = "text", 
          title = "Negative Mentions of the Right from Individual Level Data",
          font.size = "large",
          no.space = TRUE,
          column.sep.width = "4pt",
          omit.stat = c("f", "ser", "adj.rsq"),
          single.row = F,
          digits = 2,
          out = "right_sentiment_glmms.tex")

