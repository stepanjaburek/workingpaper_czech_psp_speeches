library(ordinal)
library(ggeffects)
##############
library(tidyverse)
library(glmmTMB)
library(merTools)
library(fixest)
library(brms)
library(lmtest)
library(marginaleffects)
library(data.table)
library(flexplot)
library(cmdstanr)

#--------------------------------------------
# Data
self_sentiment <- read.csv("self_sentiment.csv")
out_sentiment<- read.csv("out_sentiment.csv")


#-------------------------------------------
self_sentiment$sentiment_ord <- as.factor(self_sentiment$sentiment_ord)
out_sentiment$sentiment_ord <- as.factor(out_sentiment$sentiment_ord)



self_sentiment <- na.omit(self_sentiment)
# Multilevel/hierarchical model
mod.ord.out <- clmm(
  sentiment_ord ~  lag_public_opinion +  lag_quarterly_inflation.y +
    government     + election_period +
    right_wing +
    (1|speaker), # random effects for cluster variables
   link = "logit",
  data = out_sentiment)

summary(mod.ord.out)


# Calculate predicted probabilities for right_wing, holding other variables at their mean/median
pred_out <- ggpredict(mod.ord.out, terms = "right_wing")

# Plot
plot(pred_out) +
  labs(
    title = "Predicted Probability of Out-Ideology Sentiment by Wing Status (1=NEG, 2=NEU, 3=POS)",
    x = "Right-Wing (0 = No, 1 = Yes)",
    y = "Predicted Probability of Sentiment"
  ) +
  theme_minimal()



mod.ord.self <- clmm(
  sentiment_ord ~  lag_public_opinion +  lag_quarterly_inflation.y +
    government   + election_period +
    right_wing +
    (1|speaker), # random effects for cluster variables
   link = "logit",
  data = self_sentiment)

summary(mod.ord.self)
