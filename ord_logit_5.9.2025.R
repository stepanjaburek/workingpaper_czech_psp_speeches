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
self_sentiment <- read.csv("/Users/stepanjaburek/Documents/czech_psp_data/self_sentiment.csv")
out_sentiment<- read.csv("/Users/stepanjaburek/Documents/czech_psp_data/out_sentiment.csv")


#-------------------------------------------
self_sentiment$sentiment_ord <- as.factor(self_sentiment$sentiment_ord)
out_sentiment$sentiment_ord <- as.factor(out_sentiment$sentiment_ord)



out_sentiment <- na.omit(out_sentiment)
# Multilevel/hierarchical model
mod.ord.out <- clmm(
  sentiment_ord ~  lag_public_opinion +  lag_quarterly_inflation.y +
    government     + election_period +
    right_wing , # random effects for cluster variables
   link = "logit",
  data = self_sentiment)

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


m <- glm(sentiment ~  lag_public_opinion +  lag_quarterly_inflation.y +
    government   + election_period +
    right_wing, data = out_sentiment, family = binomial)

summary(m)

                   #       outcome,           # Name of outcome: character
                      #    treatment,         # Name of treatment: character
                     #     binary,            # Name of variable for reclassification: character

results <- misclass_sens(
  dat = out_sentiment,
  outcome = "sentiment",  
  treatment = "right_wing", 
  binary = "sentiment", 
  nsims = 100,
  tol = 0.01,
  m = m,
  R_vect = seq(0.01, 0.5, by = 0.01)
)


misclass_sens_plot(results, 
                   m, 
                   treatment = 'right_wing') +
  coord_cartesian(ylim = c(-20,20))
