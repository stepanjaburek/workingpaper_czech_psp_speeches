library(tidyverse)
library(splines)

left<-read.csv("debate_sentiment_left.csv")
right<-read.csv("debate_sentiment_right.csv")

###########################
yearly_left <- left %>% 
  group_by(year) %>%
  mutate(total = n()) %>%
  group_by(year, label) %>%
  summarize(  
    prop = n()/first(total)*100,
    n = n(),              
    total = first(total),  
    .groups = "drop"
  ) %>% 
  filter(label == "negative")

# Spline specification
attach(yearly_left)
in.knots = c(1998,2006,2013,2017)
xgrid = seq(min(year), max(year), , by = 0.1)
m.splines.l = lm(prop~bs(year, degree = 3, df = 20, knots = in.knots, Boundary.knots = range(year)))
plot(prop~year, col = 4, pch = 21, bg = "lightblue")
lines(predict(m.splines.l, newdata = data.frame(year = xgrid))~xgrid, col = 2)

yearly_right <- right %>% 
  group_by(year) %>%
  mutate(total = n()) %>%
  group_by(year, label) %>%
  summarize(  
    prop = n()/first(total)*100,
    n = n(),              
    total = first(total),  
    .groups = "drop"
  ) %>% 
  filter(label == "negative")


attach(yearly_right)
in.knots.r = c(1998,2006,2013,2017)
xgrid.r = seq(min(year), max(year), , by = 0.1)
m.splines.r = lm(prop~bs(year, degree = 3, df = 20, knots = in.knots.r, Boundary.knots = range(year)))
plot(prop~year, col = 4, pch = 21, bg = "lightblue")
lines(predict(m.splines.r, newdata = data.frame(year = xgrid.r))~xgrid.r, col = 2)


##################################################################################x
# Modeling
library(lmtest)
library(betareg)
#########################x
# linear model
model_left <- lm(prop ~ year, data=yearly_left)
model_right <- lm(prop ~ year, data=yearly_right)

# Lagged linear model
yearly_left$prop_lag <- c(NA, head(yearly_left$prop, -1))
lag_model_left <- lm(prop ~ year + prop_lag, data = yearly_left[-1,])  # Remove first row due to NA
summary(lag_model_left)

yearly_right$prop_lag <- c(NA, head(yearly_right$prop, -1))
lag_model_right <- lm(prop ~ year + prop_lag, data = yearly_right[-1,])  # Remove first row due to NA
summary(lag_model_right)

#####
# Beta regression
#beta_left <- betareg(prop ~ year, data = yearly_left)
#beta_right <- betareg(prop ~ year, data = yearly_right)
########
# Time Series ARIMA
library(forecast)
ts_data <- ts(yearly_left$prop, start = min(yearly_left$year))
model <- auto.arima(ts_data)
summary(model)
############################################xx
# export regression tables
library(modelsummary)
library(gt)
library(kableExtra)

models <- list()
models[['Left (linear)']] <- model_left 
models[['Left (lag)']] <- lag_model_left
models[['Right (linear)']] <- model_right
models[['Right (lag)']] <- lag_model_right

names <- c(
  year = "Year",
  prop_lag = "Lagged DV",
  "(Intercept)" = "Intercept"
  
)

tab <- modelsummary(models,
                    output = "gt", 
                    stars = TRUE,
                    gof_omit = 'BIC|Log|RMSE',
                    title = 'Trends in Negative Porportions of Left and Right Mentions',
                    coef_map = names)
tab

gt::gtsave(tab, filename = "mentions_models.png")

#####################################
# Model comparison
#######################x
# ANOVA and p-values
model.comparison(model_left,lag_model_left)
model.comparison(model_right,lag_model_right)

anova(model_left,m.splines.l)
anova(model_right,m.splines.r)

lrtest(beta_left, beta_spline_left)
lrtest(beta_right, beta_spline_right)

#######################x
install.packages("devtools")
library(devtools)
devtools::install_github("dustinfife/flexplot")
library(flexplot)

# AIC, BIC a model.comparison z flexplotu
model.comparison(model_left,m.splines.l)
model.comparison(model_right,m.splines.r)

model.comparison(beta_left, beta_spline_left)
model.comparison(beta_right, beta_spline_right)
