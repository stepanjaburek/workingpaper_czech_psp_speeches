library(tidyverse)
install.packages("devtools")
library(devtools)
devtools::install_github("dustinfife/flexplot")
library(flexplot)
library(lme4)
library(car)
library()
###############
# Data
left<-read.csv("left_final.csv") # individual data with topics of speeches
right<-read.csv("right_final.csv")
######################
# wrangling/cleaning
yearly_left <- left %>% #yearly data
  group_by(wing, year) %>%
  reframe(
    prop = sum(label == "negative") / n(),  # negative mentions divided by total mentions
    n = sum(label == "negative"),           # count of negative mentions
    total = n(),                             # total mentions
    gov_left = first(gov_left),
    public_left = first(public_left)
  )
##############
yearly_right <- right %>% 
  group_by(wing, year) %>% 
  reframe(
    prop = sum(label == "negative") / n(),
    n = sum(label== "negative"),
    total = n(),
    gov_right = first(gov_right),
    public_right = first(public_right)
  )
#####
yearly_left <- yearly_left %>% filter(total>=5) # get rid of the lowest ns per year, sill pretty small
yearly_right <- yearly_right %>% filter(total>=5)

############################################
# Visualization
# COPLOT Panel data by year # asi zbytečný
# prop = proportion of negative mentions
coplot(prop ~ year|wing, type="b", data=yearly_left)
coplot(prop ~ year|wing, type="b", data=yearly_right)

####################################################
# FLEXPLOT on panel data by year and wing with linear trend estimation
flexplot(prop ~ year | wing, 
         data = yearly_left, method = "lm") # year on x axis and wing in facets
flexplot(prop ~ wing | gov_left, 
         data = yearly_left, method = "lm") # by wing and binary gov in facets
flexplot(prop ~ wing | public_left, 
         data = yearly_left, method = "lm") # by wing and public left id binned in facets

flexplot(prop ~ year | wing, 
         data = yearly_right, method = "lm")  # the same for right wing 
flexplot(prop ~ wing | gov_right,
         data = yearly_right, method = "lm") 
flexplot(prop ~ wing | public_right, 
         data = yearly_right, method = "lm")
flexplot(X ~ year, data=right,jitter = c(0.4, .8))
hist(right$year)
hist(left$year)

# FLEXPLOT on individual data with logistic estimation
# sentiment <- negative sentiment = 1, neutral/postive = 0
flexplot(sentiment ~ year | wing, 
         data = left, method = "logistic", jitter = c(0, .15)) # same as before but with individual level data
flexplot(sentiment ~ year + wing | gov_left, # not losing information by grouping in years
         data = left, method = "logistic") # and we can fit a logistic line for eyeballing
flexplot(sentiment ~ public_left | wing, 
         data = left, method = "logistic")

flexplot(sentiment ~ year | wing, 
         data = right, method = "logistic")
flexplot(sentiment ~ year + wing | gov_right,
         data = right, method = "logistic")
flexplot(sentiment ~ public_right | wing,
         data = right, method = "logistic")


########################################################################x
#flexplot(prop~year+topic, 
         #data=yearly_left_topic)
#flexplot(prop~year+topic, 
         #data=yearly_right_topic)# by year and topic of the speech
#########
right$wing <- relevel(factor(right$wing), ref = "New_Parties")
left$wing <- relevel(factor(left$wing), ref = "New_Parties")
left$topic<-as.factor(left$topic)
right$topic<-as.factor(right$topic)
left$wing<-as.factor(left$wing)
right$wing<-as.factor(right$wing)
########################################
# Modeling
# Logistic regression
log_left <- glm(sentiment ~ year + wing + gov_left  + topic + public_left + election_year, 
                data = left, 
                family="binomial")
summary(log_left)

log_right <- glm(sentiment ~ year + wing + gov_right + topic + public_right+election_year, 
                data = right, 
                family="binomial")
summary(log_right)

log_left_clean <- glm(sentiment ~ year + wing + gov_left   + public_left + election_year, 
                data = left, 
                family="binomial")
summary(log_left)

log_right_clean <- glm(sentiment ~ year + wing + gov_right  + public_right+election_year, 
                 data = right, 
                 family="binomial")
summary(log_right)
# exporting the tables
library(modelsummary)
library(gt)
coef_map <- c(
  "(Intercept)" = "Intercept",  
  "year" = "Year",                    
  "wingLeft_Wing" = "Left Wing Party",      
  "wingRight_Wing" = "Right Wing Party",
  "gov_left" = "Left-Wing Government",
  "gov_right" = "Right-Wing Government", 
  "public_left" = "Public Left Self-ID",
  "public_right" = "Public Right Self-ID",
  "election_year" = "Election Year",
  "topicforeign_policy_issues" = "Foreign Policy Topic",
  "topichistorical_issues" = "Historical Topic", 
  "topicinstitutional_issues" = "Institutional Topic", 
  "topicsocial/cultural_issues" = "Social/Cultural Topic")
models <- list()
models[['Model Left 1']] <- log_left
models[['Model Left 2']] <- log_left_clean
models[['Model Right 1']] <- log_right
models[['Model Right 2']] <- log_right_clean

cap <- 'Logistic Regression: Negative/Non-negative Mentions from Individual Speech Data'
tab <- modelsummary(models,  
                    output = "gt", 
                    stars = TRUE, 
                    title = cap,  
                    gof_omit = 'F|Log|Adj|RMSE', 
                    fmt = 2,
                    coef_map = coef_map,
                    estimate = "{estimate}{stars} ({std.error})",
                    statistic = NULL, 
                    exponentiate = TRUE
                    )
tab <- tab %>%  
  opt_table_font(font = "Times New Roman") %>%  
  #tab_options(
    #data_row.padding =unit(1, "mm")) |>
  tab_source_note(
    source_note = md("**Notes:** Coefficients are expressed as odds ratios.")
  ) %>% 
  tab_source_note(
    source_note = md("Reference categories: New Parties (for party type) and Economic Issues (for topic type).")
  )
tab

gtsave(tab, "table_output.html")



######################################################
# not too important for now

# For log_left model
clustered_se_left <- vcovCL(log_left, cluster = ~ year + wing)
coeftest(log_left, vcov = clustered_se_left)

# For log_right model
clustered_se_right <- vcovCL(log_right, cluster = ~ year + wing)
coeftest(log_right, vcov = clustered_se_right)
##########################
# Multilevel model
library(lme4)
model1 <- glmer(sentiment ~ gov_left + topic + public_left + wing + (1|year),
                     family = binomial,
                     data = left)
summary(model1)
model2 <- glmer(sentiment ~ gov_left + topic + public_left + (1|wing) + (1|year),
                family = binomial,
                data = left)
summary(model2)

model3 <- glmer(sentiment ~ gov_left + topic + public_left + wing + year,
                family = binomial,
                data = left)
summary(model3)

AIC(model1, model2, model3)

vif(model_glmer)
###################x
colnames(left)[29]<-c("Analytical_Category")


yearly_right
base_plot <- flexplot(sentiment ~ year | Analytical_Category, 
         data = left, method = "logistic", jitter = c(0, .15)) 
colnames(yearly_left)[1]<-c("Analytical_Category")
colnames(yearly_right)[1]<-c("Analytical_Category")
base_plot <- flexplot(prop ~ year | Analytical_Category, data = yearly_left, method = "lm")
base_plot <- flexplot(prop ~ year | Analytical_Category, data = yearly_right, method = "lm")
base_plot +
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),  
    axis.title = element_text(size = 30, face = "bold"),  
    axis.text = element_text(size = 18),  
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
    legend.title = element_text(size = 30, face = "bold"),  
    legend.text = element_text(size = 25),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    strip.background = element_rect(fill = "grey90", color = "black", size = 1),  
    strip.text = element_text(size = 18, face = "bold")
  ) +
  labs(
    x = "Year",
    y = "Proportion of Negative Mentions" 
  )



yearly_left_topic$topic <- case_when(
  grepl("foreign_policy_issues", yearly_left_topic$topic) ~ "Foreign Policy",
  grepl("economic_issues", yearly_left_topic$topic) ~ "Economic",
  grepl("historical_issues", yearly_left_topic$topic) ~ "Historical",
  grepl("institutional_issues", yearly_left_topic$topic) ~ "Institutional",
  grepl("social/cultural_issues", yearly_left_topic$topic) ~ "Social/cultural",
  TRUE ~ yearly_left_topic$topic
)
colnames(yearly_left_topic)[1]<-c("Topic")


yearly_right_topic$topic <- case_when(
  grepl("foreign_policy_issues", yearly_right_topic$topic) ~ "Foreign Policy",
  grepl("economic_issues", yearly_right_topic$topic) ~ "Economic",
  grepl("historical_issues", yearly_right_topic$topic) ~ "Historical",
  grepl("institutional_issues", yearly_right_topic$topic) ~ "Institutional",
  grepl("social/cultural_issues", yearly_right_topic$topic) ~ "Social/cultural",
  TRUE ~ yearly_right_topic$topic
)
colnames(yearly_right_topic)[1]<-c("Topic")

base_plot<-flexplot(prop~year+Topic,data=yearly_left_topic)
base_plot<-flexplot(prop~year+Topic,data=yearly_right_topic)

base_plot +
  theme_minimal() + 
  theme(
    text = element_text(family = "serif"),  
    axis.title = element_text(size = 25, face = "bold"), 
    axis.text = element_text(size = 18), 
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
    legend.title = element_text(size = 30, face = "bold"),  
    legend.text = element_text(size = 25), 
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    strip.background = element_rect(fill = "grey90", color = "black", size = 1),  
    strip.text = element_text(size = 18, face = "bold") 
  ) +
  labs(
    x = "Year", 
    y = "Proportion of Topics of Negative Mentions"  
  )
