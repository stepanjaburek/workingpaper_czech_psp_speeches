library(tidyverse)
library(ggpubr)
library(flexplot)
library(splines)
###############
# DATA

left<-read.csv("debate_sentiment_left.csv")
soc<-read.csv("debate_sentiment_soc.csv")
kom<-read.csv("debate_sentiment_kom.csv")
right<-read.csv("debate_sentiment_right.csv")
###########################
# LEFT
#########################
#Sorting by year
yearly_left <- left %>% 
  group_by(year) %>%
  mutate(total = n()) %>%
  group_by(year, label) %>%
  summarize(  
    prop = n()/first(total),
    n = n(),              
    total = first(total),  
    .groups = "drop"
  ) %>% 
  filter(label == "negative")



##################
# Spline specification
attach(yearly_left)
in.knots = c(1998,2006,2013,2017)
xgrid = seq(min(year), max(year), , by = 0.1)
m.splines.l = lm(prop~bs(year, degree = 3, df = 20, knots = in.knots, Boundary.knots = range(year)))
plot(prop~year, col = 4, pch = 21, bg = "lightblue")
lines(predict(m.splines.l, newdata = data.frame(year = xgrid))~xgrid, col = 2)


######################
# ggplot
ggplot(yearly_left, aes(x = year, y = prop)) +
  annotate("rect", xmin = min(yearly_left$year), xmax = 1998, ymin = 0.2, ymax = 0.9, 
           fill = "blue", alpha = 0.1) +
  annotate("rect", xmin = 1998, xmax = 2006, ymin = 0.2, ymax = 0.9, 
           fill = "red", alpha = 0.1) +
  annotate("rect", xmin = 2006, xmax = 2013, ymin = 0.2, ymax = 0.9, 
           fill = "blue", alpha = 0.1) +
  annotate("rect", xmin = 2013, xmax = 2017, ymin = 0.2, ymax = 0.9, 
           fill = "red", alpha = 0.1) +
  annotate("text", x = c(1995.5, 2002, 2009.5, 2015, 2020), 
           y = 0.88,
           label = c("Right-wing Gov", "Left-wing Gov", "Right-wing Gov", "Left-wing Gov", "Babiš & Fiala"), 
           size = 6) +
  geom_point(color = "blue", fill = "#2196F3", shape = 21, size=6) +
  #geom_smooth(method = "lm", se = FALSE, color = "darkgreen", linetype = "dotted")+
  geom_line(data = data.frame(
    year = xgrid,
    pred = predict(m.splines.l, newdata = data.frame(year = xgrid))),
    aes(y = pred),
    color = "#F44336",
    linewidth=1
  ) +
  geom_vline(xintercept = c(1998, 2006, 2013, 2017),color="purple",linetype = "dashed", alpha = 2.5) +
  scale_x_continuous(breaks = seq(1993, 2023, by = 2)) +
  scale_y_continuous(
    limits = c(0.2, 0.9),
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.1)
  ) +
  labs(
    x = "Year",
    y = "Proportion",
    #title = "Proportion of Negative Mentions of the Left",
    subtitle = "B-spline function • 3rd order polynomials with knots at ideological government transitions",
  ) +
  theme_pubr() + 
  theme( 
    axis.text.x = element_text(angle = 45, hjust = 1),  
    plot.subtitle = element_text(size = 20, color = "gray40"),
    plot.title = element_text(size = 30),
    axis.title = element_text(size = 20),    
    axis.text = element_text(size = 20))

###################################################
# RIGHT
############################################x
yearly_right <- right %>% 
  group_by(year) %>%
  mutate(total = n()) %>%
  group_by(year, label) %>%
  summarize(  
    prop = n()/first(total),
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
lines(predict(m.splines.r, newdata = data.frame(year = xgrid))~xgrid, col = 2)


ggplot(yearly_right, aes(x = year, y = prop)) +
  annotate("rect", xmin = min(yearly_right$year), xmax = 1998, ymin = 0.2, ymax = 0.9, 
           fill = "blue", alpha = 0.1) +
  annotate("rect", xmin = 1998, xmax = 2006, ymin = 0.2, ymax = 0.9, 
           fill = "red", alpha = 0.1) +
  annotate("rect", xmin = 2006, xmax = 2013, ymin = 0.2, ymax = 0.9, 
           fill = "blue", alpha = 0.1) +
  annotate("rect", xmin = 2013, xmax = 2017, ymin = 0.2, ymax = 0.9, # Extended to max year
           fill = "red", alpha = 0.1) +
  annotate("text", x = c(1995.5, 2002, 2009.5, 2015,2020), 
           y = 0.88,
           label = c("Right-wing Gov", "Left-wing Gov", "Right-wing Gov", "Left-wing Gov", "Babiš & Fiala"), 
           size = 6) +
  geom_point(color = "blue", fill = "#2196F3", shape = 21, size=6) +
  #geom_smooth(method = "lm", se = FALSE, color = "#e74c3c", linetype = "dotted")+
  geom_line(data = data.frame(
    year = xgrid,
    pred = predict(m.splines.r, newdata = data.frame(year = xgrid))),
    aes(y = pred),
    color = "#F44336",
    linewidth=1
  ) +
  geom_vline(xintercept = c(1998, 2006, 2013, 2017),color="purple",linetype = "dashed", alpha = 2.5) +
  scale_x_continuous(breaks = seq(1993, 2023, by = 2)) +
  scale_y_continuous(
    limits = c(0.2, 0.9),
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.1),
    #expand = c(0, 0) 
  ) +
  labs(
    x = "Year",
    y = "Proportion",
    #title = "Proportion of Negative Mentions of the Right",
    subtitle = "B-spline function • 3rd order polynomials with knots at ideological government transitions",
  ) +
  theme_pubr() + 
  theme( 
    axis.text.x = element_text(angle = 45, hjust = 1),  
    plot.subtitle = element_text(size = 20, color = "gray40"),
    plot.title = element_text(size = 30),
    axis.title = element_text(size = 20),    
    axis.text = element_text(size = 20))

##################################################################################x
# Testing
library(lmtest)
library(betareg)
library(modelsummary)
library(gt)
#########################x
# linear model
model_left <- lm(prop ~ year, data=yearly_left)
model_right <- lm(prop ~ year, data=yearly_right)

# beta regression
beta_left <- betareg(prop ~ year, data = yearly_left)
beta_right <- betareg(prop ~ year, data = yearly_right)

# linear spline and beta spline
attach(yearly_left)
in.knots = c(1998,2006,2013,2017)
xgrid = seq(min(year), max(year), , by = 0.1)
m.splines.l = lm(prop~bs(year, degree = 3, df = 20, knots = in.knots, Boundary.knots = range(year)))
beta_spline_left <- betareg(prop ~ bs(year, degree=3, knots=in.knots), data=yearly_left)

attach(yearly_right)
in.knots.r = c(1998,2006,2013,2017)
xgrid.r = seq(min(year), max(year), , by = 0.1)
m.splines.r = lm(prop~bs(year, degree = 3, df = 20, knots = in.knots.r, Boundary.knots = range(year)))
beta_spline_right <- betareg(prop ~ bs(year, degree=3, knots=in.knots), data=yearly_right)

#################
# model comparison
anova(model_left,m.splines.l)
anova(model_right,m.splines.r)

lrtest(beta_left, beta_spline_left)
lrtest(beta_right, beta_spline_right)

models <- list()
models[['Left (linear)']] <- model_left 
models[['Left (beta)']] <- beta_left
models[['Right (linear)']] <- model_right
models[['Right (beta)']] <- beta_right

names <- c(
  year = "Year",
  "(Intercept)" = "Intercept"
)

tab <- modelsummary(models,
                    output = "gt", 
                    stars = TRUE,
                    gof_omit = 'IC|Log|Adj',
                    title = 'Trends in Negative Left and Right Mentions Over Time',
                    coef_map = names)

gt::gtsave(tab, filename = "mentions_models.png")




model_comparisons <- data.frame(
  Metric = c("Beta Regression (basic vs spline)", 
             "Linear Regression (basic vs spline)"),
  Left = c(0.8157, 0.8159),
  Right = c(0.004, 0.01308)
)

tab_comparison <- gt(model_comparisons) %>%
  tab_header(title = "Model Comparison: Basic vs Spline Models") %>%
  fmt_number(columns = c("Left", "Right"), decimals = 3) %>%
  cols_label(
    Left = "Left",
    Right = "Right"
  )

gt::gtsave(tab_comparison, filename = "model_comparisons.png")

###################################################
# SOCIALISM
############################################x

yearly_soc <- soc %>% 
  group_by(year) %>%
  mutate(total = n()) %>%
  group_by(year, label) %>%
  summarize(  
    prop = n()/first(total),
    n = n(),              
    total = first(total),  
    .groups = "drop"
  ) %>% 
  filter(label == "negative")

attach(yearly_soc)
in.knots = c(1998,2006,2013,2017)
xgrid = seq(min(year), max(year), , by = 0.1)
m.splines = lm(prop~bs(year, degree = 3, df = 20, knots = in.knots, Boundary.knots = range(year)))
plot(prop~year, col = 4, pch = 21, bg = "lightblue")
lines(predict(m.splines, newdata = data.frame(year = xgrid))~xgrid, col = 2)

ggplot(yearly_soc, aes(x = year, y = prop)) +
  annotate("rect", xmin = min(yearly_right$year), xmax = 1998, ymin = 0.2, ymax = 0.9, 
           fill = "blue", alpha = 0.05) +
  annotate("rect", xmin = 1998, xmax = 2006, ymin = 0.2, ymax = 0.9, 
           fill = "red", alpha = 0.05) +
  annotate("rect", xmin = 2006, xmax = 2013, ymin = 0.2, ymax = 0.9, 
           fill = "blue", alpha = 0.05) +
  annotate("rect", xmin = 2013, xmax = 2017, ymin = 0.2, ymax = 0.9, # Extended to max year
           fill = "red", alpha = 0.05) +
  annotate("text", x = c(1995.5, 2002, 2009.5, 2015,2020), 
           y = 0.88,
           label = c("Right-wing Gov", "Left-wing Gov", "Right-wing Gov", "Left-wing Gov", "Babiš & Fiala"), 
           size = 6) +
  geom_point(color = "blue", fill = "#2196F3", shape = 21, size=6) +
  #geom_smooth(method = "lm", se = FALSE, color = "#e74c3c", linetype = "dotted")+
  geom_line(data = data.frame(
    year = xgrid,
    pred = predict(m.splines.r, newdata = data.frame(year = xgrid))),
    aes(y = pred),
    color = "#F44336",
    linewidth=1
  ) +
  geom_vline(xintercept = c(1998, 2006, 2013, 2017),color="purple",linetype = "dashed", alpha = 2.5) +
  scale_x_continuous(breaks = seq(1993, 2023, by = 2)) +
  scale_y_continuous(
    limits = c(0.2, 0.9),
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.1),
    #expand = c(0, 0) 
  ) +
  labs(
    x = "Year",
    y = "Proportion",
    title = "Proportion of Negative Mentions of Socialism",
    subtitle = "B-spline function • 3rd order polynomials with knots at ideological government transitions",
  ) +
  theme_pubr() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.title = element_text(size = 16))

###################################################
# COMMUNISM
############################################x
yearly_kom <- kom %>% 
  group_by(year) %>%
  mutate(total = n()) %>%
  group_by(year, label) %>%
  summarize(  
    prop = n()/first(total),
    n = n(),              
    total = first(total),  
    .groups = "drop"
  ) %>% 
  filter(label == "negative")

attach(yearly_kom)
in.knots = c(1998,2006,2013,2017)
xgrid = seq(min(year), max(year), , by = 0.1)
m.splines = lm(prop~bs(year, degree = 3, df = 20, knots = in.knots, Boundary.knots = range(year)))
plot(prop~year, col = 4, pch = 21, bg = "lightblue")
lines(predict(m.splines, newdata = data.frame(year = xgrid))~xgrid, col = 2)

ggplot(yearly_kom, aes(x = year, y = prop)) +
  annotate("rect", xmin = min(yearly_right$year), xmax = 1998, ymin = 0.35, ymax = 0.7, 
           fill = "blue", alpha = 0.1) +
  annotate("rect", xmin = 1998, xmax = 2006, ymin = 0.35, ymax = 0.7, 
           fill = "red", alpha = 0.1) +
  annotate("rect", xmin = 2006, xmax = 2013, ymin = 0.35, ymax = 0.7, 
           fill = "blue", alpha = 0.1) +
  annotate("rect", xmin = 2013, xmax = 2017, ymin = 0.35, ymax = 0.7, # Extended to max year
           fill = "red", alpha = 0.1) +
  annotate("text", x = c(1995.5, 2002, 2009.5, 2015,2020), 
           y = 0.68,
           label = c("Right-wing Gov", "Left-wing Gov", "Right-wing Gov", "Left-wing Gov", "Babiš & Fiala"), 
           size = 3) +
  geom_point(color = "blue", fill = "lightblue", shape = 21, size=5) +
  #geom_smooth(method = "lm", se = FALSE, color = "darkgreen", linetype = "dotted") +
  geom_line(data = data.frame(
    year = xgrid,
    pred = predict(m.splines, newdata = data.frame(year = xgrid))),
    aes(y = pred),
    color = "red",
    linewidth=1
  ) +
  geom_vline(xintercept = c(1998, 2006, 2013, 2017),color="purple",linetype = "dashed", alpha = 2.5) +
  scale_x_continuous(breaks = seq(1993, 2023, by = 2)) +
  scale_y_continuous(
    limits = c(0.35, 0.7),
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.1),
    #expand = c(0, 0) 
  ) +
  labs(
    x = "Year",
    y = "Proportion",
    title = "Proportion of Negative Mentions of Communism",
    subtitle = "B-spline function • 3rd order polynomials with knots at ideological government transitions",
  ) +
  theme_pubr() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.title = element_text(size = 16))

#######################xx
# loess 
yearly_left <- left %>% 
  group_by(year) %>%
  mutate(total = n()) %>%
  group_by(year, label) %>%
  summarize(  
    prop = n()/first(total),
    .groups = "drop"
  ) %>% 
  filter(label == "negative") %>%
  mutate(group = "Left")

yearly_right <- right %>% 
  group_by(year) %>%
  mutate(total = n()) %>%
  group_by(year, label) %>%
  summarize(  
    prop = n()/first(total),
    .groups = "drop"
  ) %>% 
  filter(label == "negative") %>%
  mutate(group = "Right")


all_data <- bind_rows(yearly_left, yearly_right)


ggplot(all_data, aes(x = year, y = prop * 100, color = group)) +
  #geom_line() +
  geom_smooth(method="loess", linewidth=2) +
  geom_point(size=3) +
  facet_wrap(~group, ncol = 2) +
  geom_hline(yintercept = 50, color = "gray50", linetype = "dashed", alpha = 0.7) +
  scale_x_continuous(breaks = seq(1993, 2023, by = 5)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), 
                     breaks = seq(0, 100, by = 10)) +
  scale_color_manual(values = c("#FF6B6B", "royalblue")) +  
  labs(
    x = "Year",
    y = "Proportion",
    #title = "Proportion of Negative Mentions Over Time",
    subtitle = "LOESS",
    color = "Party"
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.subtitle = element_text(size = 12),
    plot.title = element_text(size = 15),
    axis.title = element_text(size = 20),    
    axis.text = element_text(size = 20),
    legend.position = "none"
  )


#######################################x
# Complete plot in base r




attach(yearly_left)
in.knots = c(1998,2006,2013,2017)
xgrid = seq(min(year), max(year), , by = 0.1)
m.splines = lm(prop~bs(year, degree = 3, df = 20, knots = in.knots, Boundary.knots = range(year)))
plot(prop~year, col = 4, pch = 21, bg = "lightblue")
lines(predict(m.splines, newdata = data.frame(year = xgrid))~xgrid, col = 2)

# Set up the plot with empty canvas first
plot(prop~year, col = 4, pch = 21, bg = "lightblue", 
     panel.first = {
       # Add colored rectangles for background
       usr <- par("usr")
       rect(usr[1], usr[3], 1998, usr[4], col = rgb(0,0,1,0.1), border = NA)
       rect(1998, usr[3], 2006, usr[4], col = rgb(1,0,0,0.1), border = NA)
       rect(2006, usr[3], 2013, usr[4], col = rgb(0,0,1,0.1), border = NA)
       rect(2013, usr[3], 2017, usr[4], col = rgb(1,0,0,0.1), border = NA)
       
       # Add vertical lines for knots
       abline(v = in.knots[1], col = "purple", lwd = 2)
       abline(v = in.knots[2], col = "purple", lwd = 2)
       abline(v = in.knots[3], col = "purple", lwd = 2)
       abline(v = in.knots2[4], col = "purple", lwd = 2)
     })

# Add the spline curve on top
lines(predict(m.splines, newdata = data.frame(year = xgrid))~xgrid, col = 2)
#lines(predict(m.splines2, newdata = data.frame(year = xgrid))~xgrid, col = 3)

# Add legend
legend("topleft", 
       legend = c("1998 knot", "2006 knot", "2013 knot"),
       col = c("purple", "orange", "green"),
       lwd = 2)
