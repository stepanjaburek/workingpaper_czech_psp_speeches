library(tidyverse)
library(ggpubr)
library(flexplot)
library(splines)
###############
# DATA
setwd("C:/Users/stepan.jaburek/Downloads")
data<-read.csv("paper_data.csv")

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
m.splines = lm(prop~bs(year, degree = 30, df = 20, knots = in.knots, Boundary.knots = range(year)))
plot(prop~year, col = 4, pch = 21, bg = "lightblue")
lines(predict(m.splines, newdata = data.frame(year = xgrid))~xgrid, col = 2)

######################
# ggplot
ggplot(yearly_left, aes(x = year, y = prop)) +
  annotate("rect", xmin = min(yearly_left$year), xmax = 1998, ymin = 0.2, ymax = 0.8, 
           fill = "blue", alpha = 0.1) +
  annotate("rect", xmin = 1998, xmax = 2006, ymin = 0.2, ymax = 0.8, 
           fill = "red", alpha = 0.1) +
  annotate("rect", xmin = 2006, xmax = 2013, ymin = 0.2, ymax = 0.8, 
           fill = "blue", alpha = 0.1) +
  annotate("rect", xmin = 2013, xmax = 2017, ymin = 0.2, ymax = 0.8, 
           fill = "red", alpha = 0.1) +
  annotate("text", x = c(1995.5, 2002, 2009.5, 2015, 2020), 
           y = 0.77,
           label = c("Right-wing Gov", "Left-wing Gov", "Right-wing Gov", "Left-wing Gov", "Babiš & Fiala"), 
           size = 3) +
  geom_point(color = "blue", fill = "lightblue", shape = 21, size=5) +
  #geom_smooth(method = "lm", se = FALSE, color = "darkgreen", linetype = "dotted")+
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
    limits = c(0.2, 0.8),
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.1)
  ) +
  labs(
    x = "Year",
    y = "Proportion",
    title = "Proportion of Negative Mentions of the Left",
    subtitle = "B-spline function • 3rd order polynomials with knots at ideological government transitions",
  ) +
  theme_pubr() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.title = element_text(size = 16))

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
in.knots = c(1998,2006,2013,2017)
xgrid = seq(min(year), max(year), , by = 0.1)
m.splines = lm(prop~bs(year, degree = 3, df = 20, knots = in.knots, Boundary.knots = range(year)))
plot(prop~year, col = 4, pch = 21, bg = "lightblue")
lines(predict(m.splines, newdata = data.frame(year = xgrid))~xgrid, col = 2)

ggplot(yearly_right, aes(x = year, y = prop)) +
  annotate("rect", xmin = min(yearly_right$year), xmax = 1998, ymin = 0.25, ymax = 0.9, 
           fill = "blue", alpha = 0.1) +
  annotate("rect", xmin = 1998, xmax = 2006, ymin = 0.25, ymax = 0.9, 
           fill = "red", alpha = 0.1) +
  annotate("rect", xmin = 2006, xmax = 2013, ymin = 0.25, ymax = 0.9, 
           fill = "blue", alpha = 0.1) +
  annotate("rect", xmin = 2013, xmax = 2017, ymin = 0.25, ymax = 0.9, # Extended to max year
           fill = "red", alpha = 0.1) +
  annotate("text", x = c(1995.5, 2002, 2009.5, 2015,2020), 
           y = 0.88,
           label = c("Right-wing Gov", "Left-wing Gov", "Right-wing Gov", "Left-wing Gov", "Babiš & Fiala"), 
           size = 3) +
  geom_point(color = "blue", fill = "lightblue", shape = 21, size=5) +
  #geom_smooth(method = "lm", se = FALSE, color = "#e74c3c", linetype = "dotted")+
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
    limits = c(0.25, 0.9),
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.1),
    #expand = c(0, 0) 
  ) +
  labs(
    x = "Year",
    y = "Proportion",
    title = "Proportion of Negative Mentions of the Right",
    subtitle = "B-spline function • 3rd order polynomials with knots at ideological government transitions",
  ) +
  theme_pubr() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.title = element_text(size = 16))


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
  annotate("rect", xmin = min(yearly_right$year), xmax = 1998, ymin = 0.25, ymax = 0.7, 
           fill = "blue", alpha = 0.1) +
  annotate("rect", xmin = 1998, xmax = 2006, ymin = 0.25, ymax = 0.7, 
           fill = "red", alpha = 0.1) +
  annotate("rect", xmin = 2006, xmax = 2013, ymin = 0.25, ymax = 0.7, 
           fill = "blue", alpha = 0.1) +
  annotate("rect", xmin = 2013, xmax = 2017, ymin = 0.25, ymax = 0.7, # Extended to max year
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
    limits = c(0.25, 0.7),
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


# First prepare all datasets
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

yearly_soc <- soc %>% 
  group_by(year) %>%
  mutate(total = n()) %>%
  group_by(year, label) %>%
  summarize(  
    prop = n()/first(total),
    .groups = "drop"
  ) %>% 
  filter(label == "negative") %>%
  mutate(group = "Social Democrats")

yearly_kom <- kom %>% 
  group_by(year) %>%
  mutate(total = n()) %>%
  group_by(year, label) %>%
  summarize(  
    prop = n()/first(total),
    .groups = "drop"
  ) %>% 
  filter(label == "negative") %>%
  mutate(group = "Communists")


all_data <- bind_rows(yearly_left, yearly_right)

# Create panel plot
ggplot(all_data, aes(x = year, y = prop, color = group)) +
  #geom_line() +
  geom_smooth(method="loess", linewidth=1)+
  geom_point(size=3) +
  facet_wrap(~group, ncol = 2) +
  geom_hline(yintercept = 0.5, color = "gray50", linetype = "dashed", alpha = 0.7) +
  scale_x_continuous(breaks = seq(1993, 2023, by = 5)) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Year",
    y = "Proportion",
    title = "Proportion of Negative Mentions Over Time",
    subtitle= "LOESS",
    color = "Party"
  ) +
  theme_pubr() +
  theme(    axis.text.x = element_text(angle = 45, hjust = 1),
            plot.subtitle = element_text(size = 12, color = "gray40"),
            plot.title = element_text(size = 15),
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
