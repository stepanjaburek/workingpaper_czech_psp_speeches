library(tidyverse)
library(ggpubr)
library(flexplot)
library(splines)
###############
# DATA
left<-read.csv("left_final.csv") 
right<-read.csv("right_final.csv")
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

flexplot(prop~year,data=yearly_left)
##################
# Spline specification
attach(yearly_left)
in.knots = c(1998,2006,2013,2017)
xgrid = seq(min(year), max(year), , by = 0.1)
m.splines.l = lm(prop~bs(year, degree = 3, df = 20, knots = in.knots, Boundary.knots = range(year)))
plot(prop~year, col = 4, pch = 21, bg = "lightblue")
lines(predict(m.splines.l, newdata = data.frame(year = xgrid))~xgrid, col = 2)


 
# ggplot ("#4573B3", "#B34547"))
ggplot(yearly_left, aes(x = year, y = prop)) +
  annotate("rect", xmin = min(yearly_left$year), xmax = 1998, ymin = 0, ymax = 1, 
           fill = "#4573B3", alpha = 0.1) +
  annotate("rect", xmin = 1998, xmax = 2006, ymin = 0, ymax = 1, 
           fill = "#B34547", alpha = 0.1) +
  annotate("rect", xmin = 2006, xmax = 2013, ymin = 0, ymax = 1, 
           fill = "#4573B3", alpha = 0.1) +
  annotate("rect", xmin = 2013, xmax = 2017, ymin = 0, ymax = 1, 
           fill = "#B34547", alpha = 0.1) +
  annotate("text", x = c(1995.5, 2002, 2009.5, 2015,2020), 
           y =0.9,
           label = c("Right-wing Gov", "Left-wing Gov", "Right-wing Gov", "Left-wing Gov", "Babiš & Fiala"), 
           size = 6) +
  geom_point(color = "#4573B3", fill = "#4573B3", shape = 21, size=6) +
  #geom_smooth(method = "lm", se = FALSE, color = "darkgreen", linetype = "dotted")+
  geom_line(data = data.frame(
    year = xgrid,
    pred = predict(m.splines.l, newdata = data.frame(year = xgrid))),
    aes(y = pred),
    color = "#B34547",
    linewidth=1
  ) +
  geom_vline(xintercept = c(1998, 2006, 2013, 2017),color="purple",linetype = "dashed", alpha = 2.5) +
  scale_x_continuous(breaks = seq(1993, 2023, by = 2)) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.2)
  ) +
  labs(
    x = "Year",
    y = "Proportion of Negative Mentions",
    #title = "Proportion of Negative Mentions of the Left",
    subtitle = "B-spline function • 3rd order polynomials with knots at ideological government transitions",
  ) +
  theme_minimal() + 
  theme( 
    text = element_text(family = "serif", size = 32),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 30),
    axis.text.y = element_text(size = 30),
    axis.title = element_text(size = 36),
    plot.subtitle = element_text(size = 25, color = "gray40"),
    legend.position = "bottom",
    legend.text = element_text(size = 30),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )


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
  filter(label == "negative") %>%
  # Add the missing years
  bind_rows(
    tibble(
      year = c(1994, 1995),
      label = "negative",
      prop = 0,
      n = 0,
      total = right %>% 
        filter(year %in% c(1994, 1995)) %>% 
        group_by(year) %>% 
        summarize(n = n()) %>% 
        pull(n)
    )
  )

attach(yearly_right)
in.knots.r = c(1998,2006,2013,2017)
xgrid.r = seq(min(year), max(year), , by = 0.1)
m.splines.r = lm(prop~bs(year, degree = 3, df = 20, knots = in.knots.r, Boundary.knots = range(year)))
plot(prop~year, col = 4, pch = 21, bg = "lightblue")
lines(predict(m.splines.r, newdata = data.frame(year = xgrid))~xgrid, col = 2)


ggplot(yearly_right, aes(x = year, y = prop)) +
  annotate("rect", xmin = min(yearly_right$year), xmax = 1998, ymin = 0, ymax = 1, 
           fill = "#4573B3", alpha = 0.1) +
  annotate("rect", xmin = 1998, xmax = 2006, ymin = 0, ymax = 1, 
           fill = "#B34547", alpha = 0.1) +
  annotate("rect", xmin = 2006, xmax = 2013, ymin = 0, ymax = 1, 
           fill = "#4573B3", alpha = 0.1) +
  annotate("rect", xmin = 2013, xmax = 2017, ymin = 0, ymax = 1, 
           fill = "#B34547", alpha = 0.1) +
  annotate("text", x = c(1995.5, 2002, 2009.5, 2015,2020), 
           y =0.9,
           label = c("Right-wing Gov", "Left-wing Gov", "Right-wing Gov", "Left-wing Gov", "Babiš & Fiala"), 
           size = 6) +
  geom_point(color = "#4573B3", fill = "#4573B3", shape = 21, size=6) +
  #geom_smooth(method = "lm", se = FALSE, color = "darkgreen", linetype = "dotted")+
  geom_line(data = data.frame(
    year = xgrid,
    pred = predict(m.splines.r, newdata = data.frame(year = xgrid))),
    aes(y = pred),
    color = "#B34547",
    linewidth=1
  ) +
  geom_vline(xintercept = c(1998, 2006, 2013, 2017),color="purple",linetype = "dashed", alpha = 2.5) +
  scale_x_continuous(breaks = seq(1993, 2023, by = 2)) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.2)
  ) +
  labs(
    x = "Year",
    y = "Proportion of Negative Mentions",
    #title = "Proportion of Negative Mentions of the Left",
    subtitle = "B-spline function • 3rd order polynomials with knots at ideological government transitions",
  ) +
  theme_minimal() + 
  theme( 
    text = element_text(family = "serif", size = 32),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 30),
    axis.text.y = element_text(size = 30),
    axis.title = element_text(size = 36),
    plot.subtitle = element_text(size = 25, color = "gray40"),
    legend.position = "bottom",
    legend.text = element_text(size = 30),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )
###################################################
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
  geom_smooth(method = "loess", 
              linewidth = 1.5,  
              se = TRUE,          
              alpha = 0.15) +      
  geom_point(size = 4,           
             alpha = 0.7) +       
  facet_wrap(~group, ncol = 2) +
  geom_hline(yintercept = 50, 
             color = "gray50", 
             linetype = "dashed", 
             linewidth = 0.5,   
             alpha = 0.5) +
  scale_x_continuous(breaks = seq(1993, 2023, by = 5),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), 
                     breaks = seq(0, 100, by = 10),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_color_manual(values = c("#B34547", "#4573B3")) +  
  labs(
    x = "Year",
    y = "Proportion of Negative Mentions",
    subtitle = "LOESS Smoothing with 95% Confidence Intervals"  
  ) +
  theme_minimal() +             
  theme(
    text = element_text(family = "serif", size = 36),
    axis.text.x = element_text(angle = 45, 
                               hjust = 1, 
                               size = 30),
    axis.text.y = element_text(size = 30),
    axis.title = element_text(size = 36),
    strip.text = element_text(size = 36,
                              face = "bold"),
    strip.background = element_rect(fill = "white",  
                                    color = "black",
                                    linewidth = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(fill = NA, 
                                color = "black", 
                                linewidth = 0.5),
    plot.subtitle = element_text(size = 30,
                                 hjust = 0.5,
                                 margin = margin(b = 20)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    panel.spacing = unit(2, "lines"),
    legend.position = "none"
  )

