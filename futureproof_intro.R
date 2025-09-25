install.packages("devtools")  # If not already installed
library(devtools)
install_local("/Users/stepanjaburek/Downloads/futureproofR") 


library(futureproofR)
library(tidyverse)
library(ranger)
library(scales)
library(stringr)

url <- "https://raw.githubusercontent.com/rupakc/UCI-Data-Analysis/refs/heads/master/Boston%20Housing%20Dataset/Boston%20Housing/housing.data"
housing <- read.table(url, header = FALSE, sep = "", stringsAsFactors = FALSE) %>%
  as_tibble()

colnames(housing) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM",
                       "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B",
                       "LSTAT", "MEDV")

head(housing)


m <- lm(MEDV ~ CHAS + NOX + CRIM, data = housing)
summary(m)


toplot <- misclass_sens(dat = housing,
                        outcome = 'MEDV',
                        treatment = 'CHAS',
                        binary = 'CHAS',
                        nsims = 200,
                        tol = 0.02,
                        m = m)

misclass_sens_plot(toplot, 
                   m, 
                   treatment = 'CHAS') +
  coord_cartesian(ylim = c(-50,50))



results <- misclass_sens(
  dat = your_data,
  outcome = "treatment",  # Treat your original treatment as the "outcome" for the function
  treatment = "DV",       # Your misclassified DV is now the "treatment" in the function
  binary = "DV",          # Specify the DV as the binary variable to flip
  nsims = 100,            # Number of simulations
  tol = 0.01,             # Tolerance for reclassification rate
  m = m,                  # Your fitted model
  R_vect = seq(0.01, 0.5, by = 0.01)  # Range of misclassification rates to test
)
