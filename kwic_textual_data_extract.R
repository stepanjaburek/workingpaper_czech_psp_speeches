library(quanteda)
library(tidyverse)
library(flexplot)


######################
# Data
data<-read.csv("C:/Users/stepan.jaburek/Downloads/CzechParlPlenaryUpdated_13.07.2024.csv")

data$month <- my(data$month)
data$month <- format(data$month, "%Y-%m")


data<-data %>% filter(year>2019) # Smaller data for test run to run faster

#####################################
# Define the regex pattern 
pattern <- "\\bgreen\\b"
#pattern <- "\\bneomarxis(mus|t[aéiů]|tka|tick[ýéáí]|tickou)\\b"


####################################
# Extract refreneces with context

# Create corpus
corpus_speeches <- corpus(
  data$text,
  docvars = data.frame(
    id = data$id,
    speaker = data$speaker,
    party = data$party,
    chair = data$chair,
    month = data$month,
    year = data$year
  )
)

# Tokenize
tokens_speeches <- tokens(corpus_speeches)

# Keywords-in-context
kwic_results <- kwic(
  tokens_speeches,
  pattern = pattern,
  window = 20, #or something else
  valuetype = "regex"
)

# Create final data frame with  the exact matched word, the full context in the window around and all metadata
kwic_df <- as_tibble(kwic_results) %>%
  left_join(
    data.frame(
      docid = names(corpus_speeches),
      id = docvars(corpus_speeches)$id,
      speaker = docvars(corpus_speeches)$speaker,
      party = docvars(corpus_speeches)$party,
      chair = docvars(corpus_speeches)$chair,
      month = docvars(corpus_speeches)$month,
      year = docvars(corpus_speeches)$year
    ),
    by = c("docname" = "docid")
  ) %>%
  mutate(
    matched_word = str_extract(keyword, "\\S+"),
    context_full = paste(pre, matched_word, post)
  ) %>%
  select(
    id,
    speaker,
    party,
    chair,
    month,
    year,
    matched_word,
    context_full
  )


write.csv(kwic_df,"green.csv")
