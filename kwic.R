####################################
# Extract keywords in context
####################################
# Setup
library(tidyverse)
library(quanteda)

######################
# Data
######################
data <- read.csv("dataset_mentions.csv")

#filter out new dataset with all left and right mentions
data_kwic<-data %>% filter(left_mentions>=1|right_mentions>=1)

data_kwic <- data_kwic %>%
  mutate(ideologid = row_number())

###############################
# Create corpus
corpus_speeches <- corpus(
  data_kwic$text,
  docvars = data.frame(
    id = data_kwic$id,
    speaker = data_kwic$speaker,
    party = data_kwic$party,
    chair = data_kwic$chair,
    month = data_kwic$month,
    year = data_kwic$year,
    ideologid=data_kwic$ideologid
  )
)

# Tokenize
tokens_speeches <- tokens(corpus_speeches)

#############################
# Left mentions extraction
############################
# Keywords-in-context 
kwic_left <- kwic(
  tokens_speeches,
  pattern = pattern_left,
  window = 40, 
  valuetype = "regex"
)

# Create final data frame with  the exact matched word, the full context in the window around and all metadata
kwic_df_left <- as_tibble(kwic_left) %>%
  left_join(
    data.frame(
      docid = names(corpus_speeches),
      id = docvars(corpus_speeches)$id,
      speaker = docvars(corpus_speeches)$speaker,
      party = docvars(corpus_speeches)$party,
      chair = docvars(corpus_speeches)$chair,
      month = docvars(corpus_speeches)$month,
      year = docvars(corpus_speeches)$year,
      ideologid=docvars(corpus_speeches)$ideologid
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
    ideologid,
    matched_word,
    context_full
  )

############################
# Filter out windows that also mention the right 
kwic_df_left$right_mentions <- str_count(kwic_df_left$context_full, regex(pattern_right))
kwic_df_left<-kwic_df_left %>% filter(right_mentions==0)

# Final dataset of 40 word windows around only left mentions
write.csv(kwic_df_left, "left.csv")


#############################
# Right mentions extraction
#############################
# Keywords-in-context
kwic_right <- kwic(
  tokens_speeches,
  pattern = pattern_right,
  window = 40, 
  valuetype = "regex"
)

# Create final data frame with  the exact matched word, the full context in the window around and all metadata
kwic_df_right <- as_tibble(kwic_right) %>%
  left_join(
    data.frame(
      docid = names(corpus_speeches),
      id = docvars(corpus_speeches)$id,
      speaker = docvars(corpus_speeches)$speaker,
      party = docvars(corpus_speeches)$party,
      chair = docvars(corpus_speeches)$chair,
      month = docvars(corpus_speeches)$month,
      year = docvars(corpus_speeches)$year,
      ideologid=docvars(corpus_speeches)$ideologid
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
    ideologid,
    matched_word,
    context_full
  )
############################
# Filter out windows that also mention the left 
kwic_df_right$left_mentions <- str_count(kwic_df_right$context_full, regex(pattern_left))
kwic_df_right<-kwic_df_right %>% filter(left_mentions==0)

# Final dataset of 40 word windows around only right mentions
write.csv(kwic_df_right, "right.csv")

##################################
# SWITCH TO PYTHON FOR MACHINE TRANSLATION AND NATURAL LANGUAGE INFERENCE
##################################