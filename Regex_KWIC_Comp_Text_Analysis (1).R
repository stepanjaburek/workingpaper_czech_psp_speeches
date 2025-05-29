####################################
# Extract keywords in context
####################################
# Setup
library(tidyverse)
library(quanteda)

######################
# Data
######################
data<-read.csv("paper_data.csv")

data<-data %>% filter(chair==0) # without chairs

################################################
# VOCABULARIES
pattern_left <- paste0("\\blev(ic[eií]|icových|icov[áýé]|icovým|icovými|ičák|ičáck[ýáé])\\b|",
                       "\\w*levic\\w*")

pattern_right <- paste0("\\bprav(ic[eií]|icových|icov[áýé]|icovým|icovými|ičák|ičáck[ýáé])\\b|",
                        "\\w*pravic\\w*")

########
pattern_kom <- paste0("\\bkomunist([aéiů]|ka|ům|ická|ickou|ický|ické|ických|ickými)\\b|",
                      "\\bkomunismu(u|us)\\b|",
                      "\\w*komunis\\w*")

pattern_soc <- paste0("\\bsocialist([aéiů]|ka|ům|ická|ickou|ický|ické|ických|ickými)\\b|",
                      "\\bsocialism(u|us)\\b|",
                      "\\w*socialis\\w*")

pattern_lib <- paste0("\\bliber(áln[ěí]|ál|álka|álním[ui]|álním|álních|álního|ismus|álně)\\b")

pattern_con <- paste0("\\bkonzervat(ivní|ivec|ivním[ui]|ivního|ivních|ivismus|ivizmus|ismus|izmus)\\b")
#############################################################X
# MENTIONS EXTRACTION

data_new <- data

data_new$left_mentions <- str_count(data$text, regex(pattern_left))

data_new$right_mentions <- str_count(data$text, regex(pattern_right))

data_new$kom_mentions <- str_count(data$text, regex(pattern_kom))

data_new$soc_mentions <- str_count(data$text, regex(pattern_soc))

data_new$lib_mentions <- str_count(data$text, regex(pattern_lib))

data_new$con_mentions <- str_count(data$text, regex(pattern_con))


df_final <- data_new %>% 
  mutate(pure_left = case_when(left_mentions >= 1 & right_mentions ==0 ~ 1, #pure left mentions
                               TRUE ~ 0), 
         pure_right = case_when(right_mentions >= 1 & left_mentions ==0 ~ 1, # pure right mentions
                                TRUE ~ 0), 
         both = case_when(right_mentions >= 1 & left_mentions >=1 ~ 1,  # mentions of both left and right
                          TRUE ~ 0),
         kom = case_when(kom_mentions >= 1 ~ 1,   # raw kom mentions
                         TRUE ~ 0),
         soc = case_when(soc_mentions >= 1 ~ 1,  # raw soc mentions
                         TRUE ~ 0),
         con = case_when(con_mentions >= 1 ~ 1,  #raw con mentions
                         TRUE ~ 0),
         lib = case_when(lib_mentions >= 1 ~ 1,  # raw lib mentions
                         TRUE ~ 0)
  )

file_out <- "dataset_mentions.csv"
#################################
# you have to run the "Control_variables.R" script with dataset_mentions, then come back here
# we add some variables for statistical modeling, its easier to do it now
#############################################################xx
##### Now we move to key words in context (KWIC) extraction 
data <- read.csv("dataset_mentions_controls.csv")

#filter out new dataset with all left and right mentions
data_kwic<-data %>% filter(left_mentions>=1|right_mentions>=1)

data_kwic <- data_kwic %>%
  mutate(ideologid = row_number())

#filter out new dataset with all ideological mentions
data_kwic_2<-data %>% filter(soc_mentions>=1|kom_mentions>=1)

data_kwic_2 <- data_kwic_2 %>%
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


######################################################################
# bonus of socialism, communism etc
###############################
# Create corpus
corpus_speeches_2 <- corpus(
  data_kwic_2$text,
  docvars = data.frame(
    id = data_kwic_2$id,
    speaker = data_kwic_2$speaker,
    party = data_kwic_2$party,
    chair = data_kwic_2$chair,
    month = data_kwic_2$month,
    year = data_kwic_2$year,
    ideologid=data_kwic_2$ideologid
  )
)

# Tokenize
tokens_speeches_2 <- tokens(corpus_speeches_2)

#############################
# Socialism mentions extraction
############################
# Keywords-in-context 
kwic_soc <- kwic(
  tokens_speeches_2,
  pattern = pattern_soc,
  window = 40, 
  valuetype = "regex"
)

# Create final data frame with  the exact matched word, the full context in the window around and all metadata
kwic_df_soc <- as_tibble(kwic_soc) %>%
  left_join(
    data.frame(
      docid = names(corpus_speeches_2),
      id = docvars(corpus_speeches_2)$id,
      speaker = docvars(corpus_speeches_2)$speaker,
      party = docvars(corpus_speeches_2)$party,
      chair = docvars(corpus_speeches_2)$chair,
      month = docvars(corpus_speeches_2)$month,
      year = docvars(corpus_speeches_2)$year,
      ideologid=docvars(corpus_speeches_2)$ideologid
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
kwic_df_soc$left_mentions <- str_count(kwic_df_soc$context_full, regex(pattern_left))
kwic_df_soc$right_mentions <- str_count(kwic_df_soc$context_full, regex(pattern_right))
kwic_df_soc<-kwic_df_soc %>% filter(right_mentions==0)
kwic_df_soc<-kwic_df_soc %>% filter(left_mentions==0)

# Final dataset of 40 word windows around only soc mentions
write.csv(kwic_df_soc, "soc.csv") #with left mentions


#############################
# Communism mentions extraction
############################
# Keywords-in-context 
kwic_kom <- kwic(
  tokens_speeches_2,
  pattern = pattern_kom,
  window = 40, 
  valuetype = "regex"
)

# Create final data frame with  the exact matched word, the full context in the window around and all metadata
kwic_df_kom <- as_tibble(kwic_kom) %>%
  left_join(
    data.frame(
      docid = names(corpus_speeches_2),
      id = docvars(corpus_speeches_2)$id,
      speaker = docvars(corpus_speeches_2)$speaker,
      party = docvars(corpus_speeches_2)$party,
      chair = docvars(corpus_speeches_2)$chair,
      month = docvars(corpus_speeches_2)$month,
      year = docvars(corpus_speeches_2)$year,
      ideologid=docvars(corpus_speeches_2)$ideologid
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
sum(data$kom_mentions)
############################
# Filter out windows that also mention the right 
kwic_df_kom$left_mentions <- str_count(kwic_df_kom$context_full, regex(pattern_left))
kwic_df_kom$right_mentions <- str_count(kwic_df_kom$context_full, regex(pattern_right))
kwic_df_kom<-kwic_df_kom %>% filter(right_mentions==0)
kwic_df_kom<-kwic_df_kom %>% filter(left_mentions==0)
# Final dataset of 40 word windows around only kom mentions
write.csv(kwic_df_kom, "kom.csv")

##################################
# SWITCH TO PYTHON FOR MACHINE TRANSLATION AND NATURAL LANGUAGE INFERENCE
##################################