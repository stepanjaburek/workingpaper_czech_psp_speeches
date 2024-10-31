# Perceptions of Left and Right in Postcommunism: The Development of Political Discourse in the Czech Parliament

## Working Paper Code Repository

Paper co-authors: Jan Vondráček, Vojtěch Pohanka, and Tomáš Kremla 


## Repository Purpose

Provide complete start-to-end reproducible analysis

Contain all code for data collection, wrangling, translation, classification and analysis

We welcome community feedback and reports of any errors! Thats the spirit of open science!

## Current Repository Contents
1. Text Extraction (kwic_textual_data_extract.R)

  contains the R code to use our dataset (available here: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/FOQUZF) to extract references and the context around them using the quanteda package

3. Translation (Translate_Opus-MT.ipynb)

  contains the python code to machine translate said references using the Opus-MT model from the Language Technology Research Group at the University of Helsinki https://huggingface.co/Helsinki-NLP/opus-mt-en-mt

3. Sentiment Analysis (Sentiment_PolDEBATE.ipynb)

  contains the python code to classify the texts using the Uses Political DEBATE model by Burnham et al. (2024) https://huggingface.co/mlburnham/Political_DEBATE_large_v1.0

