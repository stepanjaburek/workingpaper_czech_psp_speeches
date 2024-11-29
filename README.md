# Parliamentary Discourse about the Left and Right in a Postcommunist Context: The Czech Case

## Working Paper Code Repository

With paper co-authors: Jan Vondráček, Vojtěch Pohanka, and Tomáš Kremla 


## Repository Purpose

Provide complete start-to-end reproducible analysis

Contain all code for data collection, wrangling, machine translation, classification, model testing, and statistical analysis

We welcome community feedback and reports of any errors! Thats the spirit of open science!

## Current Repository Contents
1. Text Extraction (Keywords_in_context.R)

  contains the R code to extract references and the context around them using the quanteda package.

2. Translation and Automated Sentiment analysis(Streamline_Translation_Sentiment.ipynb)

  contains the python code to machine translate the texts using the Opus-MT model from the Language Technology Research Group at the University of Helsinki https://huggingface.co/Helsinki-NLP/opus-mt-en-mt and to classify them using the Political DEBATE model by Burnham et al. (2024) https://huggingface.co/mlburnham/Political_DEBATE_large_v1.0.

3. Visualisation and statistical analysis (Salience_base-script.R)
   Work in progress


## Current state

Latest version of the code streamline for outputs that will be presennted on the New Frontiers in Social Sciences conference.
