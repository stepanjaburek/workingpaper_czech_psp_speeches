# workingpaper_czech_psp_speeches
Code repository of our working paper "Perceptions of Left and Right in Postcommunism: The Development of Political Discourse in the Czech Parliament" with Jan Vondráček, Vojtěch Pohanka and Tomáš Kremla.
We will publish and continously update all the code we use to get the data, wrangle it and analyze it. We aim to have a comprehensive start-to-end repository where anyone can get exactly the same data as we and run the same analyses.
Any mistakes reported are more than welcome! That´s the point of open science, so we can catch possible mistakes, be open about them a fix them promptly. 
kwic_textual_data_extract.R has the R code to use our dataset (available here: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/FOQUZF) to extract references and the context around them using the quanteda package
Translate(Opus-MT).ipynb has the python code to machine translate said references using the Opus-MT model from the University of Helsinki.
Sentiment(PolDEBATE).ipynb has the python code to classify the texts using the Political DEBATE model by Burnham et al. (2024)
