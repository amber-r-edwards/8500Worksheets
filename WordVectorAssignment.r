#Assignment Parameters: 
#   - build according to method in class and analyze it
#   - after exploring and building at least 4 visualizations that investigate aspects of the text, write a summary of findings
#       - rely on historical context: what questions did you ask and what did you learn?
#   - something to consider:
#       - what happens if you divide corpus and train/compare two models?

#load libraries
library(wordVectors)
library(tidyverse)
library(ggplot2)

#create file and prep txt by taking every txt file from directory and compiling it into one massive file
if(!file.exists("quest.txt")) prep_word2vec(origin = "questfiles", destination = "quest.txt", lowercase = T, bundle_ngrams = 1)

#output model as a file to computer - read prevents accidentally overwriting (if you need new file delete and rerun)
if (!file.exists("quest.bin")) {
    model <- train_word2vec("quest.txt", "quest.bin", vectors = 150, threads = 3, window = 12, iter = 5, negative_samples = 0)   
} else {
    model <- read.vectors("quest.bin")
}

#similarity searches
model %>% closest_to("feminism") #(feminism, radical, bunch's, perspec, socialist, nism, nam, nonaligned, global, socialism)
model %>% closest_to("farming") #(farming, tenant, towns, farms, farmers, houses, grain, farm, clubs, hollywood) - from question from text analysis worksheet
model %>% closest_to("sex") #(sex, preference, nationality, race, uality, age, sexuality, beauvoir, shulamith, firestone)
model %>% closest_to(c("sex", "sexuality", "sexual")) #(sexuality,sexual, sex, preference, nationality, distort, superiority, permeates, hetero, uality)
model %>% closest_to(c("politics", "political")) #(politics, political, inism, frameworks, feminism, theorist, personal, economics, aesthetics, connect)
    model %>% closest_to(c("political", "personal"), n = 25)

#20 most common words with personal and political
personal <- model[[c("political", "personal"), average = F]]
personalpolitics <- model[1:3000, ] %>% cosineSimilarity(personal)
personalpolitics <- personalpolitics[
    rank(-personalpolitics[, 1]) < 20 |
        rank(-personalpolitics[, 2]) < 20,
]
plot(personalpolitics, type = "n")
text(personalpolitics, labels = rownames(personalpolitics))

#not as many very close to either word as I was expecting - wondering if I need a different model that does bi-grams to keep words together? but would have to be tri-grams in case of "personal is political"