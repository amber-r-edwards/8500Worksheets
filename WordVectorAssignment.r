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

model %>% closest_to("intersection") #just stop words

model %>% closest_to("radical") #(radical, socialist, feminism, liberal, feminists, radi, gays, aligned, departure, separatist)

#50 most common words to a group of terms: radical, feminism, gay, sexuality
radical <- model[[c("radical", "feminism", "gay", "sexuality"), average = F]]
common_similarities_radical <- model[1:3000, ] %>% cosineSimilarity(radical)
common_similarities_radical[20:30, ]
    #mostly stopwords
high_similarities_to_list <- common_similarities_radical[rank(-apply(common_similarities_radical, 1, max)) < 75, ]
high_similarities_to_list %>%
    prcomp() %>%
    biplot(main = "50 words in a \n projection of radical, feminism, gay, and sexuality")
#this one is really cool
# - a lot of ideological terms hovering toward feminism and radical spread across middle between gay and sexuality - wonder if I change to gay and hetero how it changes because i think this is skewing it to where heterosexual is seeming closer to sexuality when that may skew the proximity of gay to discsusions of sexuality(NEXT MODEL)