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

radical2 <- model[[c("radical", "feminism", "gay", "hetero"), average = F]]
common_similarities_radical2 <- model[1:3000, ] %>% cosineSimilarity(radical2)
common_similarities_radical2[20:30, ]
high_similarities_to_radical <- common_similarities_radical2[rank(-apply(common_similarities_radical2, 1, max)) < 75, ]
high_similarities_to_radical %>%
    prcomp() %>%
    biplot(main = "50 words in a \n projection of radical, feminism, gay, and hetero")
#interesting for a few reasons
#  -mostly legal/political language near gay (caucus, custody, rights) - separatist and soclialist between gay and radical
#  -a lot more words near hetero (choce because I had seen it appear higher on other lists assuming bc OCR errors
#  -most language resembling theory or scholarship closer to feminism or the center

#using calculations to find words that are most used with women vs men
#using closest_to() did not work - solution posed to use nearest_to()

model %>%
    nearest_to(model[["women"]]) %>%
    round(3)
#mostly stop words

model %>%
    nearest_to(model[["men"]]) %>%
    round(3)
#first word is HARASSING (too real) and threatened is #4

model %>%
    nearest_to(model[[c("she", "her", "women", "woman")]] - model[[c("he", "his", "man", "man")]]) %>%
    round(3)
#not super clear (astrology, expressing, politicization) - probably skewed because of content of corpus focusing on women

model %>%
    nearest_to(model[[c("gay", "lesbian", "homosexual")]] - model[[c("straight", "hetero", "heterosexual")]]) %>%
    round(3)
#legislature, connecticut, seattle, featuring, files, directory, gay, coast, chairperson, maga - wondering where these places came from, a lot of political/legislative language as well
#curious what would happen with the reverse
model %>%
    nearest_to(model[[c("straight", "hetero", "heterosexual")]] - model[[c("gay", "lesbian", "homosexual")]]) %>%
    round(3)
#hetero, superiority, heterosexual, priviledge, divides, middle, distinctions, divide, creates, perpetuated - all seem to pull from discussion of LGBTQ oppression


wowords <- model[[c("female", "females", "women", "woman", "feminine", "she", "woman's")]] %>% reject(model[[c("male", "males", "men", "man", "masculine", "he", "men's")]])
model %>% nearest_to(wowords, 100)

menwords <- model[[c("male", "males", "men", "man", "masculine", "he", "men's")]] %>% reject(model[[c("female", "females", "women", "woman", "feminine", "she", "woman's")]])
model %>% nearest_to(menwords, 100)
#wowords has a lot more variety than menwords - primarily in language describing institutions/systems of oppression dominated by men whereas wowords seems to refer to skills, values, attributes, and criticisms

