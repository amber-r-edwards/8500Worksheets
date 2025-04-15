#Assignment Parameters: 
#   - build according to method in class and analyze it
#   - after exploring and building at least 4 visualizations that investigate aspects of the text, write a summary of findings
#       - rely on historical context: what questions did you ask and what did you learn?
#   - something to consider:
#       - what happens if you divide corpus and train/compare two models?


library(wordVectors)
library(tidyverse)
library(ggplot2)

if(!file.exists("quest.txt")) prep_word2vec(origin = "questfiles", destination = "quest.txt", lowercase = T, bundle_ngrams = 1)

if (!file.exists("quest.bin")) {
    model <- train_word2vec("quest.txt", "quest.bin", vectors = 150, threads = 3, window = 12, iter = 5, negative_samples = 0)   
} else {
    model <- read.vectors("quest.bin")
}

model %>% closest_to("feminism")
model %>% closest_to("farming")
model %>% closest_to("sex")
model %>% closest_to(c("sex", "sexuality"), n = 25)
model %>% closest_to(c("feminist", "feminism"), n = 25)

