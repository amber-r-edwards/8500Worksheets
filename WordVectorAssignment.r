#Assignment Parameters: 
#   - build according to method in class and analyze it
#   - after exploring and building at least 4 visualizations that investigate aspects of the text, write a summary of findings
#       - rely on historical context: what questions did you ask and what did you learn?
#   - something to consider:
#       - what happens if you divide corpus and train/compare two models?


# ----------------------------------------------------
# Building according to class method/experimentation
# ----------------------------------------------------

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

#using same example from class but changing gender to sexuality
sexuality_vector <- model[[c("gay", "lesbian", "homosexual")]] - model[[c("straight", "hetero", "heterosexual")]]
word_scores <- data.frame(word = rownames(model))
word_scores$sexuality_score <- model %>%
    cosineSimilarity(sexuality_vector) %>%
    as.vector()

ggplot(word_scores %>% filter(abs(sexuality_score) > .33)) +
    geom_bar(aes(y=sexuality_score, x = reorder(word, sexuality_score), fill=sexuality_score <0), stat = "identity") +
    coord_flip() +
    scale_fill_discrete("Indicative of sexuality (limited to homosexual and heterosexual)", labels = c("homosexual", "heterosexual")) +
    labs(title = "The words showing the strongest skew along the sexuality binary")
#not sure how effective this is: uses a binary that is reductive of a full spectrum of sexualities, because the corpus is feminist focused it doesn't reflect a larger public consensus just a consensus from the articles published and their content

#trying the gender one from class
gender_vector <- model[[c("feminine", "feminity", "woman", "women")]] - model[[c("masculine", "masculinity", "men", "man")]]

word_scores <- data.frame(word = rownames(model))
word_scores$gender_score <- model %>%
    cosineSimilarity(gender_vector) %>%
    as.vector()

ggplot(word_scores %>% filter(abs(gender_score) > .33)) +
    geom_bar(aes(y = gender_score, x = reorder(word, gender_score), fill = gender_score < 0), stat = "identity") +
    coord_flip() +
    scale_fill_discrete("Indicative of gender", labels = c("Feminine", "masculine")) +
    labs(title = "The words showing the strongest skew along the gender binary")
#shows only feminine words  - not effective as id suspected given the content

#Clustering
set.seed(10)
centers <- 150
clustering <- kmeans(model, centers = centers, iter.max = 40)

sapply(sample(1:centers, 10), function(n) {
    names(clustering$cluster[clustering$cluster == n][1:10])
}) 
#clusters 2 and 9 seem to be about race; clusters 3 and 6 seem to be about history and/or feminisms position?

#Dendograms
ideology <- c("radical", "socialist", "marxist", "feminist")
term_set <- lapply(
    ideology,
    function(ideology) {
        nearest_words <- model %>% closest_to(model[[ideology]], 20)
        nearest_words$word
    }
) %>% unlist()
subset <- model[[term_set, average = F]]
subset %>%
    cosineDist(subset) %>%
    as.dist() %>%
    hclust() %>%
    plot()
#somewhat difficult to read - repetitions of words in diff sections (specifically socialism/socialist) but can see some distinct clusters - marxist theory and orthodoxy at the left, assuming feminist is linking together the terms with dates and quarterly, and then radical and socialist toward the right with associations with gays and political

# -----------------------------------------
#  Historical Questions/Analysis
# -----------------------------------------

#IDEA: race in the above visualizations - connection to feminisms vs one mainstream narrative of white feminism (time period of high activity in black feminist activism - esp prison mvmt, Greene, Free Joan Little)

#Question: What words are most associated with race within Quest: Feminist Quarterly and how do they relate to the idea of separate feminisms based on race/ethnicity and class as articulated by Separate Roads to Feminism?

#Results:
# - main clusters are in the middle - nothing drastically closer to the word "black" or "white" - seems to signal that if race is discussed, both represented here are discussed (potentially in comparison)
# - clusters with higher cosine similarity to "black" : (rape, perceive, brownmiller - looser cluster); (beauty, describes, token, raped); (esteem?, rapist, confrontation?, woman, racial); (slave, women, races, mainly, minaory)
# - cluster in the middle between "black" and "white" : (hence, economically, racism, slaves, man)
# - clusters with higher cosine similarity to "white" : (status, superiority, mostly, poverty); (male, is, backgrounds, threatened, typical, workers, heterosexual, working); (blue, professional, supremacy, collar, class, lower, middle)

model %>% closest_to(c("race", "ethnicity", "class"))
model %>% closest_to("black")
model %>% closest_to("white")

race <- model[[c("black","white"), average = F]]
blackwhite <- model[1:3000, ] %>% cosineSimilarity(race)
blackwhite <- blackwhite[
    rank(-blackwhite[, 1]) < 50 |
        rank(-blackwhite[, 2]) <50,
]
plot(blackwhite, type = "n")
text(blackwhite, labels = rownames(blackwhite))
#seems like i'd need to use both singular and plurals of each to visualize better

#PCA analysis
race <- model[[c("black", "white"), average = F]]
common_similarities_race <- model[1:3000, ] %>% cosineSimilarity(race)
common_similarities_race[20:30, ]
high_similarities_race <- common_similarities_race[rank(-apply(common_similarities_race, 1, max)) < 75, ]
high_similarities_race %>%
    prcomp() %>%
    biplot(main = "50 words in a projection of black and white")
#there are some interesting cluseters - see results

# --------------------------------------------------------------------------------

#IDEA: pulling from previous methodological experiments, what is the difference between the early issues and late - associated words with sexuality and race?; first issue states purpose as seeking "in-depth feminist political analysis and ideological development" so what are the changes using political and ideological?, how does the ideal of liberation seem to change (thinking about Levenstien and Greene - changing interests/focuses of feminists)

#Question: What are the differences between closest words to "feminism", "political" and "ideological" in the first volume of issues (1974) and the last volume (1980-82 - expanded because only 1 issue in 1982)? Do they reflect a maintenance of "in-depth feminist political analysis and ideological development" as stated in the original intent? 

#Results: 
# closest to cosine similarity plotted onto a stacked bar chart
# - only overlapping words: ideological, political, depth, feminism, term
# - vol 1 seems to associate the original intent as related to broader explorations, theory, and development of feminist perspectives
# - vol 5 seems to associate the original intent as related to activism and specific impact, potentially relating the political/elected roles, as well as a global approach or more varied understandings of feminism
# highest cosine similarity scores on a biplot to spatially represent relationship to words
# - vol1global - no usage of global; only clusters leaning toward ideology but not far from center; political language around culture, gay, media, dominant; clusters by ideology (selected, statement, american, professive, belt, complex), (pagem mopey?, academy, society, review, era, operation, salves, mainstream, ed), (wife, ?, open, nuclear, professionalism, bookm family, letter)
# - vol5global - feminism and global off on their own with loose words around (employed, represent, girls, conflicts, loans, adjusted, educated, built, plans); words between ideology and global (lower, working, backgrounds, vary, adequate, divide); ideology (myths, traditionally, systems, confront, status, legal, ideal); between ideology and political (decisions, reflect); political (doctors, vice, causes, dangerous, personal, policy, standards, exam, setting, service, professions); between political and feminism/global (ethnic, generally, industrial, professionalism)

#first need to make models that filter to just the years I want
# - from my files metadata - I know which files fall under the volumes: 1- (11, 12, 13, 14) and 5 - (51, 52, 53, 54)

# - create new directories with just the files I want (NOTE - MOVES THEM OUT OF QUESTFILES, will mess up if try to run original models without reverting/duplicating first)
#vol 1
if(!dir.exists("questvol1files")) {dir.create("questvol1files")}

vol1files <- file.path("questfiles", c("questfeministqua11wash.txt", "questfeministqua12wash.txt", "questfeministqua13wash.txt", "questfeministqua14wash.txt"))

for (file in vol1files) { file.rename(file, file.path("questvol1files", basename(file)))}

if(!file.exists("questvol1.txt")) prep_word2vec(origin = "questvol1files", destination = "questvol1.txt", lowercase = TRUE, bundle_ngrams = 1)

if (!file.exists("questvol1.bin")) {
    vol1model <- train_word2vec("questvol1.txt", "questvol1.bin", vectors = 150, threads = 3, window = 12, iter = 5, negative_samples = 0)   
} else {
    vol1model <- read.vectors("questvol1.bin")
}

#vol 5
if(!dir.exists("questvol5files")) {dir.create("questvol5files")}

vol5files <- file.path("questfiles", c("questfeministqua51wash.txt", "questfeministqua52unse.txt", "questfeministqua53wash.txt", "questfeministqua54wash.txt"))

for (file in vol5files) { file.rename(file, file.path("questvol5files", basename(file)))}

if(!file.exists("questvol5.txt")) prep_word2vec(origin = "questvol5files", destination = "questvol5.txt", lowercase = TRUE, bundle_ngrams = 1)

if (!file.exists("questvol5.bin")) {
    vol5model <- train_word2vec("questvol5.txt", "questvol5.bin", vectors = 150, threads = 3, window = 12, iter = 5, negative_samples = 0)   
} else {
    vol5model <- read.vectors("questvol5.bin")
}

#most common words:
vol1words <- vol1model %>% closest_to(c("feminism", "political", "ideological"), n = 20)
# political, ideological, depth, feminism, analysis, explore, term, project, ideology, inherent, theory, cultural, development, fundamental, discussions, perspectives, exists, long, tool, alters

vol5words <- vol5model %>% closest_to(c("feminism", "political", "ideological"), n = 20)
# ideological, feminism, depth, activism, political, seeking, perspective, emerged, griffin's, speaks, global, insights, impact, parties, term, comprehensive, separatism, nist, leading, various

#def shows what I was thinking with depth, perspective, global, insights, separatism - gestures toward engaging with other feminisms and international coalitions forming

#join together then plot
volumewordsimilarity <- full_join(vol1words, vol5words, by = "word")
colnames(volumewordsimilarity)[2] <- "vol1_similarity"
colnames(volumewordsimilarity)[3] <- "vol5_similarity"
volumewordsimilarity[is.na(volumewordsimilarity)] <- 0 #gave 0 values for words where they didn't have values b/c not in the top 20 of one of the volumes

#want to do stacked bar chart
longvolsimilarity <- volumewordsimilarity %>%
    pivot_longer(
        cols = c(vol1_similarity, vol5_similarity),
        names_to = "volume",
        values_to = "similarity_score"
    ) %>%
    arrange(desc(similarity_score)) %>%
    mutate(word = reorder(word, +similarity_score))

ggplot(longvolsimilarity, aes(x = word, y = similarity_score, fill = volume)) + geom_bar(stat = "identity") + labs(title = "Word Similarity Scores to Feminism, Political, and Ideological Across Volumes", x = "Word", y = "Similarity Score", fill = "Volume") + coord_flip()
#this didn't work as well as I thought it would because there aren't many overlapping words
#not as bad after mutating to put overlapping words on top but still not optimal
# - skews which from vol 1 are highest scored because they're overlapped 

#diff between overall and volume 5 
model %>% closest_to("global", n = 15)
# global, copenhagen, feminism, invaluable, rests, prospects, globally, ideology, introduction, bunch's, libera, quarter, colonialism, theresa, imper

vol5model%>% closest_to("global", n = 15)
# global, copenhagen, technologists, prospects, exploring, feminism, comprehensive, assess, links, organiza, host, alter, difining, ecology, built

#vol1 cosine similarity scores with global and the original intent
global1 <- vol1model[[c("global", "feminism", "political", "ideology"), average = F]]
common_similarities_global1 <- model[1:3000, ] %>% cosineSimilarity(global1)
common_similarities_global1[20:30, ]
high_similarities_global1 <- common_similarities_global1[rank(-apply(common_similarities_global1, 1, max)) < 75, ]
high_similarities_global1 %>%
    prcomp() %>%
    biplot(main = "Most common words in a \n projection of global, feminism, political, and ideology")

#vol5 cosine similarity scores with global and original intent
global5 <- vol5model[[c("global", "feminism", "political", "ideology"), average = F]]
common_similarities_global5 <- model[1:3000, ] %>% cosineSimilarity(global5)
common_similarities_global5[20:30, ]
high_similarities_global5 <- common_similarities_global5[rank(-apply(common_similarities_global5, 1, max)) < 75, ]
high_similarities_global5 %>%
    prcomp() %>%
    biplot(main = "Most common words in a \n projection of global, feminism, political, and ideology")

# -----------------------------------------------------------------
 #circle back to first question - race over time: How do the spatial representations change between the 1st and 5th volumes using the same parameters?


#Summary

#   Methodologically, this corpus was more challenging to construct historical questions for word embeddings because of the differences that I began to see (though they should have been inherent) in the content than a corpus of text geared toward a more public audience. Because Quest: Feminist Quarterly is largely written by and for academics, the topics covered followed the theme of the issue, correlating to varying degrees - but genearlly are using similar language around feminist theory, scholarship, and focuses. For example, models using "maleness" and "femaleness" are not as useful for revealing associations with gender norms in a binary because gender is discussed more broadly through the lens of oppression and institutionalized norms - attempted above the historical question (lines 105-110). There is still plenty to learn about the content of Quest through examinine male and female words - revealing spatial proximity of male words to discussions of violence and oprression (ridiculed, attacking, ego, hierarchy, etc. ) - but I did not decide this was the most useful application of word embeddings to the Quest corpus to explore feminist discourse. I decided to frame my questions in the historiography of 70s and 80s feminism using two points of reference. For the first question, I focused on the plurality of feminism organizing - particularlly around race/ethnicity and class as explored by Benita Roth in Separate Roads to Feminism (20024). To allow me to visualize on a binary with "opposing" impulses, and because of my assumption that there would be more content available, I focused on black and white - asking how the nearest words may relate to the idea of separate feminisms (if at all). I returned to this after creating additional models to examine the differences in the first and last issues - thinking about the period of activity of black feminism in the late 1970s and 80s that Christina Greene highlights in Free Joan Little (2022), often outside of the scope of "second wave" feminist narratives. My second question emerged from previous text analysis of this corpus and the differences between early and late issues as well as thinking chronologically about where in the historiography Quest stopped publishing. Lisa Levenstein highlights the emergence of global feminist coalitions starting in the 1980s that solidified by the 1990s - largely hidden from public perception of 90s feminism - that maintained feminist tradition and focus by extending to other issues but maintaining a network in They Didn't See Us Coming (2020). I started by considering how the content may have changed over time, using the original intent statement as my grounding text before expanding to the role of the word "global" after seeing it come up and making the connection. While I remain unsure about this corpus' utility for word embeddings, this assignment does make me feel confident in its utility for zines and other less formal publications.

#QUESTION 1:
#   Exploring the relationship between race and other words within Quest was driven by my interest to see the separate roads to feminism framed by Roth at work, particularly within a publication seeking to promote academic discussion and connect feminist scholars. I settled on a PCA analysis using "black" and "white" after results from a cosine similarity plot showed a clear separation between the terms. The main clusters are actually in the middle - nothing drastically closer to the word "black" or "white" - though still stratified at a visible level - which seems to signal that if race is duscssed, both are discussed in the same context. This makes sense considering both the roots of feminism in anti-racist activism, and ongoing debates over white feminist perspectives of focusing solely on oppression based on sex versus the inseparability of race, class, and sex-based oppression of black feminism. The clusters with higher cosine similarity to "black" center primarily around violence of different forms including sexual, systematic, and even more nuanced forms like tokenization. Discussions of sexual violence in particular relate directly to the role of rape and violence against Black women's bodies in the broader history of the African American freedom struggle that necessitates a reinterpretation of Civil Rights and feminist activism around Black women (see Danielle McGuire's Dark End of the Street - 2010). Interestingly, Susan Brownmiller appears in this visualization, most likely because of her book Against Our Will: Men, Women, and Rape that was published in 1975 that has been criticized for its inclusion (or concealment) of the racialized forms of sexism experienced by Black women. The clusters with higher cosine similarity to "white" are centered around hierarchial status within society and employment, as well as associations with men and heterosexuality. The associations with class, sex, and sexuality point toward an intersectional discussion but not one that indicates a separate feminism or the coexistance of feminisms (plural). Rather, the clusters seem to indicate that when discussing race, contributors use gendered and class based experiences to frame feminism in racial terms which could conceal the unique social positions of feminists of color.
#   After completing my plots for question 2, I decided to use the models I created for volume 1 and 5 to run the same parameters around race to see what patterns emerge.

#QUESTION 2:
#   Thinking about changes over time, Volume 1 of Quest covers just 1974 whereas Volume 5 spans from 1979-1982. By the final issues, Quest includes statements about difficulties continuing publication due to funds or contributors and editors focusing on other projects/staff turning over - hence the length of time required to finish a 5th (and final) volume. These issues are not unique to this publication as one of the challenges posed by fragmented feminist organizations is that regardless of interpersonal connections between them, there is no 'parent' organization that provides consistent funding or organization to draw together resources and contributions to keep publications afloat if they are not supported by readership and active participation. Starting with the first visualization, which admittedly does not fully achieve what I was hoping, distinctions emerge immediately between Volume 1 and Volume 5 as the only overlapping words are "ideological", "political" "depth", "feminism" and "term" - barely ranging outside of the stated intent I used to measure content - maintaining "in-depth feminist political analysis and ideolofical development." While this shows that Quest preserved its focus, the cosine similarity scores reveal changing definitions of feminist politics and application of the ideology. Volume 1 seems to associate the statement of intent with broader explorations of theory and the development of feminist political perspectives - resembling early feminist scholarship's focus on grounding the discipline and engage with the inherent structures that demand response. While the use of broad language could include a multitude of perspectives, it is not clear as a stated focus in the first volume. Volume 5 however seems to associate the statement of intent with activism potentially from differing feminist perspectives, as well as gesturing toward a global focus. Turning toward a more diverse definition of feminst politics and activism aligns with the transition of 1980s feminism toward other social movements - as well as supports Greene's argument as further evidence for the 1980s as a period of high activity among feminist activists. 
#   From there I decided to visualize the relationship between words with highest cosine similarity scores to "global" and the intent statement words - one for Volume 1 and another for Volume 5 (plots uploaded as PNGs to github) to compare. The results here are fascinating and deep, reflecting my suspicions that Quest would reflect a changing body of feminist activists in the 1980s - but I'll focus on associations with political and ideology. "The Personal is Political" is a phrase indicative of American feminsim, but in a way obfuscates the imporatnces of changing the definition for what was considered political that informed feminist activism throughout the 20th century. The plot of volume 1, with no usage of "global" at all, reveals the highest cosine similarity scores to "political" were words like "culture," "media," and "consious" as well as identity words like "woman" and "gay." "Ideology" has the most words in close proximity compared to the others, clusters pointing to American systems, academic views, and ideas of the nuclear family. These reflect the idea of bringing things that were previously considered "private" or things not considered positive rights political leaders needed to protect, changing the definition of what it means to be political while also changing perspectives toward gender norms and the American family to inform feminist movements(Self, All in the Family - 2012). The plot of volume 5 interestingly clusters "feminism" and "global" together - indicating high association with usage - but shows more specific usage of "political" and "ideology" than seen in volume 1. "Political" still maintains close association with "personal," as could be expected, but also is near "dangerous," "doctors," "policy," and "causes" which potentially reflects the increased focus of sexual violence by feminist activism as well as an increased consideration of policy that could correlate to the rising conservatism of American politics and the passage of policies that counter feminist interests. "Ideology" most closely aligns with words like "traditionally," "systems," and "myths" that seems to indicated a maintained focus on cultural institutions but a loss of more academic language. Tied with the proximity of "global" and "feminism" this embedding reflects turn away from theorhetical discussion in lieu of feminist action in support of other social causes that serve to further feminist goals themselves, not just a narrowing of views portayed in Quest that could be resulting from lack of diverse contributors. In these ways, word embedded models of Quest: Feminist Quarterly reveal an interesting view into the changing focuses of feminists from the 70s-80s.