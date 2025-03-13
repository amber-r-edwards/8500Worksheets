#Scraping Quest: A Feminist Quarterly from Internet Archive
#Research Question - comparison with women's liberation zines, what the content of the quarterly may show about the differences between their respective authors and audiences/how much they overlap and what the role of activism looks like between zines that highlight active protest and a quarterly aiming toward intellectual discussion

library(internetarchive)
library(tidyverse)
#add search results to an item and then use that item to get metadata of each item
quest <- ia_keyword_search("collection:questquarterly", num_results = 20)
questitems <- ia_get_items(quest, silence = FALSE)
questmetadata <- ia_metadata(questitems)
#download files - but I only want the text files
questdownload <- ia_files(questitems) %>%
    filter(type == "txt") %>%
    ia_download(dir = "questfiles", extended_name = TRUE, overwrite = TRUE, silence = FALSE)

#okay ended up with two dataframes - one with the files and another with the metadata - I want to join them but will tidy the metadata dataframe first
questmetadata <- questmetadata %>%
    pivot_wider(
        names_from = field,
        values_from = value
    )

questmetadata <- questmetadata %>%
    select(id, title, volume, date, identifier, notes)

#join using the id 
questmdandfiles <- left_join(questmetadata, questdownload, by=c("id"))
#would probably want to make the volume and numbers separate columns

#okay so that worked but isn't right for the assignment I don't think - will redo with instructions from notes before getting into research questions

#first loop
df <- data.frame(id = character(), title=character(), volume = character())
#loop that will search based on parameters I give it and for each result it will print the ID (?) and add each result into a new row
internetarchivesearch <- ia_keyword_search("collection:questquarterly", num_results = 20)

for (i in 1:length(internetarchivesearch)) {
    result <- internetarchivesearch[[i]]
    df <- rbind(df, data.frame(
        id = result$id,
        title = result$title,
        volume = result$volume,
        date = resuilt$date,
        identifier = result$identifier,
        notes = resul$notes,
        stringsAsFactors = FALSE
    ))
    print(paste("adding", i, sep=" ")) 
}
