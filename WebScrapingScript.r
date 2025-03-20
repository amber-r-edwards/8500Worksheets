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

library(internetarchive)
library(tidyverse)
#first loop
df <- data.frame(id = character(), title=character(), volume = character(), date = character(), identifier = character(), notes = character (), stringsAsFactors = FALSE )
#loop that will search based on parameters I give it and for each result it will print the ID (?) and add each result into a new row
internetarchivesearch <- ia_keyword_search("collection:questquarterly", num_results = 20)
metadata <- ia_get_items(internetarchivesearch)
result <- ia_metadata(metadata)

for (seq_along(internetarchivesearch)) {
    metadata <- ia_get_items(internetarchivesearch)
    print(paste("gathered metadata for", length(metadata), "items.", sep = " ")) 
}

for (seq_along(metadata)) {
    result <- ia_metadata(metadata)
        df <- result %>%
            pivot_wider(
                names_from = field,
                values_from = value
            )
    print(paste("added metadata for", length(result), "items.", sep= " "))
}
