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

#set up search parameters
internetarchivesearch <- ia_keyword_search("collection:questquarterly", num_results = 20)

#using results from search - gather metadata for each result item
metadata_list <- list()
for (i in seq_along(internetarchivesearch)) {
    metadata <- ia_get_items(internetarchivesearch[i])
    metadata_list <- append(metadata_list, metadata)
    print(paste("gathered metadata for", length(metadata), "items.", sep = " ")) 
}

#access metadata and insert into dataframe
result <- ia_metadata(metadata_list)
print(paste("added metadata for", length(metadata_list), "items.", sep= " "))

#tidy dataframe - select information that will be helpful for research question
questmetadata <- result %>%
    pivot_wider(
        names_from = field,
        values_from = value
    ) 

questmetadata <- questmetadata %>%
    select(id, title, volume, date, notes)

#go through each row and get issue/download text files - inserted into dataframe in association with metadata
for (i in questmetadata) {
    questmetadata <- ia_files(metadata_list[i]) %>%
        filter(type == "txt") %>%
        ia_download(dir = "questfiles", extended_name = TRUE, overwrite = TRUE, silence = FALSE)
}
