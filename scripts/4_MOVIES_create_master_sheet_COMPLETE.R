library(data.table)
library(tidyverse)

#################################################################
# Create a master sheet containing all the data and analyses
#################################################################

### Load in all the data sets

# subset of the films that the expanded crew worked on
films_sub <- fread("C:/Users/bge_j/Documents/UDS/Movie ecology/data/final_data_complete/moviesmetadata.csv")
# mean rating of films made by the director before release of each film 
previous_mean_ratings <- fread("C:/Users/bge_j/Documents/UDS/Movie ecology/data/imdb/previous_mean_ratings2.csv")
# counts of films directed by director before each film
director_previous_films_count <- fread("C:/Users/bge_j/Documents/UDS/Movie ecology/data/imdb/director_previous_films_count2.csv")
# amount of crew members for each film, as registered in the IMDb data set (maximum of 10)
crew_n <- fread("C:/Users/bge_j/Documents/UDS/Movie ecology/data/imdb/crew_n2.csv")
# gender subdivision of the crew working on each film
crewgenders <- fread("C:/Users/bge_j/Documents/UDS/Movie ecology/data/imdb/crewgenders_sub2.csv")
# Cornell data set with added Bechdel Test data
cornell <- fread("C:/Users/bge_j/Documents/UDS/Movie ecology/data/cornell_IMDB_BT_networkstats_chargender_dirgender_fixed_manualBT.csv")
cornell <- cornell %>% rename(tconst = imdbID)

# Merge them into one master sheet
df_final <- data.table()
#df_final <- left_join(films_sub, new_genre_combinations)
df_final <- left_join(films_sub, previous_mean_ratings)
df_final <- left_join(df_final, director_previous_films_count)
df_final <- left_join(df_final, crew_n)
df_final <- left_join(df_final, crewgenders)
df_final <- left_join(df_final, cornell)
df_final <- unique(df_final) %>% as.data.table()

#------------------------------------------------------------------
# RENAME COLUMNS FOR READABILITY, DESELECT IRRELEVANT COLUMNS
#------------------------------------------------------------------
df_final <- df_final %>% select(-category, -index, -imdbRating, 
                                -Type, -movie_id, -C.Title,
                                -Year, -I.Title, -Genre,
                                - Director, -Poster, -imdbRating,
                                -imdbVotes, -Type)

df_final <- df_final %>% rename(imdbRating = averageRating, 
                                crewCount = n,
                                percentageFemaleCrew = percentage_female,
                                releaseYear = startYear,
                                nconst_director = nconst,
                                meanPreviousIMDbRatingDirector = prevMeanRating) 

df_final$genres <- na_if(df_final$genres,"\\N")


#------------------------------------------------------------------
# WRITE THE FINAL DATA SET TO A CSV FILE
#------------------------------------------------------------------

fwrite(df_final, "C:/Users/bge_j/Documents/UDS/Movie ecology/data/FINAL_RESULTS/ChangingThePicture_mastersheet2.csv", sep=";")

#------------------------------------------------------------------
# FIX THE BOX OFFICE FORMATTING
#------------------------------------------------------------------

df_final <- fread("C:/Users/bge_j/Documents/UDS/Movie ecology/data/FINAL_RESULTS/ChangingThePicture_mastersheet2.csv")

df_final$BoxOffice <- gsub("[^0-9]", "", df_final$BoxOffice) %>% as.numeric()

fwrite(df_final,"C:/Users/bge_j/Documents/UDS/Movie ecology/data/FINAL_RESULTS/ChangingThePicture_mastersheet2.csv")

