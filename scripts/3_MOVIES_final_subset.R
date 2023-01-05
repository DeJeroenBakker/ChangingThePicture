# Load libraries
library(tidyverse)
library(data.table)

#------------------------------------------------------------------------------------------------
# Make subset of the IMDb data set for films that appear in the Cornell data set 
#------------------------------------------------------------------------------------------------

# Load in the two datasets
Cornell <- fread("C:/Users/bge_j/Documents/UDS/Movie ecology/data/cornell_IMDB_BT.csv")
IMDb <- fread("C:/Users/bge_j/Documents/UDS/Movie ecology/data/imdb/movie_titles_metadata.csv")

# Prepare for merging the datasets
c_sub <- Cornell %>% select(C.Title, BT.id, C.FilmId)
c_sub <- c_sub %>% rename(primaryTitle = C.Title)

i_sub <- IMDb %>% select(primaryTitle, tconst)
i_sub$primaryTitle <- tolower(i_sub$primaryTitle) 

# Create a subset, remove duplicates
merged <- left_join(c_sub, i_sub, keep = T)
merged <- merged[!duplicated(merged$C.FilmId),]

fwrite(merged,"C:/Users/bge_j/Documents/UDS/Movie ecology/data/final_data/merged_na.csv", sep=";")

# There's some NAs introduced now with titles that are spelled differently on IMDb. Fix these manually in Excel and write to merged_na_fixed.csv.

# Load in the fixed CSV file
df <- fread("C:/Users/bge_j/Documents/UDS/Movie ecology/data/final_data/merged_na_fixed.csv")

#------------------------------------------------------------------------------------------------
# Get all the crew that has worked on films that appear in the Cornell/IMDb subset
#------------------------------------------------------------------------------------------------

# Start with the writers and directors
crew <- read.delim(file = "C:/Users/bge_j/Documents/UDS/Movie ecology/data/imdb/title.crew.tsv", sep = '\t', header = TRUE, fill = TRUE)
crew <- as.data.table(crew)

# Make subset of the crew data based on the titles in the Cornell-dataset
crew_sub <- crew[(tconst %in% df$tconst)]

# Unnest the directors column
crew1 <- crew_sub %>% select(tconst,directors)
crew1$directors <- gsub(pattern = "\\\\N", replacement = "", as.character(crew1$directors))
crew1 <- crew1 %>% rename(nconst = directors)
crew1 <- crew1[, .(nconst = unlist(tstrsplit(nconst, ",", type.convert = TRUE))), by = "tconst"]

# Unnest the writers column
crew2 <- crew_sub %>% select(tconst,writers)
crew2$writers <- gsub(pattern = "\\\\N", replacement = "", as.character(crew2$writers))
crew2 <- crew2 %>% rename(nconst = writers)
crew2 <- crew2[, .(nconst = unlist(tstrsplit(nconst, ",", type.convert = TRUE))), by = "tconst"]

# Merge the two data sets
crew_merged <- rbind(crew1,crew2) %>% unique()

############################################################

# Now do the same for the principals

principals <- read.delim(file = "C:/Users/bge_j/Documents/UDS/Movie ecology/data/imdb/title.principals.tsv", sep = '\t', header = TRUE, fill = TRUE)
principals <- as.data.table(principals) %>% select(tconst, nconst)

# Make subset of the crew data based on the titles in the Cornell-dataset
principals_sub <- principals[(tconst %in% df$tconst)] %>% unique()

# Merge with crew_merged
final_crew <- rbind(crew_merged, principals_sub) %>% unique()

# Let's write that to a CSV
fwrite(final_crew,"C:/Users/bge_j/Documents/UDS/Movie ecology/data/final_data/allcrew_tconst_nconst.csv", sep = ";")

#------------------------------------------------------------------------------------------------
# Now find all the films that these crew members were involved with.
#------------------------------------------------------------------------------------------------

final_crew <- fread("C:/Users/bge_j/Documents/UDS/Movie ecology/data/final_data/allcrew_tconst_nconst.csv")
crew <- read.delim(file = "C:/Users/bge_j/Documents/UDS/Movie ecology/data/imdb/title.crew.tsv", sep = '\t', header = TRUE, fill = TRUE)
crew <- as.data.table(crew)

# Unnest the directors column
crew1 <- crew %>% select(tconst,directors)
crew1$directors <- gsub(pattern = "\\\\N", replacement = "", as.character(crew1$directors))
crew1 <- crew1 %>% rename(nconst = directors)
crew1 <- crew1[, .(nconst = unlist(tstrsplit(nconst, ",", type.convert = TRUE))), by = "tconst"]

# Unnest the writers column
crew2 <- crew %>% select(tconst,writers)
crew2$writers <- gsub(pattern = "\\\\N", replacement = "", as.character(crew2$writers))
crew2$writers <- as.character(crew2$writers)
crew2 <- crew2 %>% rename(nconst = writers)
crew2 <- crew2[!(nconst == ""),]
crew2 <- crew2[, .(nconst = unlist(tstrsplit(nconst, ",", type.convert = TRUE))), by = "tconst"]

# Add the principals
principals <- read.delim(file = "C:/Users/bge_j/Documents/UDS/Movie ecology/data/imdb/title.principals.tsv", sep = '\t', header = TRUE, fill = TRUE)
principals <- as.data.table(principals) %>% select(tconst, nconst)

# ... And merge all the data sets
full_crew <- rbind(crew1, crew2, principals)

# Now, find all films that people involved with the Cornell films (final_crew) worked on
crew_expanded_titles <- full_crew[(nconst %in% final_crew$nconst)] %>% unique()

#------------------------------------------------------------------------------------------------
# Get all the people that worked on those films as well
#------------------------------------------------------------------------------------------------

df <- crew_expanded

# Start with the writers and directors
crew <- read.delim(file = "C:/Users/bge_j/Documents/UDS/Movie ecology/data/imdb/title.crew.tsv", sep = '\t', header = TRUE, fill = TRUE)
crew <- as.data.table(crew)

# Make subset of the crew data based on the titles in the Cornell-dataset
crew_sub <- crew[(tconst %in% df$tconst)]

# Unnest the directors column
crew1 <- crew_sub %>% select(tconst,directors)
crew1$directors <- gsub(pattern = "\\\\N", replacement = "", as.character(crew1$directors))
crew1 <- crew1 %>% rename(nconst = directors)
crew1 <- crew1[, .(nconst = unlist(tstrsplit(nconst, ",", type.convert = TRUE))), by = "tconst"]

# Unnest the writers column
crew2 <- crew_sub %>% select(tconst,writers)
crew2$writers <- gsub(pattern = "\\\\N", replacement = "", as.character(crew2$writers))
crew2 <- crew2 %>% rename(nconst = writers)
crew2 <- crew2[, .(nconst = unlist(tstrsplit(nconst, ",", type.convert = TRUE))), by = "tconst"]

# Merge the two data sets
crew_merged <- rbind(crew1,crew2) %>% unique()

#------------------------------------------------------------------------------------------------

# Now do the same for the principals

principals <- read.delim(file = "C:/Users/bge_j/Documents/UDS/Movie ecology/data/imdb/title.principals.tsv", sep = '\t', header = TRUE, fill = TRUE)
principals <- as.data.table(principals) %>% select(tconst, nconst)

# Make subset of the crew data based on the titles in the Cornell-dataset
principals_sub <- principals[(tconst %in% df$tconst)] %>% unique()

# Merge with crew_merged
crew_expanded <- rbind(crew_merged, principals_sub) %>% unique()

# Let's write that to a CSV
fwrite(crew_expanded,"C:/Users/bge_j/Documents/UDS/Movie ecology/data/final_data/allcrew_expanded_tconst_nconst.csv", sep = ";")

#------------------------------------------------------------------------------------------------

final_crew <- fread("C:/Users/bge_j/Documents/UDS/Movie ecology/data/final_data/allcrew_tconst_nconst.csv")
crew_expanded <- fread("C:/Users/bge_j/Documents/UDS/Movie ecology/data/final_data/allcrew_expanded_tconst_nconst.csv")

#------------------------------------------------------------------------------------------------

# Findings:
cat(paste0("\nIf we only look at the Cornell data set, we have a total of ", nrow(final_crew), " combinations of people and films. \n",
           "Total unique films: ", length(unique(final_crew$tconst)), 
           "\nTotal unique people: ", length(unique(final_crew$nconst)), "\n\n",
           "We expanded this by looking at all other films, resulting in ", nrow(crew_expanded), " combinations of people and films. \n", 
           "Total unique films: ", length(unique(crew_expanded$tconst)), 
           "\nTotal unique people: ", length(unique(crew_expanded$nconst)), "\n\n"
))


#------------------------------------------------------------------------------------------------
# Now we need to create a subset of all the films that the crew members in the crew_expanded set were involved with
#------------------------------------------------------------------------------------------------
setwd("C:/Users/bge_j/Documents/UDS/Movie ecology/data/imdb")

crew_expanded <- fread("C:/Users/bge_j/Documents/UDS/Movie ecology/data/final_data/allcrew_expanded_tconst_nconst.csv")
imdb_data <- fread("C:/Users/bge_j/Documents/UDS/Movie ecology/data/imdb/movie_titles_metadata.csv")
basics_raw <- read.delim(file = "title.basics.tsv", sep = '\t', header = TRUE, fill = TRUE) 
basics_raw <- basics_raw %>% as.data.table()

films_sub <- basics_raw[(tconst %in% crew_expanded$tconst)]

# Data selection and filtering
films_sub <- films_sub %>% select(tconst, titleType, primaryTitle, startYear, genres)
films_sub <- films_sub[!films_sub$startYear < 1922, ]
films_sub <- films_sub[!films_sub$startYear > 2022, ]
films_sub <- films_sub[!is.na(films_sub$startYear), ]
# Remove individual TV episodes (series are still included)
films_sub <- films_sub[!films_sub$titleType == "tvEpisode", ]

# Order by year for readability
films_sub <- films_sub[order(startYear),]

# Write that to a CSV
fwrite(films_sub, "C:/Users/bge_j/Documents/UDS/Movie ecology/data/final_data/expanded_crew_moviesmetadata.csv", sep=";")


#------------------------------------------------------------------------------------------------
# Return to the expanded crew file and use the subset created above to filter out TV episodes and older films
#------------------------------------------------------------------------------------------------
crew_expanded <- fread("C:/Users/bge_j/Documents/UDS/Movie ecology/data/final_data/allcrew_expanded_tconst_nconst.csv")
films_sub <- fread("C:/Users/bge_j/Documents/UDS/Movie ecology/data/final_data/expanded_crew_moviesmetadata.csv")

crew_final <- crew_expanded[(tconst %in% films_sub$tconst),]
crew_final <- crew_final[order(tconst),]

fwrite(crew_final, "C:/Users/bge_j/Documents/UDS/Movie ecology/data/final_data/final_expanded_crew_moviesmetadata.csv", sep=";")

