#------------------------------------------------------------------------------------------------
# DATA ANALYSES FOR CHANGING THE PICTURE
# Jeroen Bakker, Utrecht University/Radboud University
#------------------------------------------------------------------------------------------------
# load libraries
setwd("C:/Users/bge_j/Documents/UDS/Movie ecology/data/imdb")

require(tidyverse)
require(data.table)
require(dplyr)
require(scales)

# data loading 
titles <- read.delim(file = "title.akas.tsv", sep = '\t', header = TRUE, fill = TRUE) # all titles
basics_raw <- read.delim(file = "title.basics.tsv", sep = '\t', header = TRUE, fill = TRUE) # basic info belonging to those titles
crew <- read.table(file = "title.principals.tsv", sep = '\t', header = TRUE, fill = TRUE) # crew associated with titles
ratings <- read.delim(file = "title.ratings.tsv", sep = '\t', header = TRUE, fill = TRUE) # ratings of titles
names <- read.delim(file = "name.basics.tsv", sep = '\t', header = TRUE, fill = TRUE) # ratings of titles

# faster data processing with DT
titles <- data.table(titles)
basics <- data.table(basics_raw)
crew <- data.table(crew)
ratings <- data.table(ratings)

# loading cleaned basics
basics <- fread("movie_titles_metadata.csv")
basics <- as.data.table(basics)
basics <- basics[order(startYear),]
basics$index <- 1:nrow(basics)
basics <- fwrite(basics,"movie_titles_metadata.csv", sep = ";")


#------------------------------------------------------------------------------------------------
# FILTERING DATA
#------------------------------------------------------------------------------------------------
# remove older and newer movies
basics$startYear <- as.numeric(basics$startYear)
basics <- basics[!basics$startYear < 1922, ]
basics <- basics[!basics$startYear > 2022, ]
basics <- basics[!is.na(basics$startYear), ]

basics <- basics[basics$endYear ==  "\\N", ] # no end year so no series
basics <- basics[!is.na(basics$startYear), ]

# remove separate tv episodes - these don't have an endYear
`%notin%` <- function(x,y) !(x %in% y) 
basics <- basics[!basics$titleType %notin% c("tvEpisode","tvShort","tvMiniSeries","tvSpecial","videoGame","tvSeries"), ]

# create a quick subset 
fwrite(basics,"basics_subset.csv",sep=";")
basics <- fread("basics_subset.csv")

# merge with ratings data sets
basics <- left_join(basics, ratings, by = c("tconst" = "tconst"))

# write to CSV file
fwrite(basics, "movie_titles_metadata2.csv")


#------------------------------------------------------------------------------------------------
# PLOT THE AMOUNT OF MOVIES RELEASED EACH YEAR
#------------------------------------------------------------------------------------------------
basics <- fread("movie_titles_metadata2.csv")

# count films released per year
dfplot <- basics %>% 
  group_by(startYear) %>% 
  count()

# plot the histogram
dfplot %>%
  ggplot(aes(x=startYear,
             y=n,
             fill=n)) +
  theme(legend.position="none") +
  ggtitle(paste0("Number of yearly releases (n = ", nrow(basics),")")) +
  scale_fill_gradient(low="darkgoldenrod3", high="red") +
  scale_x_continuous(limits = c(1921,2023), breaks = seq(1922, 2022, 10)) +
  scale_y_continuous(limits = c(0,max(dfplot$n)), breaks = seq(0, 100000, 10000), labels = label_comma()) +
  geom_bar(stat="identity") +
  xlab("Year") +
  ylab("")

# write to JPG file
ggsave("C:/Users/bge_j/Documents/UDS/Movie ecology/vis/yearly_releases2.jpg", width = 7, height = 7)

#------------------------------------------------------------------------------------------------
# CALCULATE PREVIOUS GENRE OCCURENCES (NOT USEFUL + TAKES VERY LONG)
#------------------------------------------------------------------------------------------------

# Create the master table that the subset will be checked against
df_master <- genre %>% as.data.table()
#df_master <- df_master[!genres == "\\N"]

# Load in the subset
df <- fread("C:/Users/bge_j/Documents/UDS/Movie ecology/data/final_data/expanded_crew_moviesmetadata.csv")

# create an empty vector to store the results
results <- c()

check_genres_appeared <- function(i) {
  print(paste(i,"/",nrow(df),"/",i/nrow(df)*100,"%"))
  
  # match the current film with its counterpart in the master table and get its row number
  x <- as.numeric(which(df_master$tconst == df$tconst[[i]]))
  
  # get the current row's genres and start year
  current_genres <- df$genres[i]
  current_year <- as.numeric(df$startYear[i])
  
  # split the genres into a vector
  current_genres_vector <- unlist(strsplit(current_genres, ","))
  
  # create a logical vector indicating whether each genre has appeared in an earlier row
  genre_appeared <- rep(FALSE, length(current_genres_vector))
  
  if (!x == 1) {
    for(j in 2:x-1) {
      # only consider rows with an earlier start year
      if (as.numeric(df_master$startYear[j]) < current_year) {
        # split the genres of the earlier row into a vector
        earlier_genres_vector <- unlist(strsplit(df_master$genres[j], ","))
        
        # check if any of the current genres appeared in the earlier row
        genre_appeared <- genre_appeared | (match(current_genres_vector, earlier_genres_vector) > 0)
      }
    }
  }
  
  # check if all the genres appeared in an earlier row
  all_appeared <- all(genre_appeared)
  print(all_appeared)
  # append the results to the results vector
  results <- c(results, all_appeared)
}

# run the function on the data set
results <- sapply(1:nrow(df), check_genres_appeared)

# append results to the full data set, rename values for more readability
df_final <- df
df_final$new_genre_combination <- results
df_final <- within(df_final, new_genre_combination[new_genre_combination == "TRUE"] <- "NO")
df_final <- within(df_final, new_genre_combination[new_genre_combination == "FALSE"] <- "YES")

# write to file
fwrite(df_final, "new_genre_combinations.csv", sep = ";")
df_final <- fread("new_genre_combinations.csv")

##### Plot the findings
dfplot <- df_final %>% select(startYear, new_genre_combination) %>%
  group_by(startYear) %>%
  count(new_genre_combination)

dfplot <- dfplot[which(dfplot$new_genre_combination == "YES"),]

dfplot %>%
  ggplot(aes(x=startYear,
             y=n,
             fill=n)) +
  theme(legend.position="none") +
  ggtitle("Number of new film genre combinations") +
  scale_fill_gradient(low="darkgoldenrod3", high="red") +
  scale_x_continuous(limits = c(1921,2023), breaks = seq(1922, 2022, 10)) +
  scale_y_continuous(limits = c(0,85), breaks = seq(0, 85, 10)) +
  geom_bar(stat="identity") +
  xlab("Year") +
  ylab("")

ggsave("C:/Users/bge_j/Documents/UDS/Movie ecology/vis/new_genre_combinations.jpg", width = 7, height = 7)

#------------------------------------------------------------------------------------------------
# CREW COUNT AND MALE/FEMALE PERCENTAGE
#------------------------------------------------------------------------------------------------

# create a data table containing information of crew members for each film
crewcount <- crew %>% select(tconst, nconst) %>% as.data.table()
crewcount <- left_join(crewcount, names, by = "nconst")
crewcount <- crewcount %>% select(tconst, nconst, primaryName)
crewcount <- left_join(crewcount, names, by = "nconst")
crewcount$firstName <- sub(" .*","",crewcount$primaryName)
crewcount <- crewcount[tconst %in% basics$tconst]

# write to file
fwrite(crewcount, "crewcount2.csv", sep = ";", row.names = F)
crewcount <- fread("crewcount2.csv")

# guess gender using genderize.io  
# load libraries
library(htmltools)
library(jsonlite)
library(httr)
library(plyr)

# only keep unique first names
firstNames <- crewcount$firstName %>% unique() %>% as.data.table()
colnames(firstNames)[1] <- "firstName"

# remove periods, otherwise the URLs will not work
firstNames$firstNameCleaned <- firstNames$firstName %>%
  str_replace_all("\\.","")

# make API call URLs for each name
URL_pattern <- "https://api.genderize.io?name="
API_key <- "&apikey=9fdb7b0311a9453cd77f8360b8e07ef8"
firstNames$url <- paste0(URL_pattern,firstNames$firstNameCleaned,API_key)

# set up empty data.frames for the for loop
dat <- data.frame()
output <- data.frame()

# make API calls to genderize.io for the names
for (x in firstNames$url)  { 
  x %>% print
  
  if (as.character(firstNames[which(url==x),firstName]) %>% head(1) %in% dat$name) {print(paste0("Skipping over ",as.character(firstNames[which(url==x),firstName])," because name already exists in dat."))
    next }
  
  text <- content(GET(x),type="text", encoding = "UTF-8")
  text <- sub("var statesData = ","",text)
  text <- sub(";$","",text)
  
  output <- fromJSON(text)
  if (is.null(output$gender)) {output$gender <- "unknown"}
  
  output <- output %>%
    as.data.frame() %>%
    head(1)
  
  dat <- rbind.fill(dat,output) 
}

# write results to CSV file
fwrite(dat, "firstNamesGenders.csv", sep = ";")
dat <- fread("firstNamesGenders.csv") %>% as.data.table()
dat <- dat %>% dplyr::rename(firstName = name)

# merge the results with the original crewcount data set
df_merged <- left_join(crewcount, dat) %>% unique() 

# tag names starting with 'Mrs.' (seemingly common in early films) with the correct metadata 
df_merged$gender[df_merged$firstName == "Mrs."] <- "female"
df_merged$probability[df_merged$firstName == "Mrs."] <- "1"
df_merged$count[df_merged$firstName == "Mrs."] <- "0"

# remove columns that aren't needed anymore
df_merged <- df_merged %>% select(-firstName, -count)

# calculate the percentage of female and male crew members for each film
df_merged_counts <- df_merged %>% group_by(tconst) %>% dplyr::count(gender) %>% as.data.table()
df_merged_counts_totals <- as.data.table(dcast(df_merged_counts, tconst~gender, fill=0)) 
df_merged_counts_totals <- df_merged_counts_totals %>% dplyr::rename(NA2 = unknown)
df_merged_counts_totals["NA"] <- as.numeric(as.character(df_merged_counts_totals["NA"]))
df_merged_counts_totals["NA2"] <- as.numeric(as.character(df_merged_counts_totals["NA2"]))
df_merged_counts_totals$unknown <- df_merged_counts_totals[,"NA"] + df_merged_counts_totals[,"NA2"]
df_merged_counts_totals <- df_merged_counts_totals %>% select(-"NA",-NA2)

df_merged_counts_totals$percentage_female <- (df_merged_counts_totals$female)/(df_merged_counts_totals$female+df_merged_counts_totals$male+df_merged_counts_totals$unknown)
df_gendertotals <- df_merged_counts_totals %>% select(-female,-male,-unknown)

# write to CSV file
fwrite(df_gendertotals, "crewgenders.csv", sep = ";")
df_gendertotals <- fread("crewgenders.csv")

# make a subset of only the films that 
df_gendersub <- df_gendertotals[tconst %in% basics$tconst]
fwrite(df_gendersub, "crewgenders_sub2.csv", sep = ";")

##### plot percentage of female crew members over time
library(reshape2)

df <- fread("crewgenders_sub2.csv")
df_filmyear <- basics %>% select(tconst,startYear)

# note that the amount of films will now be smaller (about 300,000 less) because not all films have crew data associated with them
df_merged <- left_join(df_filmyear,df)
df_merged <- df_merged[complete.cases(df_merged),]
df_merged <- df_merged[order(startYear)]

# calculate the yearly means
df_nominal <- aggregate(df_merged$percentage_female, list(df_merged$startYear), mean) %>%
  dplyr::rename(startYear = "Group.1", percentage_female = x)
df_nominal$percentage_male <- 1-df_nominal$percentage_female

# prepare for plot
dfm <- melt(df_nominal[,c('startYear','percentage_female', 'percentage_male')],id.vars=1)

# plot the percentages over the years
dfm %>%
  ggplot(aes(x = startYear,
             y = value*100)) +
  geom_bar(aes(fill = variable), stat = "identity", position = position_stack(reverse = T), width = 1) +
  scale_fill_manual(values = c(alpha("darkred",1), alpha("deepskyblue2",0.5)), labels = c("Female", "Male")) +
  scale_y_continuous(limits = c(0,101), breaks = seq(0, 100, 25)) +
  scale_x_continuous(limits = c(1921,2023), breaks = seq(1922, 2022, 10)) +
  ggtitle("Average gender subdivision of film crew members (n = 1,692,644)") +
  xlab("Year") + ylab("%") +
  labs(fill= "Gender")

# write to a JPG file
ggsave("C:/Users/bge_j/Documents/UDS/Movie ecology/vis/femalecrew.jpg", height = 7, width = 8.5)


#------------------------------------------------------------------------------------------------
# COUNT AMOUNT OF CREW MEMBERS PER FILM
#------------------------------------------------------------------------------------------------
df_crew_n <- crewcount$tconst %>% as.data.table() 
colnames(df_crew_n)[1] = "tconst"
df_crew_n <- df_crew_n %>% group_by(tconst) %>% dplyr::count(tconst)
df_crew_n <- df_crew_n[grepl("tt*",df_crew_n$tconst),]

max(df_crew_n$n) # maximum amount of crew members for a movie is 10, so MANY people missing in the data

# write to CSV file
fwrite(df_crew_n, "crew_n2.csv", sep=";")


#------------------------------------------------------------------------------------------------
# CALCULATE PREVIOUS SUCCESS OF DIRECTOR
# Note that many films will not show their director, even though they are listed in the IMDb database.
# Turns out this has to do with the limit of 10 crew members that the API provides. Sometimes these 10
# crew members do not include the director. 31959 out of 58444 films will not list directors.
#------------------------------------------------------------------------------------------------
setwd("C:/Users/bge_j/Documents/UDS/Movie ecology/data/imdb")

# prepare the dataset
basics <- fread("movie_titles_metadata2.csv") %>% select(-averageRating.y, - numVotes.y) %>% rename("averageRating" = averageRating.x, "numVotes" = numVotes.x)
prev_success <- basics %>% select(tconst, startYear, averageRating)

# make subset of crew members who are categorized as directors and write that to a file
directors <- crew %>%
  select(tconst, nconst, category)
directors <- directors[grepl("director",directors$category),]
fwrite(directors, "directors.csv", sep=";")
directors <- fread("directors.csv")

# merge data sets, only keep rows that have a rating
df <- left_join(directors, prev_success)
df <- df[complete.cases(averageRating),]
df <- df[tconst %in% prev_success$tconst]
fwrite(df, "previous_success_directors.csv", sep=";")
df <- fread("previous_success_directors.csv")

# prepare for the for loop calculating the mean rating of all previous films for each director
dat <- df
dat$index <- 1:nrow(dat)
output <- data.table()
temp3 <- data.table()

# excute the for loop
for (i in dat$index) {
  year <- dat$startYear[which(dat$index == i)] %>% as.numeric()
  name <- dat$nconst[which(dat$index == i)] %>% as.character()
  print(paste(name,year))
  temp <- dat[which(dat$startYear < year),] 
  temp2 <- temp[which(temp$nconst == name),]
  if (nrow(temp2) < 1) {
    print(paste0("Skipping over ", name, " because there's no earlier films for this director."))
    next}
  temp3 <- data.table()
  temp3$prevmean <- mean(temp2$averageRating)
  temp3$tconst <- dat$tconst[[i]]
  temp3
  
  output <- rbind(output,temp3)
}

# write results to CSV file
fwrite(output, "avg_rating.csv", sep=';')
output <- fread("avg_rating.csv")

# merge results with the full data set
df_merged <- left_join(dat,prev_success)
df_merged <- left_join(df_merged, output) %>% rename(prevMeanRating = prevmean)
df_merged <- df_merged[order(startYear),]

# write to CSV file as well
fwrite(df_merged, "previous_mean_ratings2.csv", sep=';')

#------------------------------------------------------------------------------------------------
# COUNT AMOUNT OF PREVIOUS FILMS FOR DIRECTOR
#------------------------------------------------------------------------------------------------
setwd("C:/Users/bge_j/Documents/UDS/Movie ecology/data/imdb")

# prepare the dataset
films_sub <- fread("C:/Users/bge_j/Documents/UDS/Movie ecology/data/final_data_complete/moviesmetadata.csv")
directors <- fread("directors.csv")

directors <- left_join(films_sub, directors)
directors <- directors %>% select(tconst, nconst, startYear)

# for loop to count amount of films made by director before startYear
directors$directorPreviousFilmsCount <- integer()

directors <- as.data.table(directors)
# iterate through each row of the data table
for (i in 1:nrow(directors)) {
  # select rows with a startYear less than the current row, and the same nconst value
  subset <- directors[directors$startYear < directors[i,]$startYear & directors$nconst == directors[i,]$nconst,]
  
  # count the number of rows in the subset
  count <- nrow(subset) %>% as.integer()
  
  # write the count to the directorPreviousFilmsCount column for the current row
  directors[i,]$directorPreviousFilmsCount <- count
  print(directors[i,])
}

# write to CSV file as well
fwrite(directors, "director_previous_films_count.csv", sep=';')

directors <- fread("director_previous_films_count.csv")
directors <- left_join(films_sub, directors)

fwrite(directors, "director_previous_films_count2.csv", sep=';')
