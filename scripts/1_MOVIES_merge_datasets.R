library(tidyverse)
library(htmltools)
library(reshape2)

#------------------------------------------------------------------
### Make list of OMDBAPI URLs based on Cornell database
#------------------------------------------------------------------
setwd("C:/Users/bge_j/Documents/UDS/Movie ecology/data/cornell")

df <- read.table("movie_titles_metadata.txt",sep = ";", header = FALSE)
df <- fread("csv/movie_titles_metadata.csv")
df <- mutate(df, ID = row_number())

URL_pattern <- "https://www.omdbapi.com/?t="
API_key <- "&apikey=7b5acdde"

url_df <- paste0(URL_pattern,df$Title,API_key)
url_df <- as.data.frame(url_df) %>% tibble::rowid_to_column("ID")

df <- left_join(df,url_df)
df <- df %>%
  select(ID,everything())

#------------------------------------------------------------------
### Go through OMDBAPI URLs, read JSONs and bind to DF
#------------------------------------------------------------------
library(jsonlite)
library(httr)

dat <- data.frame()
output <- data.frame()

for (x in df$url)  { 
  x %>% print
  text <- content(GET(x),type="text", encoding = "UTF-8")
  text <- sub("var statesData = ","",text)
  text <- sub(";$","",text)
  output <- fromJSON(text) %>% 
    as.data.frame() %>%
    head(1) 
  dat <- plyr::rbind.fill(dat,output)
}

dat <- dat %>% tibble::rowid_to_column("ID")

#------------------------------------------------------------------
### Merge df and dat
#------------------------------------------------------------------
df_dat <- left_join(df,dat,by="ID")

#------------------------------------------------------------------
### Add Bechdel test data through API using the IMDB ID 
#------------------------------------------------------------------
BTurl <- "https://bechdeltest.com/api/v1/getMovieByImdbId?imdbid="

dat2 <- data.frame()
output2 <- data.frame()

dat$BTapi <- dat$imdbID %>% 
  str_replace_all("tt","")
BTapiURL <- paste0(BTurl,dat$BTapi)

dat2 <- map_df(BTapiURL, ~{
  r <- GET(.x)
  print(r)
  s <- content(r, as = "text", encoding = "UTF-8")
  bind_rows(fromJSON(s), .id = 'id')
}, .id = 'index')

dat2 <- dat2 %>% tibble::rowid_to_column("ID") 

#------------------------------------------------------------------
### Merge df_dat and dat2, select relevant rows and rename to reflect sources
#------------------------------------------------------------------
df_dat2 <- left_join(df_dat,dat2,by="ID")
df_final <- df_dat2 %>% 
  select(-Genre.x, -Score, -NoVotes, -url_df, -Year.y, -Released, -Runtime, -Ratings.Source, -DVD, -Production, -Website,
         -Response, -Error, -index, -year, -visible, -date, -submitterid, -title, -imdbid, -version, -status, -description, -Ratings.Value, -totalSeasons) %>%
  rename(C.FilmId = ?..FilmId, C.Title = Title.x, Year = Year.x, I.Title = Title.y, Rated = Rated, Genre = Genre.y, BT.id = id, BT.score = rating, BT.dubious = dubious)

write.csv2(df_final, "C:/Users/bge_j/Documents/UDS/Movie ecology/data/cornell_IMDB_BT.csv", na="N/A", row.names = F)

#------------------------------------------------------------------
### Merge cornell_IMDB_BT.csv with network_stats.csv
#------------------------------------------------------------------
setwd("C:/Users/bge_j/Documents/UDS/Movie ecology/data")

df_CIB <- fread("cornell_IMDB_BT.csv")
df_CIB <- df_CIB %>% rename(movie_id = C.FilmId)
df_netstats <- fread("networkstats/networkstats.csv")

df_merged <- left_join(df_CIB,df_netstats,by="movie_id") %>% select(-ID)

fwrite(df_merged, "cornell_IMDB_BT_networkstats.csv", sep=";")

#------------------------------------------------------------------
### Calculate totals of gender of characters per movie, add to columns, append to rest of data set
###     Note: the number of unknowns in the networkstats file makes these results unreliable. 
#------------------------------------------------------------------
df <- fread("cornell_IMDB_BT_networkstats.csv")
df_chars <- fread("networkstats/character_rankings_1.csv")
df_chars$char_gender <- tolower(df_chars$char_gender) 

df_chars %>% group_by(movie_id) %>% count(char_gender)
df_gendertotals <- as.data.table(dcast(df_chars, movie_id~char_gender, fill=0))

df_char_gendertotals <- left_join(df, df_gendertotals) %>% rename(unknown = "?", female = f, male = m)
df_char_gendertotals$char_percentage_female <- (df_char_gendertotals$female)/(df_char_gendertotals$female+df_char_gendertotals$male+df_char_gendertotals$unknown)
df_char_gendertotals <- df_char_gendertotals %>% select(-female,-male,-unknown)

fwrite(df_char_gendertotals, "cornell_IMDB_BT_networkstats_chargender.csv", sep=";")

#------------------------------------------------------------------
### Guess gender for directors using genderize.io, calculate percentage of female directors, append to file
#------------------------------------------------------------------
library(jsonlite)
library(httr)

setwd("C:/Users/bge_j/Documents/UDS/Movie ecology/data/")

df <- fread("cornell_IMDB_BT_networkstats_chargender.csv")

df_directors <- as.data.frame(df$Director) %>% rename(director = "df$Director") %>% separate_rows(director, sep = ", ") %>% unique()
df_directors$firstname <- str_replace_all(df_directors$director," .*","")
df_directors <- df_directors[df_directors$firstname != "N/A",]
df_directors <- df_directors[!grepl("*\\.$|[[:punct:]]",df_directors$firstname),] #%>% mutate_all(funs(gsub("[[:punct:]]", "", .)))

URL_pattern <- "https://api.genderize.io?name="

df_directors$url <- paste0(URL_pattern,df_directors$firstname)

dat <- data.frame()
output <- data.frame()

for (x in df_directors$url)  { 
  x %>% print
  text <- content(GET(x),type="text", encoding = "UTF-8")
  text <- sub("var statesData = ","",text)
  text <- sub(";$","",text)
  output <- fromJSON(text) %>% 
    as.data.frame() %>%
    head(1) 
  dat <- rbind.fill(dat,output)
}

dat <- dat %>% tibble::rowid_to_column("ID") %>% rename(firstname = name)

df_merged <- left_join(df_directors, dat) %>% select(-firstname, -ID, -count) %>% rename(dir_gender = gender, dir_gender_probability = probability, Director = director)

df_director_genders <- df %>% select(movie_id, Director) %>% separate_rows(Director, sep = ", ")
df_director_genders <- left_join(df_director_genders, df_merged)
df_director_genders %>% group_by(movie_id) %>% count(dir_gender)
df_director_gendertotals <- as.data.table(dcast(df_director_genders, movie_id~dir_gender, fill=0)) %>% rename(unknown = "NA")
df_director_gendertotals$dir_percentage_female <- (df_director_gendertotals$female)/(df_director_gendertotals$female+df_director_gendertotals$male+df_director_gendertotals$unknown)
df_director_gendertotals <- df_director_gendertotals %>% select(-female,-male,-unknown)

df_final <- left_join(df, df_director_gendertotals)
fwrite(df_final, "cornell_IMDB_BT_networkstats_chargender_dirgender.csv")


#------------------------------------------------------------------
### Merge with fixed networkstats_correct.csv file that Roel generated 
#------------------------------------------------------------------
df <- fread("cornell_IMDB_BT_networkstats_chargender_dirgender.csv")
df <- df %>% select(-gender_assortativity, -nr_nodes, -nr_edges, 
                    -density, -triad_closure, -clustering_coef,
                    -is_connected)
df2 <- fread("networkstats_correct.csv")

df3 <- left_join(df, df2)

fwrite(df3, "cornell_IMDB_BT_networkstats_chargender_dirgender_fixed.csv", sep=";")


#------------------------------------------------------------------
### Create summaries of the data set
#------------------------------------------------------------------
df <- fread("cornell_IMDB_BT_networkstats_chargender_dirgender.csv")

# How many movies have at least one female director?
length(which(df$dir_percentage_female > 0))

# How many movies have a predominantly female cast? 
### NOTE: the amount of unknowns in char data set means this is unreliable
length(which(df$char_percentage_female > 0.5))

#------------------------------------------------------------------
### Create edges file for every movie
#------------------------------------------------------------------
require(splitstackshape)
require(dedupewider)
df <- fread("cornell_IMDB_BT_networkstats_chargender_dirgender.csv")

df_network <- df %>% select(Director,Writer,Actors) 
df_network <- cSplit(df_network, c("Actors","Director","Writer"), ", ")

fwrite(df_final,"gephi_edges.csv",col.names = FALSE, sep=";")

#------------------------------------------------------------------
### Guess gender for characters using genderize.io 
#------------------------------------------------------------------
df <- fread("networkstats/character_rankings_1.csv")

df_chars <- as.data.frame(df$char_name) %>% rename(char = "df$char_name") %>% separate_rows(char, sep = ", ") %>% unique()
df_chars$firstname <- str_replace_all(df_chars$char,"THE ","")
df_chars$firstname <- str_replace_all(df_chars$firstname," .*","")
df_chars <- df_chars[df_chars$firstname != "N/A",]
df_chars <- df_chars[!grepl("*\\.$|[[:punct:]]",df_chars$firstname),] #%>% mutate_all(funs(gsub("[[:punct:]]", "", .)))
df_chars$firstname <- str_replace_all(df_chars$firstname,"\t","")

Genderize_API_key <- "9fdb7b0311a9453cd77f8360b8e07ef8"
URL_pattern <- "https://api.genderize.io?name="

df_chars$url <- paste0(URL_pattern,df_chars$firstname,"&apikey=",Genderize_API_key) %>% str_replace_all("\t","")

dat <- data.frame()
output <- data.frame()

for (x in df_chars$url)  { 
  x %>% print
  text <- content(GET(x),type="text", encoding = "UTF-8")
  text <- sub("var statesData = ","",text)
  text <- sub(";$","",text)
  output <- fromJSON(text) %>% 
    as.data.table() %>%
    head(1) %>%
    print()
  dat <- rbind.fill(dat,output)
}

dat <- dat %>% rename(firstname = name)

df_merged <- left_join(df_chars, dat) %>% select(-firstname, -count, -error, -url) %>% rename(char_gender_NEW = gender, char_gender_NEW_probability = probability, char_name = char) %>% unique()
df_merged$char_gender_NEW[grepl("WOMAN|LADY", df_merged$char_name)] <- "female"

df_final <- left_join(df, df_merged, by = "char_name")

fwrite(df_final,"networkstats/character_rankings_2.csv",sep=";")


#------------------------------------------------------------------------------------------------
# CHECK FOR BECHDEL TEST ZERO SCORES USING THE CHAR_PERCENTAGE_FEMALE COLUMN, REPLACE BT.score WITH 0 IF PERCENTAGE IS ZERO
#------------------------------------------------------------------------------------------------
library(data.table)

setwd("C:/Users/bge_j/Documents/UDS/Movie ecology/data")
df <- fread("cornell_IMDB_BT_networkstats_chargender_dirgender_fixed.csv")

# if the percentage of female characters is zero, change BT.score (Bechdel Test Score) to 0
df <- within(df, BT.score[char_percentage_female == '0'] <- '0')

fwrite(df, "C:/Users/bge_j/Documents/UDS/Movie ecology/data/cornell_IMDB_BT_networkstats_chargender_dirgender_fixed_manualBT.csv")
