---
title: "stopping_tables"
author: "A.M. Mulders"
date: '2022-12-10'
output: html_document
---
  

#staging the script
rm(list = ls())

# custom function to check for packages / installs
fpackage.check <- function(packages) {
  lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
}

# declare packages
packages = c("tidyverse", "statnet", "igraph", "reshape2")


# load / install pacakges
fpackage.check(packages)

# read the data
df <- read.csv("data/final_results/ChangingThePicture_mastersheet.csv", sep = ";")

x <- data.frame(table(df$tconst))

movcre <- read.csv("data/final_results/movies_crew_network.csv", sep = ";")

# tconst == movie
# nconst == actors

# some descriptives
length(unique(movcre$nconst)) # --> 3197 actors cornell datbase?
length(unique(movcre$tconst)) # --> that spread out over 50k movies in imdb?

# what we want is in each movie-year combination a network for whether those folks worked together

# let's start with making an object for each year?
year_vec <- sort(unique(movcre$year))
max(year_vec)-min(year_vec)+1 == length(year_vec) # all years represented 1922-2022
movie_years <- unique(movcre[, c("tconst", "year")])

# then for each movie
movie_vec <- unique(movcre$tconst)

movid_list <- list()
movie_mat <- list()
movie_edges <- list()
dims <- list()
l <- 0
for (i in 1:length(movie_vec)) {
  
  movid_list[[i]] <- movcre[movcre$tconst == movie_vec[i], ] # so for each year movie?

  l[i] <- length(movid_list[[i]][["nconst"]]) #actors as matrix size
  dims[[i]] <- movid_list[[i]][["nconst"]] # dimensions is numer of actors

  movie_mat[[i]] <-  matrix(rep(1, l[i]^2), nrow = l[i], ncol = l[i], byrow= TRUE, dimnames = list(dims[[i]], dims[[i]])) # input in matrix

  movie_edges[[i]] <- melt(movie_mat[[i]]) # make edgelist
  movie_edges[[i]][["tconst"]] <- movie_vec[i] #attach movie to that
  
  movie_edges[[i]] <- left_join(movie_edges[[i]], movie_years, by = c("tconst" = "tconst"))
}
movie_edges_c <- bind_rows(movie_edges) # long dataset of edges




#priorcollabs <- list()

for (i in 8208:length(movie_vec)) {
  
  priorcollabs[[i]] <- left_join(movie_edges[[i]], movie_edges_c, by = c("Var1" = "Var1", "Var2" = "Var2"))
  
  priorcollabs[[i]][["value.x"]] <- ifelse(priorcollabs[[i]][["value.y"]] == 1 & 
                                             priorcollabs[[i]][["year.y"]] < priorcollabs[[i]][["year.x"]], 1, 0)
  
  
  priorcollabs[[i]] <- unique(priorcollabs[[i]][, c("Var1", "Var2", "value.x", "tconst.x")])
  
  names(priorcollabs[[i]]) <- c("Var1", "Var2", "value", "tconst")
  
  priorcollabs[[i]][["Var1"]] <- as.character(priorcollabs[[i]][["Var1"]])
  priorcollabs[[i]][["Var2"]] <- as.character(priorcollabs[[i]][["Var2"]])
  
}


nets <- list()
densities <- 0
centrals <- 0
for (i in seq(priorcollabs)) {
  
  nets[[i]] <- graph_from_edgelist(as.matrix(priorcollabs[[i]][priorcollabs[[i]][["value"]] == 1 & 
                                                                 priorcollabs[[i]][["Var1"]] != priorcollabs[[i]][["Var2"]], c("Var1", "Var2")]), directed = TRUE)
    
  
  nets[[i]] <- add_vertices(nets[[i]], length(unique(priorcollabs[[i]][["Var1"]]))-vcount(nets[[i]]))
  
  V(nets[[i]])$name <- unique(priorcollabs[[i]][["Var1"]])             
  
  densities[i] <- edge_density(nets[[i]], loops=F)
  centrals[i] <- centr_degree(nets[[i]], mode="all", normalized=T)$centralization
}

collabnets <- data.frame(cbind(movie_vec[1:length(densities)], densities, centrals))
collabnets$densities[collabnets$densities == "NaN"] <- 0
collabnets$centrals[collabnets$centrals == "NaN"] <- 0
names(collabnets) <- c("tconst", "collabdens", "collabcentr")
write.csv(collabnets, "data/final_results/collabnets.csv")

x <- read.csv("data/final_results/collabnets.csv")

plot(density(as.numeric(collabnets$collabdens)))
plot(density(as.numeric(collabnets$collabcentr)))
cor(as.numeric(collabnets$collabdens), as.numeric(collabnets$collabcentr)) # not so highly correlated!
