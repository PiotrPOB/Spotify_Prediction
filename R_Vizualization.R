# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("tidyverse")
# install.packages("fmsb")
# loading libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(skimr)
library(ggplot2)
library(plotly)
library(dplyr)
library(fmsb)

# loading data
Spotify_All_Music = read_csv("AllSpotifySongs.csv")
Spotify_All_Music <- Spotify_All_Music[-1]

# loading own liked music from Spotify Playlist - 90 obs.
Spotify_Likied_Music = read_csv("RandomSpotifyList_Liked.csv")
Spotify_Likied_Music <- Spotify_Likied_Music[-1]

# loading unliked music from Spotify Playlist - 90 obs.
Spotify_Unlikied_Music = read_csv("RandomSpotifyList_Unliked.csv")
Spotify_Unlikied_Music <- Spotify_Unlikied_Music[-1] 

# loading prediction data based on RandomForest algorythm 
Spotify_NewList = read_csv("NewList.csv")

# preparing date to make some visualization - comparison music liked with all music 
tracks_like <- Spotify_Likied_Music
tracks_dislike <- Spotify_All_Music

# presenting data 
skim(Spotify_Likied_Music)
skim(Spotify_Likied_Music)

tracks <- bind_rows(tracks_like %>% mutate(liked = TRUE),
                    tracks_dislike %>% mutate(liked = FALSE)) %>%
  distinct(id, .keep_all = TRUE) %>% mutate(liked=as.factor(liked))


tracks_all <- tracks %>% select(id, liked, danceability, energy, liveness, popularity, 
                                loudness, liveness, instrumentalness, tempo, mode, valence, 
                                speechiness, duration_ms) %>%  
  gather(feature, value, -id, -liked) %>% 
  mutate(like = case_when(.$liked == TRUE ~ "songs liked",.$liked == FALSE ~ "songs disliked")) %>% select(-liked)


# creating plot showing difference between own liked play list and all music songs
tracks_all %>% ggplot() + geom_density(aes(value, fill=like), alpha=0.2) + 
  facet_wrap(~feature, scales = "free") + labs(x = "", y ="", fill = "Music:") + theme(legend.position = "bottom")

# presenting data 
skim(Spotify_NewList)

# preparing data to make some visualization - comparison predicted music and liked music
tracks_like <- Spotify_Likied_Music
tracks_like_prediction <- Spotify_NewList[-19]

tracks_pre <- bind_rows(tracks_like %>% mutate(liked = TRUE),
                        tracks_like_prediction %>% mutate(liked = FALSE)) %>%
  distinct(id, .keep_all = TRUE) %>% mutate(liked=as.factor(liked))


tracks_all_pre <- tracks_pre %>% 
  select(id, popularity, liked, danceability, energy, liveness, popularity, 
         loudness, liveness, instrumentalness, tempo, mode, valence, speechiness, duration_ms) %>%  
  gather(feature, value, -id, -liked) %>% 
  mutate(like = case_when(.$liked == TRUE ~ "songs prediction liked",.$liked == FALSE ~ "songs liked")) %>% select(-liked)

# creating plot showing difference between predicted music and liked music
tracks_all_pre %>% ggplot() + geom_density(aes(value, fill=like), alpha=0.2) + 
  facet_wrap(~feature, scales = "free") + labs(x = "", y ="", fill = "Music:") + theme(legend.position = "bottom")


# create polar chart for features of 
mediana_likes <- tracks_like %>% select(c("danceability","energy", "speechiness", "acousticness", "instrumentalness", "liveness", "valence"))
mediana_predictions <- tracks_like_prediction %>% select(c("danceability","energy", "speechiness", "acousticness", "instrumentalness", "liveness", "valence"))

mediana_likes <- apply(mediana_likes, 2, median)
mediana_predictions <- apply(mediana_predictions, 2, median)

all_data <- rbind(mediana_likes, mediana_predictions)


data <- as.data.frame(matrix(all_data , ncol=ncol(all_data)))
colnames(data) <- c("danceability","energy", "speechiness", "acousticness", "instrumentalness", "liveness", "valence")

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!

data <- rbind(1, 0 , data)

# Check your data, it has to look like this!
# head(data)


radarchart(data, title = "Songs liked vs Songs predicted")


