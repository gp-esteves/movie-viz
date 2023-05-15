library(jsonlite)
library(httr)
library(tidyverse)
library(stringr)
library(ggtext)
library(ggimage)
library(viridis)
library(patchwork)
options(scipen = 9999)
#source("get-movie-ids.R")

movie_ids <- read.csv("movie_ids.csv")
# api_key <- "YourKeyHere" # Make sure to get and insert your own key here!

theme_dark <- function() {
  theme_bw() +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_line(color = "gray30"),
          panel.grid.major.y = element_blank(),
          panel.background = element_rect(fill = 'gray19', colour = 'black'),
          plot.background = element_rect(fill = 'gray15', colour = 'black'),
          axis.title = element_text(colour = "gray90"),
          axis.text = element_text(colour = "gray90"),
          plot.title = element_text(colour = "gray90"),
          plot.subtitle = element_text(colour = "gray90"))
  
}

## Get images for top 5 ##

base_url <- "https://image.tmdb.org/t/p/"
file_size <- "w500"

get_poster_path <- function(id) {
  api_req <- GET(paste0("https://api.themoviedb.org/3/movie/", id, "/images?api_key=", api_key, 
                        "&language=en-US&include_image_language=en,null"))
  if (api_req$status_code == 200) {
    file_path <- fromJSON(rawToChar(api_req$content), flatten=TRUE)[["logos"]] |> 
      slice_max(vote_average, n=1) |> slice(1) |> 
      select(file_path) |> pull()
    
    paste0(base_url, file_size, file_path)
  } else print(paste("API request denied, status code:", api_req$status_code))
}

top_5 <- movie_ids |> 
  slice_max(popularity, n=5) |> 
  mutate(original_title = fct_reorder(original_title, -popularity))

poster_paths <- map(.x=top_5$id, .f=get_poster_path) |> 
  enframe() |>
  unnest(value) |> 
  select(value) 

poster_paths <- poster_paths$value

#Map(function(u, d) download.file(u, d, method='curl'), poster_paths, poster_destinations)

## get top 10 grossing movie backdrops

poster_paths <- paste0(base_url, file_size, horror_path$backdrop_path)

poster_destinations <- paste0("C:\\Users\\Gabriel\\Desktop\\movie-viz\\Posters\\top_grossing\\", 
                              horror_path$original_title, ".jpg")

#Map(function(u, d) download.file(u, d, method='curl'), poster_paths, poster_destinations)

## Get movie info

get_all_horror_ids <- function(pages) {
  api_req <- GET(paste0("https://api.themoviedb.org/3/discover/movie?api_key=", api_key, 
                        "&language=en-US&sort_by=release_date.desc&page=", pages, "&with_genres=27"))
  
  if (api_req$status_code == 200) {
    movie_info <- fromJSON(rawToChar(api_req$content), flatten=TRUE)
    return(movie_info)
  } else print(paste("API request denied, status code:", api_req$status_code))
}

pages <- 1:500

all_horror_ids <- map(.x=pages, .f=get_all_horror_ids, .progress=TRUE)

load(file = "horror_ids.RData")

i <- 1

while (i < 501) {
  new_info <- all_horror_ids[[i]][["results"]] |> select(id, original_title)
  if (i == 1) {
    horror_id <- new_info
  } else horror_id <- full_join(horror_id, new_info)
  i = i+1
}

get_info <- function(id) {
  api_req <- GET(paste0("https://api.themoviedb.org/3/movie/", id, "?api_key=", api_key, 
                        "&language=en-US&append_to_response=images,account_states"))
  if (api_req$status_code == 200) {
    movies_info <- fromJSON(rawToChar(api_req$content), flatten=TRUE) 
    return(movies_info)
  } else print(paste("API request failed, status code:", api_req$status_code))
}

horror_info <- map(.x=horror_id$id, .f=get_info, .progress=TRUE)

save(horror_info, file="horror_info.RData")

load(file="horror_info.RData")

i <- 1

while (i < 9502) {
  new_info <- horror_info[[i]] |> enframe() |> pivot_wider(names_from=name, values_from=value) 
  if (i == 1) {
    horror_df <- new_info
  } else horror_df <- full_join(horror_df, new_info)
  i = i+1
}

#save(horror_df, file="horror_df.RData")
