library(jsonlite)
library(httr)
library(tidyverse)
library(viridis)

date <- format(as.Date(Sys.Date()), "%m_%d_%Y")

movie_ids <- stream_in(
  gzcon(
    url(paste0("http://files.tmdb.org/p/exports/movie_ids_", date, ".json.gz"))))

write.csv(movie_ids, "movie_ids.csv")