library(stringi)
library(data.table)
library(parallel)
library(fastDummies)


get_saved_tracks <- function(){
  offset <- 0
  result <- data.frame()
  cur_tracks <- get_my_saved_tracks(limit=50)
  while (length(cur_tracks) > 0) {
    result <- rbind(result, cur_tracks)
    offset <- offset + 50
    cur_tracks <- get_my_saved_tracks(limit=50, offset = offset)
  }
  result <- cbind(result, meta(result))
  return(result)
}

#____________________________________________________#

# data processing functions#


get_artists_str <- function(artists_df)
{
  return(toString(artists_df$name))
}



year_helper <- function(x){
  splited <- x %>% strsplit('-')
  return(splited[[1]][1])
}

img_helper <- function(img_df)
{
  return(list(small.img = img_df$url[[3]], med.img = img_df$url[[2]]))
}

meta <- function(df)
{
  release_year <- mclapply(df$track.album.release_date,year_helper) %>% strtoi()
  artists <-mclapply(df$track.album.artist, get_artists_str)
  return(data.frame(cbind(artists,release_year)))
}


sections_analys <- function(track_an)
{
  duration <- track_an[["track"]][["duration"]]
  sections <- data.frame(track_an['sections'])
  norm_duration <- sections$sections.duration / duration
  selected_df <- select(sections,c('sections.loudness', 'sections.tempo', 'sections.key')) 
  sec_sum <- colSums(selected_df * norm_duration) 
  std <-  apply(selected_df, 2, sd)
  names(std) <- c('sections.loudness.std', 'sections.tempo.std', 'sections.key.std')
  return(c(sec_sum, std, list(sections.num = dim(selected_df)[1])))
}

interval_analys <- function(interval, title)
{
  res <- list(sum(interval$duration) / dim(interval)[1], dim(interval)[1])
  names(res) <-c(paste0("average.",title,".duration"),paste0(title,".amount"))
  return(res)
}



track_vector <- function(track_an)
{
  sections_vec <- sections_analys(track_an)
  beats_vec <- interval_analys(track_an$beats,"beats")
  bars_vec <- interval_analys(track_an$bars, "bars")
  return(rbind(c(sections_vec, beats_vec, bars_vec)))
}

helper_func <-function(x)
{
  colnames(x) <- c( "sections.loudness", "sections.tempo", "sections.key", "sections.loudness.std", "sections.tempo.std", "sections.key.std",  
                    "sections.num","average.beats.duration", "beats.amount","average.bars.duration","bars.amount")
  return(x)
}

get_data_df <- function(ids){
  features <- get_track_audio_features(ids)[,1:11]
  features <- mclapply(ids, get_track_audio_analysis) %>%
    mclapply(track_vector) %>%
    rbindlist() %>% helper_func() %>%
    cbind(features)
}
# data_processing <-function(df)
# {
#   ids <- df$track.id
#   anlys_df <- data.frame()
#   i <- min(50, length(ids))
#   p <-0
#   print("start to process the data (it takes some time)")
#   while (dim(anlys_df)[1] != length(ids)) {
#     cur_ids <- ids[p:i]
#     anlys_df <- rbind(anlys_df, get_data_df(cur_ids))
#     p <- i + 1
#     i <- min(i + 50, length(ids))
#     print(paste0(round( i / length(ids),digits = 2) * 100,"% ","complete"))
#   }
#   print("create dummies")
#   dummies <- mclapply(df$artists, stri_enc_toutf8)  %>%
#     I %>% data.frame() %>%
#     dummy_cols(split=',', remove_selected_columns= TRUE)
#   anlys_df <- cbind(anlys_df,release_year = as.numeric(df$release_year), dummies)
#   print("done processing!")
#   return(anlys_df) }

#______________________________________________#

# T-sne analysis #




normalize_df <- function(df){
  
  # norm_df <- (as.matrix(df[,1:23]) - colMeans(df[,1:23])) / apply(df[,1:23],2, sd)
  return(df)
  
}

get_tsne_df <- function(df){
  norm_df <- normalize_df(df)
  tsne_df <- Rtsne(as.matrix(norm_df), normalize=FALSE)$Y
  
}

kmeans_label <-function(df, n)
{
  norm_df <- normalize_df(df)
  return(kmeans(norm_df,n)$cluster)
  
}

find_duplicates <- function(data_df)
{
  
  res = c()
  dup_idx <-  which(duplicated(data_df) | duplicated(data_df, fromLast = TRUE))
  while (length(dup_idx) > 0) {
    res <- cbind(res,dup_idx[[1]] )
    data_df <- data_df[-c(dup_idx[[1]])]
    dup_idx <-  which(duplicated(data_df) | duplicated(data_df, fromLast = TRUE))
  }
  return(res)
}

# get_plotdf<- function(){
#   tracks_df <- get_saved_tracks() 
#   track_anlys <- data_processing(tracks_df)
#   #remove duplicated song
#   print("Remove duplicates")
#   duplicates <- find_duplicates(track_anlys)
#   track_anlys <- track_anlys[-duplicates,]
#   meta_d <- tracks_df[-duplicates,]
#   print("doing T-sne ")
#   res <- get_tsne_df(track_anlys)
#   meta_d$T_sne.X <- res[,1]
#   meta_d$T_sne.Y <- res[,2]
#   pca_res <- prcomp(track_anlys, scale. = FALSE)
#   print("All done :)")
#   return(list(meta_d, pca_res, track_anlys))
# }

