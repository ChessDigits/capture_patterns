"
2021.02.25
Chess Digits
Capture Patterns

Functions for article:
https://web.chessdigits.com/articles/...

"


#### imports ####
library(ggplot2)


#### load data ####
load_data <- function(k_games=c(200,500), use_local_file=TRUE)
{
  if (!use_local_file)
  {
    cat("Loading data directly from ChessDigits.com is very time consuming.\nWe recommend saving the data locally and setting use_local_file to TRUE.\n")
    dir <- "https://chessdigits.com/data/"
  } else dir <- "d:/Chess/databases/lichess_May2019/out/"
  fpath <- paste0(dir, k_games, "k_blitz_rapid_classical_bullet.csv")
  df <- read.csv(fpath, stringsAsFactors = T)
  return(df)
}


#### data prep ####
# remove abnormal termination
remove_abnormal_termination <- function(df)
{
  n_pre <- nrow(df)
  remove <- c("Abandoned", "Rules infraction")
  df <- df[!df$Termination %in% remove,]
  df$Termination <- factor(df$Termination) # to ensure non-zero levels
  df$Result <- factor(df$Result) # to ensure non-zero levels
  n_post <- nrow(df)
  
  # out
  print("Removed abnormal terminations:")
  print(remove)
  print(paste("Total games removed:", n_pre - n_post))
  return(df)
}


#### ratings ####
# restrict by rating differential
restrict_by_rating_differential <- function(df, max_diff)
{
  n_pre <- nrow(df)
  diff <- abs(df$WhiteElo - df$BlackElo)
  df <- df[diff <= max_diff,]
  n_post <- nrow(df)
  
  # out
  print(paste0("Removed games where players differed by Elo > ", max_diff, " (n pre = ", n_pre, ", n post = ", n_post, ")"))
  return(df)
}


# restrict by rating
restrict_by_rating <- function(df, player=c("White", "Black"), min_rating=900, max_rating=2400)
{
  n_pre <- nrow(df)
  player <- match.arg(player)
  df <- df[df[paste0(player, "Elo")] < max_rating & df[paste0(player, "Elo")] >= min_rating,]
  n_post <- nrow(df)
  
  # out
  print(paste0("Removed games where had Elo >= ", max_rating, " (n pre = ", n_pre, ", n post = ", n_post, ")"))
  return(df)
}


# rating buckets
add_rating_buckets <- function(df)
{
  for (player in c("White", "Black"))
  {
    # ordered factor
    df[,paste0(player, "Elo_bucket")] <- factor(floor(df[,paste0(player, "Elo")]/100), ordered=TRUE)
    
    # levels (e.g. 1400 instead of 14)
    levels(df[,paste0(player, "Elo_bucket")]) <- as.numeric(levels(df[,paste0(player, "Elo_bucket")]))*100
  }
  
  # out
  print("Added variables WhiteElo_bucket and BlackElo_bucket")
  return(df)
}
