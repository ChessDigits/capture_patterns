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

