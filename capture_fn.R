"
2021.02.25
Chess Digits
Capture Patterns

Functions for article:
https://web.chessdigits.com/articles/...

"


#### imports ####
library(ggplot2)


#### helper fn ####
view <- utils::View


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

# remove results
remove_results <- function(df, results)
{
  n_pre <- nrow(df)
  df <- df[!df$Result %in% results,]
  df$Result <- factor(df$Result) # to ensure non-zero levels
  n_post <- nrow(df)
  # out
  print("Removed results:")
  print(results)
  print(paste("Total games removed:", n_pre - n_post))
  return(df)
}

# last move
add_last_ply <- function(df)
{
  # using move
  cols <- grep("Move_ply_", colnames(df))
  last_ply <- apply(df[cols], 1, function(r) which(r == "" | is.na(r))[1]-1) # -1 bc otherwise it indicates first ply at which no move
  
  # replace NA with max ply in df
  last_ply[is.na(last_ply)] <- length(cols)
  
  # add to df
  df$last_ply <- last_ply
  
  # out
  print("Added variable last_ply")
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

#### captures ####
add_capture_indicator_at_each_ply <- function(df, first_ply=1, last_ply=30*2)
{
  # remove previous capture indicators if any
  df <- df[,!grepl("Capture_ply_", colnames(df))]
  
  # get capture indicators
  cap <- apply(df[paste0("Move_ply_", first_ply:last_ply)], 2, function(col) grepl("x", col))
  colnames(cap) <- paste0(gsub(pattern = "Move", replacement = "Capture", x = colnames(cap)))
  df <- cbind(df, cap)
  
  # out
  print(paste0("Added columns Capture_ply_", first_ply, " to Capture_ply_", last_ply))
  return(df)
}


add_cumulative_captures_at_each_ply <- function(df)
{
  # remove previous cumulative capture columns if any
  df <- df[,!grepl("Cumcap_ply_", colnames(df))]
  
  # capture indicator columns
  cols <- colnames(df)[substr(colnames(df), 1, 12) == "Capture_ply_"]
  cols_ply <- as.numeric(gsub(pattern = "Capture_ply_", replacement = "", x = cols))
  df_cols <- df[cols]
  
  # placeholder dataframe
  cap <- data.frame(matrix(NA, nrow=nrow(df), ncol=length(cols)))
  
  # add cumulative sum
  for (i in 1:length(cols))
  {
    .df <- df_cols[1:i]
    cap[,i] <- apply(.df, 1, sum, na.rm=TRUE)
  }
  
  # add to df
  colnames(cap) <- paste0("Cumcap_ply_", cols_ply[1]:cols_ply[ncol(cap)])
  df <- cbind(df, cap)
  
  # out
  print(paste0("Added columns Cumcap_ply_", cols_ply[1], " to Cumcap_ply_", cols_ply[ncol(cap)]))
  return(df)
  
}


# replace with NA if game had ended
replace_capture_vars_with_NA_after_game_ended <- function(df, var_prefix="Cumcap")
{
  # columns to work on
  cols <- grep(var_prefix, colnames(df), value=T)
  cols_ply <- as.numeric(gsub(pattern = paste0(var_prefix, "_ply_"), replacement = "", x = cols))
  
  # do
  for (i in 1:length(cols))
  {
    df[df$last_ply < cols_ply[i], cols[i]] <- NA
  }
  
  # out
  print(paste0("Replaced ", var_prefix, "_ply_", " variables with NA after game ended"))
  return(df)
  
}


# add number of trades initiated
add_trades_initiated <- function(df, last_ply=200)
{
  df_with_selected_captures <- add_capture_indicator_at_each_ply(df, first_ply=1, last_ply=last_ply)
  cols <- grep("Capture_ply_", colnames(df_with_selected_captures), value=T)
  
  init <- list(w=c(), b=c())
  for (i in 1:nrow(df)) # each row
  {
    row <- as.logical(df_with_selected_captures[i,cols])
    max_ply <- min(df$last_ply[i], length(row))
    row <- row[1:max_ply]
    captures <- row
    capture_next_move <- c(row[-1], F)
    capture_previous_move <- c(F, row[-length(row)])
    
    # do
    initiated <- which(captures & !capture_previous_move & capture_next_move)
    init$b[i] <- sum(initiated %% 2 == 0)
    init$w[i] <- sum(initiated %% 2 != 0)
    
  } # end each row
  
  # add to df
  df[,"White_trades_initiated"] <- init$w
  df[,"Black_trades_initiated"] <- init$b
  
  # out
  print("Added variables White_trades_initiated and Black_trades_initiated")
  return(df)
  
}

# trades diff
add_trades_initiated_differential <- function(df)
{
  df$trades_initiated_diff <- df$White_trades_initiated - df$Black_trades_initiated
  print("Added variable trades_initiated_diff (White minus Black)")
  return(df)
}



#### plots ####
# helper fn
get_average_capture_at_each_ply_by <- function(df, by=NULL)
{
  # set up: columns and list
  cols <- grep("Cumcap_ply_", colnames(df), value=T)
  cols_ply <- as.numeric(gsub(pattern = paste0("Cumcap_ply_"), replacement = "", x = cols))
  avg_cap <- list()

  # get values for each column
  for (i in 1:length(cols))
  {
    agg <- aggregate(
      list(Cumulative_Capture=df[,cols[i]]),
      list(Group=df[,by]),
      mean, na.rm=TRUE
    )
    agg$Ply <- cols_ply[i]
    names(agg)[which(names(agg)=="Group")] <- by
    avg_cap[[cols[i]]] <- agg
  }
  
  # combine
  avg_cap <- do.call(rbind, avg_cap)
  
  # out
  return(avg_cap)
  
}

# plot
get_plot_cumulative_captures_by <- function(df, by=NULL, by_label=NULL, linetype=TRUE)
{
  # groups
  if(is.null(by)) { by <- "All_Games"; df[,by] <- "All Games" }
  if(is.null(by_label)) by_label <- by
  
  # get points to plot
  agg <- get_average_capture_at_each_ply_by(df, by=by)
  
  # config plot
  #ymax <- 55
  labs <- list(
    x="Move",
    y="Total Captures"
  )
  #yticks <- seq(0,ymax,5)
  
  # plot
  ggplot(agg, aes_string(x="Ply", y="Cumulative_Capture", group=by, color=by)) + geom_line(aes_string(linetype=if(by %in% c("WhiteElo_bucket", "BlackElo_bucket") | !linetype) NULL else by), size=2) + 
    #ylim(0,ymax) +
    labs(color=by_label, linetype=by_label, x=labs$x, y=labs$y) +
    scale_y_continuous(breaks=1:50)+#, limits=c(0,ymax)) + 
    scale_x_continuous(breaks=seq(1, 200, 2), labels=seq(1, 100, 1))+
    theme(text=element_text(size=15))
}


# helper fn
get_percent_result_by <- function(df, result="1-0", by=NULL)
{
  
  agg <- aggregate(
    list(Percent_Winning=df$Result),
    list(Group=df[,by]),
    function(x) sum(x==result, na.rm=T)/length(x)*100 # percent
  )
  names(agg)[which(names(agg)=="Group")] <- by
  
  # out
  return(agg)
  
}


# plot trades initiated by
get_plot_trades_initiated_by <- function(df, by=NULL, by_label=NULL, linetype=TRUE)
{
  "
  this could use refactoring
  a lot of repetition with get_plot_cumulative_captures_by() above
  
  "
  # groups
  if(is.null(by)) { by <- "All_Games"; df[,by] <- "All Games" }
  if(is.null(by_label)) by_label <- by
  
  # get points to plot
  "
  this could be made into a function, argument = variable to split by
  (here trades initiated)
  "
  aggs <- list()
  for (trade in unique(df[,"trades_initiated_diff"]))
  {
    .df <- df[df[,"trades_initiated_diff"]==trade,]
    agg <- get_percent_result_by(.df, by=by) # agg helper fn here
    agg[,"trades_initiated_diff"] <- trade
    aggs[[as.character(trade)]] <- agg
  }
  agg <- do.call(rbind, aggs)

  
  # config plot
  #ymax <- 55
  labs <- list(
    x="Difference in Trades Initiated",
    y="Percent Winning (%)"
  )
  #yticks <- seq(0,ymax,5)
  
  # plot
  ggplot(agg, aes_string(x="trades_initiated_diff", y="Percent_Winning", group=by, color=by)) + geom_line(aes_string(linetype=if(by %in% c("WhiteElo_bucket", "BlackElo_bucket") | !linetype) NULL else by), size=2) + 
    #ylim(0,ymax) +
    labs(color=by_label, linetype=by_label, x=labs$x, y=labs$y) +
    #scale_y_continuous(breaks=1:50)+#, limits=c(0,ymax)) + 
    #scale_x_continuous(breaks=seq(1, 200, 2), labels=seq(1, 100, 1))+
    theme(text=element_text(size=15))
  
}
