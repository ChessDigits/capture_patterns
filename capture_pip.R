"
2021.02.25
Chess Digits
Capture Patterns

Pipeline for article:
https://web.chessdigits.com/articles/...

"

df <- load_data(k_games=200, use_local_file=TRUE)
bu<-df
df <- df[sample(1:nrow(df), size = 10000, replace = F),]
df <- remove_abnormal_termination(df)
df <- restrict_by_rating_differential(df, max_diff=100)
df <- restrict_by_rating(df, player = "White", min_rating=800, max_rating=2300)
df <- restrict_by_rating(df, player = "Black", min_rating=800, max_rating=2300)
df <- add_rating_buckets(df)
df <- add_last_ply(df)
df <- add_capture_indicator_at_each_ply(df, first_ply = 1, last_ply = 60)
df <- add_cumulative_captures_at_each_ply(df)
df <- replace_capture_vars_with_NA_after_game_ended(df, var_prefix="Cumcap")
get_plot_cumulative_captures_by(df, by="WhiteElo_bucket", by_label="Rating")
#get_plot_cumulative_captures_by(df, by="Category", by_label="Time Control")

# keep only games that hadn't ended yet at max ply?!

# trades initiated
df <- add_trades_initiated(df, last_ply = 200)
df <- add_trades_initiated_differential(df)
df <- remove_results(df, results = "1/2-1/2")

# subset, too few games with large discrepancy in trades initiated
.df <- df[df$trades_initiated_diff>= -5 & df$trades_initiated_diff<= 5,]
get_plot_trades_initiated_by(.df, by="WhiteElo_bucket", by_label = "Rating")
get_plot_trades_initiated_by(.df, by="Category", by_label = "Time Control")
get_plot_trades_initiated_by(.df)#, by="Category", by_label = "Time Control")