"
2021.02.25
Chess Digits
Capture Patterns

Pipeline for article:
https://web.chessdigits.com/articles/...

"

df <- load_data(k_games=200, use_local_file=TRUE)
bu <- df # temp
df <- remove_abnormal_termination(df)
