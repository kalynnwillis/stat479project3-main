
# Load data
if (file.exists("processed/analysis_full.rds")) {
  path <- "processed/analysis_full.rds"
} else {
  path <- "../processed/analysis_full.rds"
}

df <- readRDS(path)

# Filter NAs
na_rows <- df[is.na(df$IASA), ]

if (nrow(na_rows) > 0) {
    # Extract week from game_id (first 8 chars date, but we can just list unique game_ids)
    # Or just count plays per game
    game_counts <- table(na_rows$game_id)
    print(head(game_counts))
    cat("Total games affected:", length(game_counts), "\n")
    
    # Check if all weeks are present
    # game_id structure: YYYYMMDDXX
    dates <- substr(names(game_counts), 1, 8)
    print(table(dates))
}

