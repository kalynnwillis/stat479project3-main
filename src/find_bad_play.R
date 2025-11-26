
# Load data
if (file.exists("processed/analysis_full.rds")) {
  path <- "processed/analysis_full.rds"
} else {
  path <- "../processed/analysis_full.rds"
}

df <- readRDS(path)

# Find NA rows
na_rows <- df[is.na(df$IASA), ]

if (nrow(na_rows) > 0) {
    cat("Found", nrow(na_rows), "rows with NA IASA.\n")
    # Pick the first one
    first_na <- na_rows[1, ]
    cat("Inspecting Game ID:", first_na$game_id, "Play ID:", first_na$play_id, "\n")
    
    # Check if d_throw or d_catch is the culprit
    cat("d_throw:", first_na$d_throw, "\n")
    cat("d_catch:", first_na$d_catch, "\n")
} else {
    cat("No NA IASA found.\n")
}

