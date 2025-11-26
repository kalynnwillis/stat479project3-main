
# Load data
# Assuming run from root or we need to adjust path
if (file.exists("processed/analysis_full.rds")) {
  path <- "processed/analysis_full.rds"
} else if (file.exists("../processed/analysis_full.rds")) {
  path <- "../processed/analysis_full.rds"
} else {
  stop("File not found")
}

df <- readRDS(path)

# Check NAs
na_IASA <- sum(is.na(df$IASA))
na_d_throw <- sum(is.na(df$d_throw))
na_air_time <- sum(is.na(df$air_time))
inf_IASA <- sum(is.infinite(df$IASA))
inf_d_throw <- sum(is.infinite(df$d_throw))

cat("Total rows:", nrow(df), "\n")
cat("NA IASA:", na_IASA, "\n")
cat("NA d_throw:", na_d_throw, "\n")
cat("NA air_time:", na_air_time, "\n")
cat("Inf IASA:", inf_IASA, "\n")
cat("Inf d_throw:", inf_d_throw, "\n")

if("d_catch" %in% names(df)) {
    cat("NA d_catch:", sum(is.na(df$d_catch)), "\n")
}

# Inspect a few NA rows if any
if (na_IASA > 0) {
    cat("\nFirst 5 rows with NA IASA:\n")
    print(head(df[is.na(df$IASA), c("game_id", "play_id", "d_throw", "d_catch", "air_time")], 5))
}
