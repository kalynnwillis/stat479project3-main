# 02_compute_IASA.R
# Description: Compute Ball-Flight Differential added (BFD).
# Formula: BFD = d_catch - d_throw
# Interpretation:
#   Positive BFD -> Gained separation (Good for WR)
#   Negative BFD -> Lost separation (Good for DB)

library(tidyverse)

compute_BFD <- function(play_features) {
  play_features |>
    mutate(
      BFD = d_catch - d_throw,
      # Additional derived metrics
      separation_change_pct = ifelse(d_throw > 0, (d_catch - d_throw) / d_throw, NA_real_),
      is_separation_gained = BFD > 0
    )
}

if (sys.nframe() == 0) {
  # Source path helpers
  if (file.exists("src/utils_paths.R")) {
    source("src/utils_paths.R")
  } else if (file.exists("utils_paths.R")) {
    source("utils_paths.R")
  } else {
    stop("Could not find 'utils_paths.R'.")
  }

  # Determine input/output directory
  PROC_DIR <- get_proc_dir()

  # Find all feature files
  feature_files <- list.files(PROC_DIR, pattern = "play_features_w[0-9]+\\.rds", full.names = TRUE)

  if (length(feature_files) == 0) {
    stop(paste("No feature files found in", PROC_DIR, ". Run 01_engineer_features.R first."))
  }

  message(paste("Found", length(feature_files), "feature files. Loading and combining..."))

  features <- purrr::map_dfr(feature_files, readRDS)

  message(paste("Total plays loaded:", nrow(features)))

  message("Computing BFD and derived metrics...")
  analysis_df <- compute_BFD(features)

  # --- Optional: Join supplementary play-level data, if available ---
  # This adds route / coverage / EPA context from BDB2026_supplementary_data.csv
  SUPP_PATH <- NULL
  if (dir.exists("data")) {
    SUPP_PATH <- file.path("data", "BDB2026_supplementary_data.csv")
  } else if (dir.exists("../data")) {
    SUPP_PATH <- file.path("../data", "BDB2026_supplementary_data.csv")
  }

  if (!is.null(SUPP_PATH) && file.exists(SUPP_PATH)) {
    message("Joining supplementary play-level data...")

    supp <- readr::read_csv(SUPP_PATH, show_col_types = FALSE) |>
      dplyr::select(
        game_id,
        play_id,
        season,
        week,
        play_description,
        offense_formation,
        receiver_alignment,
        route_of_targeted_receiver,
        pass_result,
        pass_length,
        pass_location_type,
        play_action,
        dropback_type,
        dropback_distance,
        defenders_in_the_box,
        team_coverage_man_zone,
        team_coverage_type,
        expected_points,
        expected_points_added,
        yards_gained
      )

    # Ensure join keys have compatible types
    analysis_df <- analysis_df |>
      mutate(
        game_id = as.numeric(game_id),
        play_id = as.numeric(play_id)
      ) |>
      left_join(
        supp |>
          mutate(
            game_id = as.numeric(game_id),
            play_id = as.numeric(play_id)
          ),
        by = c("game_id", "play_id")
      )
  } else {
    message("Supplementary play-level data not found; proceeding without it.")
  }

  # Basic summary
  summary_stats <- analysis_df |>
    summarise(
      avg_BFD = mean(BFD, na.rm = TRUE),
      median_BFD = median(BFD, na.rm = TRUE),
      prop_positive = mean(BFD > 0, na.rm = TRUE),
      n_plays = n()
    )

  print(summary_stats)

  saveRDS(analysis_df, file.path(PROC_DIR, "analysis_full.rds"))
  message(paste("Saved analysis_full.rds to", PROC_DIR))
}
