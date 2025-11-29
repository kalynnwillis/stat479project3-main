# 01_engineer_features.R
# Description: Feature engineering for receiver movement analysis.
# Input: Raw tracking data (from 00_load_data.R)
# Output: Play-level features including separation at throw/catch.

library(tidyverse)

# --- Helpers ---

identify_targeted_receiver <- function(play_data) {
  tr <- play_data |>
    filter(player_role == "Targeted Receiver") |>
    distinct(nfl_id) |>
    pull(nfl_id)

  if (length(tr) == 0) {
    return(NA_integer_)
  } # no targeted receiver (should be rare)
  if (length(tr) > 1) {
    # if multiple, just take first for now; you can refine later
    warning("Multiple targeted receivers found in one play; taking first.")
    tr <- tr[1]
  }
  tr
}

calculate_separation <- function(receiver_loc, defenders_loc) {
  if (nrow(receiver_loc) == 0 || nrow(defenders_loc) == 0) {
    return(NA_real_)
  }
  rx <- receiver_loc$x[1]
  ry <- receiver_loc$y[1]
  dx <- defenders_loc$x
  dy <- defenders_loc$y
  min(sqrt((rx - dx)^2 + (ry - dy)^2))
}

compute_play_separation <- function(gid, pid, input_df, output_df) {
  play_in <- input_df |> filter(game_id == gid, play_id == pid)
  play_out <- output_df |> filter(game_id == gid, play_id == pid)

  if (nrow(play_in) == 0 || nrow(play_out) == 0) {
    return(NULL)
  }

  tr_id <- identify_targeted_receiver(play_in)
  if (is.na(tr_id)) {
    return(NULL)
  }

  # --- Throw frame (input data: up to throw) ---
  throw_frame <- max(play_in$frame_id, na.rm = TRUE)
  rec_throw <- play_in |> filter(nfl_id == tr_id, frame_id == throw_frame)

  # Targeted receiver name (from input CSV)
  tr_name_vec <- play_in |>
    filter(nfl_id == tr_id) |>
    distinct(player_name) |>
    pull(player_name)

  tr_name <- if (length(tr_name_vec) > 0) tr_name_vec[1] else NA_character_

  # Targeted receiver position (from input CSV; e.g., WR/TE/RB/FB)
  tr_pos_vec <- play_in |>
    filter(nfl_id == tr_id) |>
    distinct(player_position) |>
    pull(player_position)

  tr_pos <- if (length(tr_pos_vec) > 0) tr_pos_vec[1] else NA_character_

  # Identify Defenders
  defs_throw <- play_in |>
    filter(player_role == "Defensive coverage", frame_id == throw_frame)

  if (nrow(defs_throw) == 0) {
    message(
      "No 'Defensive coverage' labels for game ", gid,
      ", play ", pid, "; using non-offensive players at throw_frame as defenders."
    )
    defs_throw <- play_in |>
      filter(!player_role %in% c("Targeted Receiver", "Passer", "Other Route Runner"), frame_id == throw_frame)
  }

  defender_ids <- unique(defs_throw$nfl_id)

  d_throw <- calculate_separation(rec_throw, defs_throw)

  # --- In-air frames (output data: from throw to ball arrival) ---
  rec_out <- play_out |> filter(nfl_id == tr_id)
  if (nrow(rec_out) == 0) {
    return(NULL)
  }

  first_out_frame <- min(rec_out$frame_id, na.rm = TRUE)
  catch_frame <- max(rec_out$frame_id, na.rm = TRUE)
  rec_catch <- rec_out |> filter(frame_id == catch_frame)

  defs_catch <- play_out |>
    filter(nfl_id %in% defender_ids, frame_id == catch_frame)

  d_catch <- calculate_separation(rec_catch, defs_catch)

  # Assume 10 Hz frame rate; air_time is time between first in-air frame
  # in the output and the catch frame.
  air_time <- (catch_frame - first_out_frame) * 0.1

  # --- NEW: Compute Separation Trajectory ---
  # We compute separation for EVERY frame in the output for this receiver
  # Using simple map instead of group_modify to avoid grouping issues

  frames <- unique(rec_out$frame_id)

  traj_list <- map(frames, function(f) {
    rec_loc <- rec_out |> filter(frame_id == f)
    defs <- play_out |> filter(frame_id == f, nfl_id %in% defender_ids)

    tibble(
      game_id = gid,
      play_id = pid,
      frame_id = f,
      separation = calculate_separation(rec_loc, defs)
    )
  })

  traj_df <- bind_rows(traj_list)

  list(
    play_features = tibble(
      game_id = gid,
      play_id = pid,
      targeted_id = tr_id,
      targeted_name = tr_name,
      targeted_position = tr_pos,
      throw_frame = throw_frame,
      catch_frame = catch_frame,
      d_throw = d_throw,
      d_catch = d_catch,
      air_time = air_time,
      ball_land_x = first(play_in$ball_land_x),
      ball_land_y = first(play_in$ball_land_y)
    ),
    trajectory = traj_df
  )
}

build_play_feature_table <- function(week_data) {
  input_df <- week_data$input
  output_df <- week_data$output

  plays <- input_df |> distinct(game_id, play_id)

  message(paste("Processing", nrow(plays), "plays..."))

  all_features <- list()
  all_trajectories <- list()

  for (i in 1:nrow(plays)) {
    p <- plays[i, ]
    res <- compute_play_separation(p$game_id, p$play_id, input_df, output_df)

    if (!is.null(res)) {
      all_features[[length(all_features) + 1]] <- res$play_features
      all_trajectories[[length(all_trajectories) + 1]] <- res$trajectory
    }

    if (i %% 100 == 0) cat(".")
  }
  cat("\n")

  features_df <- if (length(all_features) > 0) dplyr::bind_rows(all_features) else tibble()
  traj_df <- if (length(all_trajectories) > 0) dplyr::bind_rows(all_trajectories) else tibble()

  list(
    features = features_df,
    trajectories = traj_df
  )
}

# --- Main Execution (if run as script) ---

if (sys.nframe() == 0) {
  # Source helpers and loader from the same directory
  if (file.exists("src/utils_paths.R")) {
    source("src/utils_paths.R")
  } else if (file.exists("utils_paths.R")) {
    source("utils_paths.R")
  } else {
    stop("Cannot find utils_paths.R")
  }

  if (file.exists("src/00_load_data.R")) {
    source("src/00_load_data.R")
  } else if (file.exists("00_load_data.R")) {
    source("00_load_data.R")
  } else {
    stop("Cannot find 00_load_data.R")
  }

  OUT_DIR <- get_proc_dir()

  message(paste("Processing weeks:", paste(WEEKS, collapse = ", ")))

  for (week in WEEKS) {
    message(paste("--- Starting", week, "---"))
    week_data_list <- load_week_data(week)

    if (is.null(week_data_list)) {
      message(paste("Skipping", week, "- data not found."))
      next
    }

    message(paste("Building play-level feature table for", week, "..."))
    results <- build_play_feature_table(week_data_list)

    # Save Features
    feat_file <- file.path(OUT_DIR, paste0("play_features_", week, ".rds"))
    saveRDS(results$features, feat_file)

    # Save Trajectories (New!)
    traj_file <- file.path(OUT_DIR, paste0("separation_trajectories_", week, ".rds"))
    saveRDS(results$trajectories, traj_file)

    message(paste("Saved", feat_file, "and", traj_file))

    rm(week_data_list, results)
    gc()
  }
}
