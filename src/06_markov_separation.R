# 06_markov_separation.R
# Description: Analyze separation trajectories using Markov Chains.
# Goal: Compute transition probabilities between separation states and "Markov Lift".

library(tidyverse)

# --- Constants ---
# Define separation states (in yards)
# S0: < 1 yard (Tight)
# S1: 1-3 yards (Moderate)
# S2: 3-5 yards (Open)
# S3: > 5 yards (Wide Open)

get_state <- function(sep) {
  case_when(
    sep < 1 ~ "S0_Tight",
    sep < 3 ~ "S1_Moderate",
    sep < 5 ~ "S2_Open",
    TRUE ~ "S3_WideOpen"
  )
}

# Order of states for matrix
STATE_LEVELS <- c("S0_Tight", "S1_Moderate", "S2_Open", "S3_WideOpen")

# --- Functions ---

load_trajectories <- function(proc_dir = "processed") {
  files <- list.files(proc_dir, pattern = "separation_trajectories_w[0-9]+\\.rds", full.names = TRUE)
  if (length(files) == 0) stop("No trajectory files found.")

  message(paste("Loading", length(files), "trajectory files..."))

  map(files, readRDS) |>
    list_rbind()
}

compute_transitions <- function(traj_df) {
  # Compute state t -> state t+1 for each play
  traj_df |>
    arrange(game_id, play_id, frame_id) |>
    group_by(game_id, play_id) |>
    mutate(
      state = get_state(separation),
      next_state = lead(state)
    ) |>
    # Drop any transitions where either side is NA
    filter(!is.na(state), !is.na(next_state)) |>
    ungroup()
}

estimate_transition_matrix <- function(transitions) {
  # Enforce consistent state ordering and keep zero-count combos
  transitions <- transitions |>
    mutate(
      state = factor(state, levels = STATE_LEVELS),
      next_state = factor(next_state, levels = STATE_LEVELS)
    )

  # Count transitions (including zero cells)
  counts <- transitions |>
    count(state, next_state, .drop = FALSE)

  # Convert to probabilities by row (current state)
  counts |>
    group_by(state) |>
    mutate(prob = ifelse(sum(n) > 0, n / sum(n), 0)) |>
    ungroup()
}

plot_transition_matrix <- function(trans_probs) {
  ggplot(trans_probs, aes(x = next_state, y = fct_rev(state), fill = prob)) +
    geom_tile() +
    geom_text(aes(label = round(prob, 2)), color = "white") +
    scale_fill_gradient(low = "navy", high = "red") +
    theme_minimal() +
    labs(
      title = "Separation State Transition Probabilities",
      subtitle = "Probability of moving from State Y to State X in next frame",
      x = "Next State",
      y = "Current State",
      fill = "Probability"
    )
}

compute_markov_lift <- function(transitions, features_df) {
  # Calculate how often a specific receiver improves their state vs league average
  # Improvement: Moving to a 'better' state index, or maintaining a good state?
  # Let's define "Positive Outcome" as ending in Open/WideOpen (S2/S3)

  # Actually, simpler metric:
  # For each transition observed for a player (Start -> End),
  # What was the PROBABILITY the league would have made that transition?
  # Or: Difference between Player's transition matrix and League matrix?

  # Metric: "Openness Lift"
  # P(End in S2/S3 | Start in S0/S1) for Player vs League

  # Filter for starts in Tight/Moderate
  relevant_starts <- c("S0_Tight", "S1_Moderate")
  good_ends <- c("S2_Open", "S3_WideOpen")

  # League Baseline
  league_rate <- transitions |>
    filter(state %in% relevant_starts) |>
    summarise(
      success_rate = mean(next_state %in% good_ends)
    ) |>
    pull(success_rate)

  message("League Baseline Success Rate (S0/S1 -> S2/S3): ", round(league_rate, 4))

  player_stats <- transitions |>
    inner_join(features_df, by = c("game_id", "play_id")) |>
    filter(state %in% relevant_starts) |>
    group_by(targeted_id) |>
    summarise(
      n_transitions = n(),
      successes = sum(next_state %in% good_ends),
      player_rate = successes / n_transitions
    ) |>
    filter(n_transitions >= 50) |> # Minimum sample size
    mutate(
      markov_lift = player_rate - league_rate
    ) |>
    arrange(desc(markov_lift))

  player_stats
}

compute_break_counts <- function(transitions, features_df, min_transitions = 50) {
  # Helper to compute per-receiver transition counts and successes for S0/S1 -> S2/S3
  relevant_starts <- c("S0_Tight", "S1_Moderate")
  good_ends <- c("S2_Open", "S3_WideOpen")

  league_rate <- transitions |>
    filter(state %in% relevant_starts) |>
    summarise(
      success_rate = mean(next_state %in% good_ends)
    ) |>
    pull(success_rate)

  player_counts <- transitions |>
    inner_join(features_df, by = c("game_id", "play_id")) |>
    filter(state %in% relevant_starts) |>
    group_by(targeted_id) |>
    summarise(
      n_transitions = n(),
      successes = sum(next_state %in% good_ends),
      .groups = "drop"
    ) |>
    filter(n_transitions >= min_transitions)

  list(
    break_df = player_counts,
    league_rate = league_rate
  )
}

fit_bayesian_markov_model <- function(break_df) {
  # Fit a hierarchical Bayesian model for break-open rates
  if (!requireNamespace("brms", quietly = TRUE)) {
    warning("Package 'brms' not installed; skipping Bayesian Markov Lift model.")
    return(NULL)
  }

  message("Fitting Bayesian Markov model for break-open rates (via brms)...")

  break_df <- break_df |>
    mutate(targeted_id = as.factor(targeted_id))

  bayes_fit <- brms::brm(
    successes | trials(n_transitions) ~ 1 + (1 | targeted_id),
    data = break_df,
    family = binomial(),
    prior = c(
      brms::prior(normal(0, 2), class = "Intercept"),
      brms::prior(exponential(1), class = "sd")
    ),
    iter = 2000,
    warmup = 1000,
    chains = 4,
    cores = min(4, parallel::detectCores()),
    control = list(adapt_delta = 0.95),
    refresh = 0
  )

  bayes_fit
}

extract_bayesian_markov_lift <- function(bayes_model, break_df, league_rate) {
  # Extract posterior summaries for per-receiver break-open probabilities and lift
  if (is.null(bayes_model) || nrow(break_df) == 0) {
    return(NULL)
  }

  # Posterior expected probabilities for each row in break_df
  # posterior_epred returns draws on the probability scale for binomial models
  post_prob <- brms::posterior_epred(
    bayes_model,
    newdata = break_df,
    re_formula = ~ (1 | targeted_id)
  )

  # post_prob: iterations x n_players
  summaries <- t(apply(post_prob, 2, function(draws) {
    c(
      player_rate_mean  = mean(draws),
      player_rate_sd    = sd(draws),
      player_rate_lower = quantile(draws, 0.025),
      player_rate_upper = quantile(draws, 0.975),
      markov_lift_mean  = mean(draws) - league_rate,
      markov_lift_lower = quantile(draws - league_rate, 0.025),
      markov_lift_upper = quantile(draws - league_rate, 0.975)
    )
  }))

  summaries <- as_tibble(summaries)

  tibble(
    targeted_id = as.character(break_df$targeted_id),
    n_transitions = break_df$n_transitions,
    successes = break_df$successes
  ) |>
    bind_cols(summaries) |>
    arrange(desc(markov_lift_mean))
}

# --- Main Execution ---

if (sys.nframe() == 0) {
  if (dir.exists("processed")) {
    PROC_DIR <- "processed"
  } else if (dir.exists("../processed")) {
    PROC_DIR <- "../processed"
  } else {
    stop("Could not find 'processed' directory.")
  }

  message("Loading trajectories...")
  traj <- load_trajectories(PROC_DIR)

  message("Computing transitions...")
  trans <- compute_transitions(traj)

  message("Estimating league transition matrix...")
  tm <- estimate_transition_matrix(trans)
  print(tm)

  # Save plot
  p <- plot_transition_matrix(tm)
  if (!dir.exists("figures")) dir.create("figures")
  ggsave(file.path("figures", "markov_transitions.png"), p, width = 8, height = 6)

  message("Computing Markov Lift for players...")
  features_df <- readRDS(file.path(PROC_DIR, "analysis_full.rds")) |>
    select(game_id, play_id, targeted_id)
  lift <- compute_markov_lift(trans, features_df)

  # Optionally add names if targeted_name is available in analysis data
  df_names <- readRDS(file.path(PROC_DIR, "analysis_full.rds"))
  if ("targeted_name" %in% names(df_names)) {
    name_map <- df_names |>
      distinct(targeted_id, targeted_name) |>
      mutate(targeted_id = as.character(targeted_id))

    lift <- lift |>
      mutate(targeted_id = as.character(targeted_id)) |>
      left_join(name_map, by = "targeted_id") |>
      relocate(targeted_name, .before = targeted_id)
  }

  print(head(lift, 10))

  write_csv(lift, file.path(PROC_DIR, "markov_lift_rankings.csv"))
  message("Saved markov_lift_rankings.csv")

  # --- Bayesian Markov Lift (if brms is available) ---
  message("Fitting Bayesian Markov Lift model (if brms is installed)...")
  break_info <- compute_break_counts(trans, features_df, min_transitions = 50)
  bayes_markov <- fit_bayesian_markov_model(break_info$break_df)
  bayes_lift <- extract_bayesian_markov_lift(bayes_markov, break_info$break_df, break_info$league_rate)

  if (!is.null(bayes_markov) && !is.null(bayes_lift)) {
    if ("targeted_name" %in% names(df_names)) {
      name_map <- df_names |>
        distinct(targeted_id, targeted_name) |>
        mutate(targeted_id = as.character(targeted_id))

      bayes_lift <- bayes_lift |>
        mutate(targeted_id = as.character(targeted_id)) |>
        left_join(name_map, by = "targeted_id") |>
        relocate(targeted_name, .before = targeted_id)
    }

    message("Bayesian Markov Lift model fit successfully.")
    print(head(bayes_lift, 10))

    saveRDS(bayes_markov, file.path(PROC_DIR, "markov_lift_bayes_model.rds"))
    write_csv(bayes_lift, file.path(PROC_DIR, "markov_lift_rankings_bayes.csv"))
  } else {
    message("Bayesian Markov Lift model not fit; skipping Bayesian lift export.")
  }
}
