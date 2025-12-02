# 04_visualizations.R
# Description: Generate plots and visualizations for reports.

library(tidyverse)
library(ggplot2)

plot_BFD_distribution <- function(data) {
  ggplot(data, aes(x = BFD)) +
    geom_histogram(binwidth = 0.5, fill = "#0072B2", color = "white", alpha = 0.8) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
    theme_minimal() +
    labs(
      title = "Distribution of Ball-Flight Differential added (BFD)",
      subtitle = "Full Data Analysis",
      x = "BFD (Yards Gained/Lost)",
      y = "Number of Plays",
      caption = "Positive BFD = Separation Gained, Negative BFD = Separation Lost"
    )
}

plot_BFD_vs_airtime <- function(data) {
  ggplot(data, aes(x = air_time, y = BFD)) +
    geom_point(alpha = 0.5, color = "#D55E00") +
    geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
    theme_minimal() +
    labs(
      title = "BFD vs. Air Time",
      subtitle = "Longer throws give defenders more time to close in",
      x = "Air Time (seconds)",
      y = "BFD (Yards)"
    )
}

plot_BFD_vs_throw_separation <- function(data) {
  ggplot(data, aes(x = d_throw, y = BFD)) +
    geom_point(alpha = 0.5, color = "#009E73") +
    geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
    theme_minimal() +
    labs(
      title = "BFD vs. Initial Differential at Throw",
      subtitle = "Regression to the mean: Open receivers tend to lose separation",
      x = "Separation at Throw (Yards)",
      y = "BFD (Yards)"
    )
}

# BFD by route of targeted receiver
plot_BFD_by_route <- function(data, min_plays = 50) {
  data |>
    filter(!is.na(route_of_targeted_receiver)) |>
    group_by(route_of_targeted_receiver) |>
    summarise(
      n = n(),
      avg_BFD = mean(BFD, na.rm = TRUE)
    ) |>
    filter(n >= min_plays) |>
    arrange(desc(avg_BFD)) |>
    mutate(route_of_targeted_receiver = fct_reorder(route_of_targeted_receiver, avg_BFD)) |>
    ggplot(aes(x = route_of_targeted_receiver, y = avg_BFD)) +
    geom_col(fill = "#0072B2") +
    coord_flip() +
    theme_minimal() +
    labs(
      title = "Average BFD by Route Type",
      subtitle = paste("Routes with at least", min_plays, "plays"),
      x = "Route of Targeted Receiver",
      y = "Average BFD (Yards)"
    )
}

# BFD by coverage type (man/zone)
plot_BFD_by_coverage <- function(data, min_plays = 50) {
  data |>
    filter(!is.na(team_coverage_man_zone)) |>
    group_by(team_coverage_man_zone) |>
    summarise(
      n = n(),
      avg_BFD = mean(BFD, na.rm = TRUE)
    ) |>
    filter(n >= min_plays) |>
    ggplot(aes(x = fct_reorder(team_coverage_man_zone, avg_BFD), y = avg_BFD)) +
    geom_col(fill = "#D55E00") +
    coord_flip() +
    theme_minimal() +
    labs(
      title = "Average BFD by Coverage Family",
      subtitle = paste("Man vs Zone concepts (at least", min_plays, "plays)"),
      x = "Coverage (Man/Zone)",
      y = "Average BFD (Yards)"
    )
}

# --- New: Bayesian interval plots for report ---

plot_bayesian_BFD_intervals <- function(rankings_bayes, top_n = 15) {
  # Visualize posterior mean and 95% CrI for BFD over expected
  if (!"BFD_over_expected_mean" %in% names(rankings_bayes)) {
    return(NULL)
  }

  if ("targeted_name" %in% names(rankings_bayes)) {
    rankings_bayes <- rankings_bayes |>
      mutate(label = targeted_name)
  } else {
    rankings_bayes <- rankings_bayes |>
      mutate(label = as.character(nfl_id))
  }

  rankings_bayes |>
    filter(!is.na(BFD_over_expected_mean)) |>
    slice_max(BFD_over_expected_mean, n = top_n, with_ties = FALSE) |>
    mutate(label = fct_reorder(label, BFD_over_expected_mean)) |>
    ggplot(aes(x = label, y = BFD_over_expected_mean)) +
    geom_point(color = "#0072B2") +
    geom_errorbar(
      aes(ymin = BFD_over_expected_lower, ymax = BFD_over_expected_upper),
      width = 0
    ) +
    coord_flip() +
    theme_minimal() +
    labs(
      title = "Top Receivers by BFD Over Expected (Bayesian)",
      subtitle = "Posterior mean and 95% credible intervals",
      x = "Receiver",
      y = "BFD Over Expected (Yards)"
    )
}

plot_bayesian_markov_intervals <- function(lift_bayes, top_n = 15) {
  # Visualize posterior mean and 95% CrI for break-open rates (Markov Lift)
  if (!"player_rate_mean" %in% names(lift_bayes)) {
    return(NULL)
  }

  # Handle both legacy and current column names for interval bounds
  # Legacy CSVs may have `player_rate_lower.2.5%` / `player_rate_upper.97.5%`.
  if ("player_rate_lower.2.5%" %in% names(lift_bayes) &&
    !"player_rate_lower" %in% names(lift_bayes)) {
    lift_bayes <- lift_bayes |>
      dplyr::rename(
        player_rate_lower = `player_rate_lower.2.5%`,
        player_rate_upper = `player_rate_upper.97.5%`
      )
  }

  # Use player name when available
  if ("targeted_name" %in% names(lift_bayes)) {
    lift_bayes <- lift_bayes |>
      mutate(label = targeted_name)
  } else {
    lift_bayes <- lift_bayes |>
      mutate(label = as.character(targeted_id))
  }

  # Ensure interval columns exist (they will, given how we write the CSV)
  lift_bayes |>
    filter(!is.na(player_rate_mean)) |>
    slice_max(player_rate_mean, n = top_n, with_ties = FALSE) |>
    mutate(
      label = fct_reorder(label, player_rate_mean),
      player_rate_pct = player_rate_mean
    ) |>
    ggplot(aes(x = label, y = player_rate_pct)) +
    geom_point(color = "#D55E00") +
    geom_errorbar(
      aes(
        ymin = player_rate_lower,
        ymax = player_rate_upper
      ),
      width = 0
    ) +
    coord_flip() +
    theme_minimal() +
    labs(
      title = "Top Receivers by Break-Open Rate (Bayesian Markov Lift)",
      subtitle = "Per-frame break-open probability with 95% credible intervals",
      x = "Receiver",
      y = "Break-Open Probability (%)"
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

  # Determine input directory
  PROC_DIR <- get_proc_dir()

  analysis_path <- file.path(PROC_DIR, "analysis_full.rds")
  if (!file.exists(analysis_path)) {
    # Fallback to week 1 if full analysis not run yet
    analysis_path_w1 <- file.path(PROC_DIR, "analysis_w01.rds")
    if (file.exists(analysis_path_w1)) {
      warning("analysis_full.rds not found, using analysis_w01.rds")
      analysis_path <- analysis_path_w1
    } else {
      stop(paste(analysis_path, "not found. Run 02_compute_IASA.R (now BFD) first."))
    }
  }

  df <- readRDS(analysis_path)

  # Optional: align visuals with modeling sample
  # Keep only WR/TE targets on forward passes when fields are available
  if (all(c("pass_length", "targeted_position") %in% names(df))) {
    message("Filtering visuals to WR/TE forward-pass targets for consistency with modeling...")
    df <- df |>
      filter(
        !is.na(pass_length),
        pass_length > 0,
        targeted_position %in% c("WR", "TE")
      )
  }

  # Check for missing BFD values
  missing_BFD <- sum(is.na(df$BFD))
  if (missing_BFD > 0) {
    message(sprintf("Note: Removing %d rows where BFD could not be calculated (likely due to missing defender tracking data in output).", missing_BFD))
    df <- df[!is.na(df$BFD), ]
  }

  # Determine output directory
  FIG_DIR <- get_fig_dir()

  p1 <- plot_BFD_distribution(df)
  ggsave(file.path(FIG_DIR, "BFD_distribution.png"), p1, width = 8, height = 5)

  p2 <- plot_BFD_vs_airtime(df)
  ggsave(file.path(FIG_DIR, "BFD_vs_airtime.png"), p2, width = 8, height = 5)

  p3 <- plot_BFD_vs_throw_separation(df)
  ggsave(file.path(FIG_DIR, "BFD_vs_dthrow.png"), p3, width = 8, height = 5)

  # New contextual plots if supplementary data is present
  if ("route_of_targeted_receiver" %in% names(df)) {
    p4 <- plot_BFD_by_route(df)
    ggsave(file.path(FIG_DIR, "BFD_by_route.png"), p4, width = 8, height = 6)
  }

  if ("team_coverage_man_zone" %in% names(df)) {
    p5 <- plot_BFD_by_coverage(df)
    ggsave(file.path(FIG_DIR, "BFD_by_coverage.png"), p5, width = 8, height = 6)
  }

  # Bayesian BFD over expected intervals (if available)
  bayes_BFD_path <- file.path(PROC_DIR, "receiver_rankings_bayes.csv")
  if (file.exists(bayes_BFD_path)) {
    rankings_bayes <- read_csv(bayes_BFD_path, show_col_types = FALSE)
    p6 <- plot_bayesian_BFD_intervals(rankings_bayes, top_n = 15)
    if (!is.null(p6)) {
      ggsave(file.path(FIG_DIR, "receiver_BFD_bayes_intervals.png"), p6, width = 8, height = 6)
    }
  }

  # Bayesian Markov Lift intervals (if available)
  bayes_markov_path <- file.path(PROC_DIR, "markov_lift_rankings_bayes.csv")
  if (file.exists(bayes_markov_path)) {
    lift_bayes <- read_csv(bayes_markov_path, show_col_types = FALSE)
    p7 <- plot_bayesian_markov_intervals(lift_bayes, top_n = 15)
    if (!is.null(p7)) {
      ggsave(file.path(FIG_DIR, "markov_lift_bayes_intervals.png"), p7, width = 8, height = 6)
    }
  }

  message(paste("Saved figures to", FIG_DIR))
}
