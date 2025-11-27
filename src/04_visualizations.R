# 04_visualizations.R
# Description: Generate plots and visualizations for reports.

library(tidyverse)
library(ggplot2)

plot_iasa_distribution <- function(data) {
  ggplot(data, aes(x = IASA)) +
    geom_histogram(binwidth = 0.5, fill = "#0072B2", color = "white", alpha = 0.8) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
    theme_minimal() +
    labs(
      title = "Distribution of In-Air Separation Added (IASA)",
      subtitle = "Full Data Analysis",
      x = "IASA (Yards Gained/Lost)",
      y = "Number of Plays",
      caption = "Positive IASA = Separation Gained | Negative IASA = Separation Lost"
    )
}

plot_iasa_vs_airtime <- function(data) {
  ggplot(data, aes(x = air_time, y = IASA)) +
    geom_point(alpha = 0.5, color = "#D55E00") +
    geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
    theme_minimal() +
    labs(
      title = "IASA vs. Air Time",
      subtitle = "Longer throws give defenders more time to close in",
      x = "Air Time (seconds)",
      y = "IASA (Yards)"
    )
}

plot_iasa_vs_throw_separation <- function(data) {
  ggplot(data, aes(x = d_throw, y = IASA)) +
    geom_point(alpha = 0.5, color = "#009E73") +
    geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
    theme_minimal() +
    labs(
      title = "IASA vs. Initial Separation at Throw",
      subtitle = "Regression to the mean: Open receivers tend to lose separation",
      x = "Separation at Throw (Yards)",
      y = "IASA (Yards)"
    )
}

# New: IASA by route of targeted receiver
plot_iasa_by_route <- function(data, min_plays = 50) {
  data |>
    filter(!is.na(route_of_targeted_receiver)) |>
    group_by(route_of_targeted_receiver) |>
    summarise(
      n = n(),
      avg_IASA = mean(IASA, na.rm = TRUE)
    ) |>
    filter(n >= min_plays) |>
    arrange(desc(avg_IASA)) |>
    mutate(route_of_targeted_receiver = fct_reorder(route_of_targeted_receiver, avg_IASA)) |>
    ggplot(aes(x = route_of_targeted_receiver, y = avg_IASA)) +
    geom_col(fill = "#0072B2") +
    coord_flip() +
    theme_minimal() +
    labs(
      title = "Average IASA by Route Type",
      subtitle = paste("Routes with at least", min_plays, "plays"),
      x = "Route of Targeted Receiver",
      y = "Average IASA (Yards)"
    )
}

# New: IASA by coverage type (man/zone)
plot_iasa_by_coverage <- function(data, min_plays = 50) {
  data |>
    filter(!is.na(team_coverage_man_zone)) |>
    group_by(team_coverage_man_zone) |>
    summarise(
      n = n(),
      avg_IASA = mean(IASA, na.rm = TRUE)
    ) |>
    filter(n >= min_plays) |>
    ggplot(aes(x = fct_reorder(team_coverage_man_zone, avg_IASA), y = avg_IASA)) +
    geom_col(fill = "#D55E00") +
    coord_flip() +
    theme_minimal() +
    labs(
      title = "Average IASA by Coverage Family",
      subtitle = paste("Man vs Zone concepts (at least", min_plays, "plays)"),
      x = "Coverage (Man/Zone)",
      y = "Average IASA (Yards)"
    )
}

# --- New: Bayesian interval plots for report ---

plot_bayesian_iasa_intervals <- function(rankings_bayes, top_n = 15) {
  # Visualize posterior mean and 95% CrI for IASA over expected
  if (!"iasa_over_expected_mean" %in% names(rankings_bayes)) {
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
    filter(!is.na(iasa_over_expected_mean)) |>
    slice_max(iasa_over_expected_mean, n = top_n, with_ties = FALSE) |>
    mutate(label = fct_reorder(label, iasa_over_expected_mean)) |>
    ggplot(aes(x = label, y = iasa_over_expected_mean)) +
    geom_point(color = "#0072B2") +
    geom_errorbar(
      aes(ymin = iasa_over_expected_lower, ymax = iasa_over_expected_upper),
      width = 0
    ) +
    coord_flip() +
    theme_minimal() +
    labs(
      title = "Top Receivers by IASA Over Expected (Bayesian)",
      subtitle = "Posterior mean and 95% credible intervals",
      x = "Receiver",
      y = "IASA Over Expected (Yards)"
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
      stop(paste(analysis_path, "not found. Run 02_compute_IASA.R first."))
    }
  }

  df <- readRDS(analysis_path)

  # Check for missing IASA values
  missing_iasa <- sum(is.na(df$IASA))
  if (missing_iasa > 0) {
    message(sprintf("Note: Removing %d rows where IASA could not be calculated (likely due to missing defender tracking data in output).", missing_iasa))
    df <- df[!is.na(df$IASA), ]
  }

  # Determine output directory
  FIG_DIR <- get_fig_dir()

  p1 <- plot_iasa_distribution(df)
  ggsave(file.path(FIG_DIR, "iasa_distribution.png"), p1, width = 8, height = 5)

  p2 <- plot_iasa_vs_airtime(df)
  ggsave(file.path(FIG_DIR, "iasa_vs_airtime.png"), p2, width = 8, height = 5)

  p3 <- plot_iasa_vs_throw_separation(df)
  ggsave(file.path(FIG_DIR, "iasa_vs_dthrow.png"), p3, width = 8, height = 5)

  # New contextual plots if supplementary data is present
  if ("route_of_targeted_receiver" %in% names(df)) {
    p4 <- plot_iasa_by_route(df)
    ggsave(file.path(FIG_DIR, "iasa_by_route.png"), p4, width = 8, height = 6)
  }

  if ("team_coverage_man_zone" %in% names(df)) {
    p5 <- plot_iasa_by_coverage(df)
    ggsave(file.path(FIG_DIR, "iasa_by_coverage.png"), p5, width = 8, height = 6)
  }

  # Bayesian IASA over expected intervals (if available)
  bayes_iasa_path <- file.path(PROC_DIR, "receiver_rankings_bayes.csv")
  if (file.exists(bayes_iasa_path)) {
    rankings_bayes <- read_csv(bayes_iasa_path, show_col_types = FALSE)
    p6 <- plot_bayesian_iasa_intervals(rankings_bayes, top_n = 15)
    if (!is.null(p6)) {
      ggsave(file.path(FIG_DIR, "receiver_iasa_bayes_intervals.png"), p6, width = 8, height = 6)
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
