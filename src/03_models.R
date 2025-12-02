# 03_models.R
# Description: Statistical and ML modeling of BFD.
# Goal: Isolate receiver ability (random intercept) controlling for context.

library(tidyverse)
library(lme4)
library(broom.mixed) # For tidy model outputs

# --- Functions ---

fit_mixed_effects_model <- function(data) {
  # Model BFD controlling for:
  # - Air Time (longer throws allow more convergence)
  # - Initial Differential (regression to the mean)
  # - Route / coverage context and defensive structure
  # - Random Effect: Receiver ID (The metric we want)
  # - Random Effect: Game ID (Game-specific conditions/weather)

  message("Fitting Mixed-Effects Model: BFD ~ air_time_scaled + d_throw_scaled + pass_length_scaled + defenders_in_the_box_scaled +\n  team_coverage_man_zone + route_of_targeted_receiver + targeted_position + (1|targeted_id) + (1|game_id)...")

  m <- lmer(
    BFD ~ air_time_scaled + d_throw_scaled + pass_length_scaled + defenders_in_the_box_scaled +
      team_coverage_man_zone + route_of_targeted_receiver + targeted_position +
      (1 | targeted_id) + (1 | game_id),
    data = data,
    REML = FALSE
  )
  return(m)
}

extract_receiver_rankings <- function(model) {
  # Extract random intercepts for receivers (targeted_id)
  ranefs <- ranef(model)$targeted_id

  ranefs |>
    as_tibble(rownames = "nfl_id") |>
    rename(BFD_over_expected = `(Intercept)`) |>
    arrange(desc(BFD_over_expected))
}

fit_bayesian_BFD_model <- function(data) {
  # Fit a Bayesian analogue of the mixed-effects model (partial pooling over receivers/games)
  if (!requireNamespace("brms", quietly = TRUE)) {
    warning("Package 'brms' not installed; skipping Bayesian BFD model.")
    return(NULL)
  }

  message("Fitting Bayesian Mixed-Effects Model for BFD (via brms)...")

  bayes_fit <- brms::brm(
    BFD ~ air_time + d_throw + pass_length + defenders_in_the_box +
      team_coverage_man_zone + route_of_targeted_receiver + targeted_position +
      (1 | targeted_id) + (1 | game_id),
    data = data,
    family = gaussian(),
    prior = c(
      brms::prior(normal(0, 2), class = "Intercept"),
      brms::prior(normal(0, 1), class = "b"),
      brms::prior(exponential(1), class = "sd"),
      brms::prior(exponential(1), class = "sigma")
    ),
    iter = 4000,
    warmup = 1000,
    chains = 4,
    cores = min(4, parallel::detectCores()),
    control = list(adapt_delta = 0.95),
    refresh = 0
  )

  bayes_fit
}

extract_bayesian_receiver_rankings <- function(bayes_model) {
  # Extract posterior mean and 95% CrI for receiver random intercepts (BFD over expected)
  # NOTE: This helper is intentionally conservative; if the structure of the
  # brms object is not as expected, it returns NULL and the pipeline falls
  # back to frequentist rankings only.
  if (is.null(bayes_model)) {
    return(NULL)
  }

  # Use posterior draws for random effects; this is more robust to version
  # differences than relying on summary dimnames.
  if (!requireNamespace("posterior", quietly = TRUE)) {
    warning("Package 'posterior' not available; skipping Bayesian receiver rankings.")
    return(NULL)
  }

  draws <- brms::as_draws_df(bayes_model)

  # Levels of the targeted receiver random effect in the model data
  if (!"targeted_id" %in% names(bayes_model$data)) {
    warning("Model data does not contain 'targeted_id'; skipping Bayesian receiver rankings.")
    return(NULL)
  }

  receiver_ids <- unique(as.character(bayes_model$data$targeted_id))

  summary_list <- purrr::map(receiver_ids, function(rid) {
    cand_cols <- c(
      paste0("r_targeted_id[", rid, ",Intercept]"),
      paste0("r_targeted_id[", rid, ",(Intercept)]")
    )
    col_name <- cand_cols[cand_cols %in% names(draws)][1]
    if (is.na(col_name)) {
      return(NULL)
    }
    vals <- draws[[col_name]]
    tibble(
      nfl_id = rid,
      BFD_over_expected_mean = mean(vals),
      BFD_over_expected_sd = sd(vals),
      BFD_over_expected_lower = quantile(vals, 0.025),
      BFD_over_expected_upper = quantile(vals, 0.975)
    )
  })

  summary_list <- purrr::compact(summary_list)

  if (length(summary_list) == 0) {
    warning("No valid posterior random effects found for targeted_id; skipping Bayesian receiver rankings.")
    return(NULL)
  }

  summary_df <- dplyr::bind_rows(summary_list)

  if (is.null(summary_df) || nrow(summary_df) == 0) {
    warning("Could not locate any 'r_targeted_id' random effect columns in Bayesian draws; skipping Bayesian receiver rankings.")
    return(NULL)
  }

  summary_df |>
    arrange(desc(BFD_over_expected_mean))
}

# --- Main Execution ---

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

  # Load FULL dataset
  analysis_path <- file.path(PROC_DIR, "analysis_full.rds")
  if (!file.exists(analysis_path)) {
    # Fallback to W1 if full not ready
    if (file.exists(file.path(PROC_DIR, "analysis_w01.rds"))) {
      warning("analysis_full.rds not found. Using Week 1 data.")
      analysis_path <- file.path(PROC_DIR, "analysis_w01.rds")
    } else {
      stop("No analysis data found. Run 02_compute_IASA.R (now BFD) first.")
    }
  }

  df <- readRDS(analysis_path)

  # Ensure supplementary categorical variables exist and are usable (no NAs, factors)
  if (!"offense_formation" %in% names(df)) df$offense_formation <- NA_character_
  if (!"receiver_alignment" %in% names(df)) df$receiver_alignment <- NA_character_
  if (!"route_of_targeted_receiver" %in% names(df)) df$route_of_targeted_receiver <- NA_character_
  if (!"team_coverage_man_zone" %in% names(df)) df$team_coverage_man_zone <- NA_character_
  if (!"team_coverage_type" %in% names(df)) df$team_coverage_type <- NA_character_

  # Ensure pass / position fields exist (needed for football-correct filtering)
  if (!"pass_length" %in% names(df)) df$pass_length <- NA_real_
  if (!"pass_result" %in% names(df)) df$pass_result <- NA_character_
  if (!"targeted_position" %in% names(df)) df$targeted_position <- NA_character_

  df <- df |>
    mutate(
      offense_formation = factor(replace_na(offense_formation, "Unknown")),
      receiver_alignment = factor(replace_na(receiver_alignment, "Unknown")),
      route_of_targeted_receiver = factor(replace_na(route_of_targeted_receiver, "Unknown")),
      team_coverage_man_zone = factor(replace_na(team_coverage_man_zone, "Unknown")),
      team_coverage_type = factor(replace_na(team_coverage_type, "Unknown")),
      targeted_position = replace_na(targeted_position, "Unknown")
    )

  # --- Football-correct filtering (plays kept for modeling) ---
  # 1) Forward passes only (screens / pitches dropped via pass_length > 0)
  # 2) Route-running positions only (WR / TE)

  df_filtered <- df |>
    filter(
      !is.na(pass_length),
      pass_length > 0,
      targeted_position %in% c("WR", "TE")
    )

  message(paste("Total rows loaded:", nrow(df)))
  message(paste("Rows after football filters (forward passes, WR/TE):", nrow(df_filtered)))

  # --- Debugging Missing Values (post-filter) ---

  missing_stats <- df_filtered |>
    summarise(
      missing_BFD = sum(is.na(BFD)),
      missing_air_time = sum(is.na(air_time)),
      missing_d_throw = sum(is.na(d_throw)),
      missing_d_catch = sum(is.na(d_catch)),
      missing_targeted_id = sum(is.na(targeted_id)),
      zero_air_time = sum(air_time == 0, na.rm = TRUE),
      negative_air_time = sum(air_time < 0, na.rm = TRUE)
    )

  print("Missing Value Counts:")
  print(missing_stats)

  # Filter for valid data
  # IMPORTANT: Ensure targeted_id is a factor for random effects
  df_model <- df_filtered |>
    drop_na(BFD, air_time, d_throw, targeted_id, pass_length, defenders_in_the_box) |>
    filter(air_time >= 0) |> # Allow 0 air time? No, but >= just in case floating point issues
    mutate(
      targeted_id = as.factor(targeted_id),
      game_id = as.factor(game_id),
      targeted_position = factor(targeted_position),
      air_time_scaled = scale(air_time)[, 1],
      d_throw_scaled = scale(d_throw)[, 1],
      pass_length_scaled = scale(pass_length)[, 1],
      defenders_in_the_box_scaled = scale(defenders_in_the_box)[, 1]
    )

  message(paste("Fitting model on", nrow(df_model), "plays."))

  # NOTE: If the dataset is small (e.g., only Week 1 valid plays),
  # random effects might be estimated as zero (singular fit).
  # This is expected with sparse data.

  # Fit Model
  mem_model <- fit_mixed_effects_model(df_model)

  if (lme4::isSingular(mem_model)) {
    message("Warning: mixed-effects model fit is singular.")
  }

  # Print Summary
  print(summary(mem_model))

  # --- Model validation metrics (in-sample) ---
  y_obs <- df_model$BFD
  y_hat <- fitted(mem_model)
  resid <- y_obs - y_hat

  rmse <- sqrt(mean(resid^2, na.rm = TRUE))
  mae <- mean(abs(resid), na.rm = TRUE)
  r2 <- 1 - var(resid, na.rm = TRUE) / var(y_obs, na.rm = TRUE)

  mem_metrics <- tibble(
    metric = c("RMSE", "MAE", "Pseudo_R2"),
    value  = c(rmse, mae, r2)
  )

  message("Mixed-effects model metrics:")
  print(mem_metrics)

  # --- Diagnostic figures ---
  diag_df <- tibble(
    BFD = y_obs,
    fitted = y_hat,
    resid = resid
  )

  # Determine figure directory
  FIG_DIR <- get_fig_dir()

  p_fit <- ggplot(diag_df, aes(x = fitted, y = BFD)) +
    geom_point(alpha = 0.3, color = "#0072B2") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    theme_minimal() +
    labs(
      title = "Mixed-Effects Model: Fitted vs Observed BFD",
      x = "Fitted BFD",
      y = "Observed BFD"
    )

  ggsave(
    filename = file.path(FIG_DIR, "mixed_effects_fitted_vs_observed.png"),
    plot = p_fit,
    width = 8, height = 5, dpi = 300
  )

  p_resid <- ggplot(diag_df, aes(x = resid)) +
    geom_histogram(bins = 40, fill = "#D55E00", color = "white", alpha = 0.8) +
    theme_minimal() +
    labs(
      title = "Mixed-Effects Model Residuals (BFD)",
      x = "Residual",
      y = "Count"
    )

  ggsave(
    filename = file.path(FIG_DIR, "mixed_effects_residuals_hist.png"),
    plot = p_resid,
    width = 8, height = 5, dpi = 300
  )

  # Extract and Save Rankings
  rankings <- extract_receiver_rankings(mem_model)

  # Target counts (forward-pass targets per receiver used in modeling)
  target_counts <- df_model |>
    group_by(targeted_id) |>
    summarise(
      n_targets = n(),
      .groups = "drop"
    ) |>
    mutate(targeted_id = as.character(targeted_id))

  # Join with player names / positions from the (filtered) analysis data if available
  meta_map <- df_filtered |>
    distinct(targeted_id, targeted_name, targeted_position) |>
    mutate(targeted_id = as.character(targeted_id))

  rankings_full <- rankings |>
    left_join(meta_map, by = c("nfl_id" = "targeted_id")) |>
    left_join(target_counts, by = c("nfl_id" = "targeted_id")) |>
    relocate(targeted_name, .before = nfl_id) |>
    relocate(targeted_position, .after = targeted_name)

  rankings_qualified <- rankings_full |>
    filter(n_targets >= 30) |>
    arrange(desc(BFD_over_expected))

  print(head(rankings_qualified, 10))

  # Fit Bayesian version of the mixed-effects model (if brms is available)
  bayes_model <- fit_bayesian_BFD_model(df_model)
  bayes_rankings <- extract_bayesian_receiver_rankings(bayes_model)

  if (!is.null(bayes_model) && !is.null(bayes_rankings)) {
    bayes_rankings_full <- bayes_rankings |>
      left_join(meta_map, by = c("nfl_id" = "targeted_id")) |>
      left_join(target_counts, by = c("nfl_id" = "targeted_id")) |>
      relocate(targeted_name, .before = nfl_id) |>
      relocate(targeted_position, .after = targeted_name)

    bayes_rankings_qualified <- bayes_rankings_full |>
      filter(n_targets >= 30) |>
      arrange(desc(BFD_over_expected_mean))

    message("Bayesian mixed-effects model for BFD fit successfully.")
    print(head(bayes_rankings_qualified, 10))

    saveRDS(bayes_model, file.path(PROC_DIR, "mixed_effects_model_bayes.rds"))
    write_csv(bayes_rankings_qualified, file.path(PROC_DIR, "receiver_rankings_bayes.csv"))
  } else {
    message("Bayesian BFD model not fit; skipping Bayesian rankings export.")
  }

  # Save outputs
  saveRDS(mem_model, file.path(PROC_DIR, "mixed_effects_model.rds"))
  write_csv(rankings_qualified, file.path(PROC_DIR, "receiver_rankings.csv"))
  write_csv(mem_metrics, file.path(PROC_DIR, "mixed_effects_metrics.csv"))

  message(paste("Saved model and rankings to", PROC_DIR))
}
