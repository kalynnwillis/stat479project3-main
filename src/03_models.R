# 03_models.R
# Description: Statistical and ML modeling of IASA.
# Goal: Isolate receiver ability (random intercept) controlling for context.

library(tidyverse)
library(lme4)
library(broom.mixed) # For tidy model outputs

# --- Functions ---

fit_mixed_effects_model <- function(data) {
  # Model IASA controlling for:
  # - Air Time (longer throws allow more convergence)
  # - Initial Separation (regression to the mean)
  # - Route / coverage context and defensive structure
  # - Random Effect: Receiver ID (The metric we want)
  # - Random Effect: Game ID (Game-specific conditions/weather)

  message("Fitting Mixed-Effects Model: IASA ~ air_time + d_throw + pass_length + defenders_in_the_box +\n  team_coverage_man_zone + route_of_targeted_receiver + (1|targeted_id) + (1|game_id)...")

  m <- lmer(
    IASA ~ air_time + d_throw + pass_length + defenders_in_the_box +
      team_coverage_man_zone + route_of_targeted_receiver +
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
    rename(iasa_over_expected = `(Intercept)`) |>
    arrange(desc(iasa_over_expected))
}

fit_bayesian_iasa_model <- function(data) {
  # Fit a Bayesian analogue of the mixed-effects model (partial pooling over receivers/games)
  if (!requireNamespace("brms", quietly = TRUE)) {
    warning("Package 'brms' not installed; skipping Bayesian IASA model.")
    return(NULL)
  }

  message("Fitting Bayesian Mixed-Effects Model for IASA (via brms)...")

  bayes_fit <- brms::brm(
    IASA ~ air_time + d_throw + pass_length + defenders_in_the_box +
      team_coverage_man_zone + route_of_targeted_receiver +
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
  # Extract posterior mean and 95% CrI for receiver random intercepts (IASA over expected)
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
      iasa_over_expected_mean = mean(vals),
      iasa_over_expected_sd = sd(vals),
      iasa_over_expected_lower = quantile(vals, 0.025),
      iasa_over_expected_upper = quantile(vals, 0.975)
    )
  })

  summary_df <- list_rbind(summary_list)

  if (is.null(summary_df) || nrow(summary_df) == 0) {
    warning("Could not locate any 'r_targeted_id' random effect columns in Bayesian draws; skipping Bayesian receiver rankings.")
    return(NULL)
  }

  summary_df |>
    arrange(desc(iasa_over_expected_mean))
}

# --- Main Execution ---

if (sys.nframe() == 0) {
  # Determine input/output directory
  if (dir.exists("processed")) {
    PROC_DIR <- "processed"
  } else if (dir.exists("../processed")) {
    PROC_DIR <- "../processed"
  } else {
    stop("Could not find 'processed' directory.")
  }

  # Load FULL dataset
  analysis_path <- file.path(PROC_DIR, "analysis_full.rds")
  if (!file.exists(analysis_path)) {
    # Fallback to W1 if full not ready
    if (file.exists(file.path(PROC_DIR, "analysis_w01.rds"))) {
      warning("analysis_full.rds not found. Using Week 1 data.")
      analysis_path <- file.path(PROC_DIR, "analysis_w01.rds")
    } else {
      stop("No analysis data found. Run 02_compute_IASA.R first.")
    }
  }

  df <- readRDS(analysis_path)

  # Ensure supplementary categorical variables exist and are usable (no NAs, factors)
  if (!"offense_formation" %in% names(df)) df$offense_formation <- NA_character_
  if (!"receiver_alignment" %in% names(df)) df$receiver_alignment <- NA_character_
  if (!"route_of_targeted_receiver" %in% names(df)) df$route_of_targeted_receiver <- NA_character_
  if (!"team_coverage_man_zone" %in% names(df)) df$team_coverage_man_zone <- NA_character_
  if (!"team_coverage_type" %in% names(df)) df$team_coverage_type <- NA_character_

  df <- df |>
    mutate(
      offense_formation = factor(replace_na(offense_formation, "Unknown")),
      receiver_alignment = factor(replace_na(receiver_alignment, "Unknown")),
      route_of_targeted_receiver = factor(replace_na(route_of_targeted_receiver, "Unknown")),
      team_coverage_man_zone = factor(replace_na(team_coverage_man_zone, "Unknown")),
      team_coverage_type = factor(replace_na(team_coverage_type, "Unknown"))
    )

  # --- Debugging Missing Values ---
  message(paste("Total rows loaded:", nrow(df)))

  missing_stats <- df |>
    summarise(
      missing_IASA = sum(is.na(IASA)),
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
  df_model <- df |>
    drop_na(IASA, air_time, d_throw, targeted_id, pass_length, defenders_in_the_box) |>
    filter(air_time >= 0) |> # Allow 0 air time? No, but >= just in case floating point issues
    mutate(
      targeted_id = as.factor(targeted_id),
      game_id = as.factor(game_id)
    )

  message(paste("Fitting model on", nrow(df_model), "plays."))

  # NOTE: If the dataset is small (e.g., only Week 1 valid plays),
  # random effects might be estimated as zero (singular fit).
  # This is expected with sparse data.

  # Fit Model
  mem_model <- fit_mixed_effects_model(df_model)

  # Print Summary
  print(summary(mem_model))

  # --- Model validation metrics (in-sample) ---
  y_obs <- df_model$IASA
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
    IASA = y_obs,
    fitted = y_hat,
    resid = resid
  )

  # Determine figure directory
  if (dir.exists("figures")) {
    FIG_DIR <- "figures"
  } else if (dir.exists("../figures")) {
    FIG_DIR <- "../figures"
  } else {
    dir.create("../figures", showWarnings = FALSE)
    FIG_DIR <- "../figures"
  }

  p_fit <- ggplot(diag_df, aes(x = fitted, y = IASA)) +
    geom_point(alpha = 0.3, color = "#0072B2") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    theme_minimal() +
    labs(
      title = "Mixed-Effects Model: Fitted vs Observed IASA",
      x = "Fitted IASA",
      y = "Observed IASA"
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
      title = "Mixed-Effects Model Residuals (IASA)",
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

  # Join with player names from analysis data if available
  if ("targeted_name" %in% names(df)) {
    name_map <- df |>
      distinct(targeted_id, targeted_name) |>
      mutate(targeted_id = as.character(targeted_id))

    rankings <- rankings |>
      left_join(name_map, by = c("nfl_id" = "targeted_id")) |>
      relocate(targeted_name, .before = nfl_id)
  }

  print(head(rankings, 10))

  # Fit Bayesian version of the mixed-effects model (if brms is available)
  bayes_model <- fit_bayesian_iasa_model(df_model)
  bayes_rankings <- extract_bayesian_receiver_rankings(bayes_model)

  if (!is.null(bayes_model) && !is.null(bayes_rankings)) {
    if ("targeted_name" %in% names(df)) {
      name_map <- df |>
        distinct(targeted_id, targeted_name) |>
        mutate(targeted_id = as.character(targeted_id))

      bayes_rankings <- bayes_rankings |>
        left_join(name_map, by = c("nfl_id" = "targeted_id")) |>
        relocate(targeted_name, .before = nfl_id)
    }

    message("Bayesian mixed-effects model for IASA fit successfully.")
    print(head(bayes_rankings, 10))

    saveRDS(bayes_model, file.path(PROC_DIR, "mixed_effects_model_bayes.rds"))
    write_csv(bayes_rankings, file.path(PROC_DIR, "receiver_rankings_bayes.csv"))
  } else {
    message("Bayesian IASA model not fit; skipping Bayesian rankings export.")
  }

  # Save outputs
  saveRDS(mem_model, file.path(PROC_DIR, "mixed_effects_model.rds"))
  write_csv(rankings, file.path(PROC_DIR, "receiver_rankings.csv"))
  write_csv(mem_metrics, file.path(PROC_DIR, "mixed_effects_metrics.csv"))

  message(paste("Saved model and rankings to", PROC_DIR))
}
