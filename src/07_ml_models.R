# 07_ml_models.R
# Description: Predict Final Separation using Gradient Boosting (XGBoost).
# Goal: Predict d_catch (or IASA) using pre-throw features.

library(tidyverse)
library(xgboost)
library(caret)

# --- Functions ---

prepare_ml_data <- function(analysis_df) {
  # Select features for ML
  # We want to predict d_catch (or IASA)
  # Features: core separation metrics + route / coverage / context
  
  # Only use columns that actually exist (for robustness)
  desired_cols <- c(
    "d_catch",      # target
    "d_throw",
    "air_time",
    "pass_length",
    "defenders_in_the_box",
    "expected_points",
    "expected_points_added",
    "yards_gained",
    "offense_formation",
    "receiver_alignment",
    "route_of_targeted_receiver",
    "team_coverage_man_zone",
    "team_coverage_type",
    "pass_location_type",
    "dropback_type"
  )
  
  available <- intersect(desired_cols, names(analysis_df))
  
  ml_df <- analysis_df |>
    select(all_of(available)) |>
    # Ensure target is present
    filter(!is.na(d_catch), !is.na(d_throw), !is.na(air_time)) |>
    # Handle categorical predictors as factors with explicit 'Unknown' level
    mutate(
      across(
        where(is.character),
        ~ factor(replace_na(.x, "Unknown"))
      )
    )
  
  ml_df
}

train_xgboost <- function(ml_df) {
  # Split Data
  set.seed(123)
  train_index <- createDataPartition(ml_df$d_catch, p = 0.8, list = FALSE)
  train_data <- ml_df[train_index, ]
  test_data  <- ml_df[-train_index, ]
  
  # Matrix format for XGBoost (one-hot encode factors)
  train_matrix <- model.matrix(d_catch ~ . - 1, data = train_data)
  test_matrix  <- model.matrix(d_catch ~ . - 1, data = test_data)
  
  dtrain <- xgb.DMatrix(
    data = train_matrix,
    label = train_data$d_catch
  )
  
  dtest <- xgb.DMatrix(
    data = test_matrix,
    label = test_data$d_catch
  )
  
  # Parameters
  params <- list(
    objective = "reg:squarederror",
    eta = 0.1,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8
  )
  
  # Train
  message("Training XGBoost model...")
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 100,
    watchlist = list(train = dtrain, test = dtest),
    print_every_n = 10,
    early_stopping_rounds = 10
  )
  
  # Feature Importance
  importance <- xgb.importance(model = model)
  importance_plot <- xgb.plot.importance(importance_matrix = importance)

  # --- Validation metrics ---
  # Predictions on train and test
  pred_train <- predict(model, dtrain)
  pred_test  <- predict(model, dtest)

  obs_train <- train_data$d_catch
  obs_test  <- test_data$d_catch

  rmse_train <- sqrt(mean((obs_train - pred_train)^2, na.rm = TRUE))
  rmse_test  <- sqrt(mean((obs_test  - pred_test )^2, na.rm = TRUE))

  mae_train <- mean(abs(obs_train - pred_train), na.rm = TRUE)
  mae_test  <- mean(abs(obs_test  - pred_test ), na.rm = TRUE)

  r2_train <- 1 - var(obs_train - pred_train, na.rm = TRUE) / var(obs_train, na.rm = TRUE)
  r2_test  <- 1 - var(obs_test  - pred_test,  na.rm = TRUE) / var(obs_test,  na.rm = TRUE)

  metrics <- tibble(
    split  = c("train", "test"),
    RMSE   = c(rmse_train, rmse_test),
    MAE    = c(mae_train, mae_test),
    R2     = c(r2_train, r2_test)
  )

  message("XGBoost validation metrics:")
  print(metrics)

  list(
    model = model,
    test_data = test_data,
    importance = importance,
    importance_plot = importance_plot,
    metrics = metrics,
    test_diag = tibble(
      obs  = obs_test,
      pred = pred_test
    )
  )
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
  
  analysis_path <- file.path(PROC_DIR, "analysis_full.rds")
  if (!file.exists(analysis_path)) stop("analysis_full.rds not found.")
  
  df <- readRDS(analysis_path)
  
  message("Preparing ML dataset...")
  ml_data <- prepare_ml_data(df)
  
  res <- train_xgboost(ml_data)
  
  # Save Importance Plot
  if (!dir.exists("figures")) dir.create("figures")
  importance_fig <- res$importance_plot + ggplot2::theme_minimal(base_size = 14)
  ggplot2::ggsave(
    filename = file.path("figures", "feature_importance.png"),
    plot = importance_fig,
    width = 8,
    height = 5,
    dpi = 300
  )

  # Predicted vs Observed plot (test set)
  p_pred <- ggplot(res$test_diag, aes(x = pred, y = obs)) +
    geom_point(alpha = 0.3, color = "#009E73") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    theme_minimal() +
    labs(
      title = "XGBoost: Predicted vs Observed d_catch (Test Set)",
      x = "Predicted d_catch",
      y = "Observed d_catch"
    )

  ggplot2::ggsave(
    filename = file.path("figures", "xgboost_pred_vs_observed.png"),
    plot = p_pred,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  saveRDS(res$model, file.path(PROC_DIR, "xgboost_model.rds"))
  write_csv(res$metrics, file.path(PROC_DIR, "xgboost_metrics.csv"))
  message("Saved xgboost_model.rds, xgboost_metrics.csv, feature_importance.png, and xgboost_pred_vs_observed.png")
}

