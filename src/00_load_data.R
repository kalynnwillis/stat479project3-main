# 00_load_data.R
# Description: Load and preprocess input and output tracking data.

library(tidyverse)
library(readr)

# --- Constants ---
# Determine DATA_DIR based on where the script is run from
if (dir.exists("data")) {
  DATA_DIR <- "data"
} else if (dir.exists("../data")) {
  DATA_DIR <- "../data"
} else {
  warning("Could not find data/ directory. Assuming 'data' but might fail.")
  DATA_DIR <- "data"
}

WEEKS <- sprintf("w%02d", 1:18) # Weeks w01 to w18

# --- Functions ---

load_week_data <- function(week) {
  input_file <- file.path(DATA_DIR, paste0("input_2023_", week, ".csv"))
  output_file <- file.path(DATA_DIR, paste0("output_2023_", week, ".csv"))
  
  if (!file.exists(input_file)) {
    warning(paste("Input file not found:", input_file))
    return(NULL)
  }
  if (!file.exists(output_file)) {
    warning(paste("Output file not found:", output_file))
    return(NULL)
  }
  
  message(paste("Loading data for", week, "..."))
  input_data <- read_csv(input_file, show_col_types = FALSE)
  output_data <- read_csv(output_file, show_col_types = FALSE)
  
  return(list(input = input_data, output = output_data))
}

load_tracking_data <- function(weeks = WEEKS) {
  # Loads data for multiple weeks and combines them (optional)
  # For now, returning a list keyed by week
  
  all_data <- list()
  
  for (week in weeks) {
    week_data <- load_week_data(week)
    if (!is.null(week_data)) {
      all_data[[week]] <- week_data
    }
  }
  
  return(all_data)
}

# --- Main Execution ---

if (sys.nframe() == 0) {
  # Check if data directory has files
  if (length(list.files(DATA_DIR, pattern = "*.csv")) == 0) {
    stop("No CSV files found in data/ directory. Please download the Big Data Bowl data.")
  }
  
  data <- load_tracking_data()
  message("Data loading complete.")
  # saveRDS(data, file.path(DATA_DIR, "loaded_data.rds"))
}
