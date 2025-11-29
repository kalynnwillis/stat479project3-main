## utils_paths.R
## Helper functions for resolving project directories consistently.

get_proc_dir <- function() {
    if (dir.exists("processed")) {
        return("processed")
    }
    if (dir.exists("../processed")) {
        return("../processed")
    }
    stop("Could not find 'processed' directory.")
}

get_fig_dir <- function() {
    # Prefer the project‑root figures directory if it exists,
    # so we don't accidentally create multiple figures folders.
    if (dir.exists("../figures")) {
        return("../figures")
    }
    if (dir.exists("figures")) {
        return("figures")
    }
    # If neither exists, create a figures directory in the current
    # working directory (e.g., project root) as a fallback.
    dir.create("figures", showWarnings = FALSE)
    "figures"
}
