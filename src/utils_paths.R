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
    if (dir.exists("figures")) {
        return("figures")
    }
    if (dir.exists("../figures")) {
        return("../figures")
    }
    dir.create("figures", showWarnings = FALSE)
    "figures"
}
