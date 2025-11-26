# 05_example_play_plot.R
# Description: Visualize a single play using the provided example code.
# Note: Ensure input_2023_w01.csv and output_2023_w01.csv are in the data/ folder.

library(tidyverse)

# --- Load Data ---
# Using readr as per the example
input_path <- file.path("../data", "input_2023_w01.csv")
output_path <- file.path("../data", "output_2023_w01.csv")

if (!file.exists(input_path) || !file.exists(output_path)) {
  stop("Data files not found in data/. Please download input_2023_w01.csv and output_2023_w01.csv.")
}

message("Loading Week 1 data...")
raw_w1_input <- readr::read_csv(input_path)
raw_w1_output <- readr::read_csv(output_path)

# --- Prepare Play Data ---
# Select the first play in the dataset
gid <- raw_w1_input$game_id[1]
pid <- raw_w1_input$play_id[1]

message(paste("Visualizing Game:", gid, "Play:", pid))

tmp_in <- raw_w1_input |>
  dplyr::filter(game_id == gid & play_id == pid)

tmp_out <- raw_w1_output |>
  dplyr::filter(game_id == gid & play_id == pid)

ball_land_x <- tmp_in$ball_land_x[1]
ball_land_y <- tmp_in$ball_land_y[1]
all_players <- unique(c(tmp_in$nfl_id, tmp_out$nfl_id))

# --- Plotting ---
oi_colors <- palette.colors(palette = "Okabe-Ito")

# Save plot to a file (optional, or display in RStudio)
# pdf("report/example_play.pdf", width = 10, height = 6) 

par(mar = c(1,1,1,1), mgp = c(1.8, 0.5, 0))
plot(1, type = "n", xlim = c(0,120), ylim = c(0, 54),
     xaxt = "n", yaxt = "n", xlab = "", ylab = "",
     main = paste("Play", pid, "Game", gid))

# Plot Ball Landing
points(ball_land_x, ball_land_y, pch = 4, cex = 2, col = oi_colors[8])

# Plot Line of Scrimmage
lines(x = c(tmp_in$absolute_yardline_number[1], tmp_in$absolute_yardline_number[1]),
      y = c(par("usr")[3], par("usr")[4]), lty = 2, col = "gray")

# Plot Players
for(player in all_players){
  player_in <- tmp_in |> dplyr::filter(nfl_id == player)
  role <- player_in$player_role[1]
  
  # Pre-throw positions
  if(role %in% c("Passer", "Targeted Receiver", "Other Route Runner")){
    points(player_in$x, player_in$y, pch = 16, cex = 0.5,
           col = adjustcolor(oi_colors[9], alpha.f = 0.5))
  } else{
    points(player_in$x, player_in$y, pch = 15, cex = 0.5,
           col = adjustcolor(oi_colors[9], alpha.f = 0.5))
  }
  
  # In-air positions (if predicted/available)
  player_out <- tmp_out |> dplyr::filter(nfl_id == player)
  
  # Check if player exists in output and has player_to_predict flag
  # Note: player_to_predict is often only in output files, or needs to be checked there
  is_predicted <- FALSE
  if(nrow(player_out) > 0 && "player_to_predict" %in% names(player_out)) {
      if(isTRUE(player_out$player_to_predict[1])) is_predicted <- TRUE
  }
  
  if(is_predicted){
    if(role %in% c("Passer", "Targeted Receiver", "Other Route Runner")){
      points(player_out$x, player_out$y, pch = 16, cex = 0.5,
             col = adjustcolor(oi_colors[2], alpha.f = 0.75))
    } else{
      points(player_out$x, player_out$y, pch = 15, cex = 0.5,
             col = adjustcolor(oi_colors[3], alpha.f = 0.75))
    }
  } 
} 

legend("topleft", legend = c("Offense", "Defense"), pch = c(15, 16), col = oi_colors[c(2,3)], bty = "n")
legend("bottomright", legend = c("Pre-throw", "Ball in air", "Ball end location"), pch = c(15, 16, 4),
       col = oi_colors[c(9,9, 8)], bty = "n")

# dev.off()

