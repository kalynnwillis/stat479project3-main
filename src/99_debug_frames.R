# 99_debug_frames.R
# Check frame IDs for a single play to debug negative air_time

library(tidyverse)

source("src/00_load_data.R")

# Load Week 1
data <- load_week_data("w01")
input <- data$input
output <- data$output

# Pick a play
gid <- input$game_id[1]
pid <- input$play_id[1]

message(paste("Debugging Game:", gid, "Play:", pid))

# Filter
p_in <- input |> filter(game_id == gid, play_id == pid)
p_out <- output |> filter(game_id == gid, play_id == pid)

# Targeted Receiver
tr_id <- p_in |> 
  filter(player_role == "Targeted Receiver") |> 
  distinct(nfl_id) |> 
  pull(nfl_id)

if(length(tr_id) > 0) tr_id <- tr_id[1]

message("Targeted Receiver ID: ", tr_id)

# Frame Ranges
in_frames <- p_in$frame_id
out_frames <- p_out$frame_id

message("Input Frames: ", min(in_frames), " to ", max(in_frames))
message("Output Frames: ", min(out_frames), " to ", max(out_frames))

# Check specific player frames
tr_in <- p_in |> filter(nfl_id == tr_id)
tr_out <- p_out |> filter(nfl_id == tr_id)

message("Receiver Input Frames: ", min(tr_in$frame_id), " to ", max(tr_in$frame_id))
if(nrow(tr_out) > 0) {
  message("Receiver Output Frames: ", min(tr_out$frame_id), " to ", max(tr_out$frame_id))
} else {
  message("Receiver NOT FOUND in Output!")
}

# Calculate proposed throw/catch frames
throw_frame <- max(p_in$frame_id)
catch_frame <- max(tr_out$frame_id)

message("Proposed Throw Frame: ", throw_frame)
message("Proposed Catch Frame: ", catch_frame)
message("Air Time: ", (catch_frame - throw_frame) * 0.1)

