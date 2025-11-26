
# Check unique roles in output file
output_file <- "data/output_2023_w01.csv"
if (!file.exists(output_file)) {
  stop("File not found")
}

df_out <- read.csv(output_file, nrows = 10000) # Read a chunk
print(colnames(df_out))
if ("player_role" %in% colnames(df_out)) {
  print(table(df_out$player_role))
} else {
  print("player_role column not found in output")
}

# Check unique nfl_ids in output vs input
input_file <- "data/input_2023_w01.csv"
df_in <- read.csv(input_file, nrows = 10000)
print("Input roles:")
print(table(df_in$player_role))

