library(tidyverse)

# Define your path
output_dir <- "C:/Users/PRASAD/Desktop/Syracuse/Academic/IST687.M001.IDS/Project/outputs"
csv_files <- list.files(output_dir, pattern = "\\.csv$", full.names = TRUE)

# Combine all CSVs
final_df_all <- purrr::map_dfr(seq_along(csv_files), function(i) {
  cat("Reading file", i, "of", length(csv_files), ":", basename(csv_files[i]), "\n")
  
  df <- read_csv(csv_files[i], show_col_types = FALSE)
  
  # Fix known inconsistent columns
  df$in.cooling_setpoint_offset_magnitude <- as.numeric(df$in.cooling_setpoint_offset_magnitude)
  df$in.vintage <- as.numeric(df$in.vintage)
  df$in.vintage_acs <- as.numeric(df$in.vintage_acs)
  df$in.occupants <- as.numeric(df$in.occupants)
  
  return(df)
})

write_csv(final_df_all, file.path(output_dir, "final_combined_dataset.csv"))
saveRDS(final_df_all, file.path(output_dir, "final_combined_dataset.rds"))

cat("All files combined successfully: ", nrow(final_df_all), " rows total.\n")
