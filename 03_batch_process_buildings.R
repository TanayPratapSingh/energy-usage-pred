library(tidyverse)
source("C:/Users/PRASAD/Desktop/Syracuse/Academic/IST687.M001.IDS/Project/01_generate_function.r")


building_id_county_map <- read_csv("C:/Users/PRASAD/Desktop/Syracuse/Academic/IST687.M001.IDS/Project/building_id_county_map.csv")


output_dir <- "C:/Users/PRASAD/Desktop/Syracuse/Academic/IST687.M001.IDS/Project/outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)




