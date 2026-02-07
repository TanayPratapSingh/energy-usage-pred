# Load necessary libraries
library(dplyr)
library(arrow)    # for read_parquet
library(readr)    # for readRDS/writeRDS
library(lubridate)

# --- Load Data ---


final_all_df <- readRDS("C:/Users/PRASAD/Desktop/Syracuse/Academic/IST687.M001.IDS/Project/outputs/final_combined_dataset.rds")
static_house_info <- read_parquet("https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/static_house_info.parquet")


final_all_df <- final_all_df %>%
  left_join(static_house_info %>% select(bldg_id, in.building_america_climate_zone ),
            by = c("bldg_id" = "bldg_id"))

# Removing Unnecessary Columns 
# 
cols_to_remove <- c(
  "in.federal_poverty_level",
  "in.cooling_setpoint_offset_magnitude",
  "in.heating_setpoint",
  "in.hvac_heating_efficiency",
  "in.hvac_cooling_efficiency",
  "in.ceiling_fan",
  "in.clothes_dryer",
  "in.dishwasher",
  "in.water_heater_fuel",
  "in.infiltration",
  "out.electricity.clothes_dryer.energy_consumption",
  "out.electricity.clothes_washer.energy_consumption"
)

final_all_df <- final_all_df %>% select(-all_of(cols_to_remove))

#  Saving the output 
#  
output_path <- "C:/Users/PRASAD/Desktop/Syracuse/Academic/IST687.M001.IDS/Project/outputs/final_combined_dataset01.rds"
saveRDS(final_all_df, output_path)


