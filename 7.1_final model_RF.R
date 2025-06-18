library(tidyverse)

final_all_df = read_rds("C:/Users/PRASAD/Desktop/Syracuse/Academic/IST687.M001.IDS/Project/outputs/final_combined_dataset01.rds")

library(dplyr)
library(lubridate)

# Add hour from time column
final_all_df <- final_all_df %>%
  mutate(hour = hour(as.POSIXct(time)))

# Select relevant columns and clean
model_df <- final_all_df %>%
  select(
    total_energy_usage,
    `Dry Bulb Temperature [°C]`,
    in.sqft,
    in.occupants,
    in.cooling_setpoint,
    hour
  ) %>%
  rename(
    temp = `Dry Bulb Temperature [°C]`,
    sqft = in.sqft,
    occupants = in.occupants,
    setpoint = in.cooling_setpoint
  ) %>%
  na.omit()


library(caret)

set.seed(42)
split_index <- createDataPartition(model_df$total_energy_usage, p = 0.7, list = FALSE)
train_df1 <- model_df[split_index, ]
test_df1  <- model_df[-split_index, ]


# Random Forest 

 


saveRDS(rf_model1, "C:/Users/PRASAD/Desktop/Syracuse/Academic/IST687.M001.IDS/Project/outputs/final_rf_model.rds")

library(randomForest)
library(lubridate)

sample_df$hour <- hour(sample_df$time)

rf_model <- randomForest(
  total_energy_usage ~ hour + `Dry Bulb Temperature [°C]` + in.sqft,
  data = sample_df,
  importance = TRUE
)

varImpPlot(rf_model)


