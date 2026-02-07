library(readr)
library(dplyr)
library(ranger)


df <- readRDS("C:/Users/PRASAD/Desktop/Syracuse/Academic/IST687.M001.IDS/Project/outputs/final_combined_dataset01.rds")


df <- df %>%
  rename(
    temperature_c = `Dry Bulb Temperature [°C]`
  )


set.seed(42)
train_df1 <- df %>%
  select(in.cooling_setpoint, in.sqft, in.occupants, in.income, temperature_c, total_energy_usage) %>%
  sample_n(50000)


final_rf_model <- ranger(
  x = train_df1 %>% select(-total_energy_usage),
  y = train_df1$total_energy_usage,
  num.trees = 50,
  max.depth = 10,
  write.forest = TRUE,
  save.memory = TRUE
)


saveRDS(final_rf_model, "C:/Users/PRASAD/Desktop/Syracuse/Academic/IST687.M001.IDS/Project/outputs/final_rf_model_fast_clean.rds")
