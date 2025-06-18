library(readr)
library(dplyr)
library(ranger)

# Load and rename column
df <- readRDS("final_combined_dataset01.rds")

# Rename the problem column
df <- df %>%
  rename(
    temperature_c = `Dry Bulb Temperature [°C]`
  )

# Subset to needed features and sample
set.seed(42)
train_df1 <- df %>%
  select(in.cooling_setpoint, in.sqft, in.occupants, in.income, temperature_c, total_energy_usage) %>%
  sample_n(40000)

# Train lighter model
final_rf_model <- ranger(
  x = train_df1 %>% select(-total_energy_usage),
  y = train_df1$total_energy_usage,
  num.trees = 50,
  max.depth = 10,
  write.forest = TRUE,
  save.memory = TRUE
)


