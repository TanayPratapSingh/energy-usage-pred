library(dplyr)
library(lubridate)
library(ranger)
library(readr)

final_all_df <- read_rds("C:/Users/PRASAD/Desktop/Syracuse/Academic/IST687.M001.IDS/Project/outputs/final_combined_dataset01.rds")


final_all_df <- final_all_df %>%
  mutate(hour = hour(as.POSIXct(time)))

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

set.seed(42)
split_index <- createDataPartition(model_df$total_energy_usage, p = 0.7, list = FALSE)
train_df <- model_df[split_index, ]
test_df  <- model_df[-split_index, ]


rf_model <- ranger(total_energy_usage ~ ., data = train_df, num.trees = 100)


test_df$predicted_actual <- predict(rf_model, data = test_df)$predictions

test_df_warmer <- test_df


test_df_warmer$temp <- test_df_warmer$temp + 5

test_df$predicted_warmer <- predict(rf_model, data = test_df_warmer)$predictions


test_df$delta_energy <- test_df$predicted_warmer - test_df$predicted_actual


summary_stats <- test_df %>%
  summarise(
    avg_actual = mean(predicted_actual),
    avg_warmer = mean(predicted_warmer),
    avg_increase = mean(delta_energy),
    pct_increase = mean(delta_energy) / mean(predicted_actual) * 100
  )

print(summary_stats)

write_rds(test_df, "C://Users/PRASAD/Desktop/Syracuse/Academic/IST687.M001.IDS/Project/test_df_with_5C_predictions.rds")


# Saving the output 

library(ggplot2)
library(dplyr)
library(tidyr)


plot_df <- test_df %>% 
  mutate(index = row_number()) %>% 
  head(48) %>%
  select(index, predicted_actual, predicted_warmer) %>%
  pivot_longer(cols = c(predicted_actual, predicted_warmer), names_to = "scenario", values_to = "energy")

ggplot(plot_df, aes(x = index, y = energy, color = scenario)) +
  geom_line(size = 1) +
  labs(
    title = "Predicted Energy Usage: Actual vs +5°C (First 48 Records)",
    x = "Time Index (Hour)",
    y = "Predicted Energy Usage (kWh)",
    color = "Scenario"
  ) +
  theme_minimal()

#############################################################
# HOURLY GRAPH 
###########################################################



# Summarize total energy usage by hour
peak_hour_summary <- test_df %>%
  group_by(hour) %>%
  summarise(
    total_actual = sum(predicted_actual, na.rm = TRUE),
    total_warmer = sum(predicted_warmer, na.rm = TRUE),
    increase = total_warmer - total_actual,
    pct_increase = (increase / total_actual) * 100
  ) %>%
  arrange(desc(total_warmer))

print(peak_hour_summary)

# Find the true peak hour
peak_hour <- peak_hour_summary %>% head(1)
cat("Future peak hour is:", peak_hour$hour, 
    "with", round(peak_hour$total_warmer, 2), "kWh of total predicted usage (+",
    round(peak_hour$pct_increase, 1), "% increase from actual).\n")


############################################################





