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
train_df <- model_df[split_index, ]
test_df  <- model_df[-split_index, ]

# Linear Model 

lm_model <- lm(total_energy_usage ~ ., data = train_df)
lm_preds <- predict(lm_model, newdata = test_df)

lm_rmse <- RMSE(lm_preds, test_df$total_energy_usage)
lm_r2   <- R2(lm_preds, test_df$total_energy_usage)

# Random Forest 

library(ranger)

rf_model <- ranger(total_energy_usage ~ ., data = train_df, num.trees = 100)
rf_preds <- predict(rf_model, data = test_df)$predictions

rf_rmse <- RMSE(rf_preds, test_df$total_energy_usage)
rf_r2   <- R2(rf_preds, test_df$total_energy_usage)


# XGBoost

library(xgboost)


train_matrix <- model.matrix(total_energy_usage ~ . - 1, data = train_df)
test_matrix  <- model.matrix(total_energy_usage ~ . - 1, data = test_df)

dtrain <- xgb.DMatrix(data = train_matrix, label = train_df$total_energy_usage)
dtest  <- xgb.DMatrix(data = test_matrix)

xgb_model <- xgboost(data = dtrain, nrounds = 100, objective = "reg:squarederror", verbose = 0)
xgb_preds <- predict(xgb_model, newdata = dtest)

xgb_rmse <- RMSE(xgb_preds, test_df$total_energy_usage)
xgb_r2   <- R2(xgb_preds, test_df$total_energy_usage)

# Comparing the result

cat("Linear Model     => RMSE:", lm_rmse,  " | R²:", lm_r2,  "\n")
cat("Random Forest    => RMSE:", rf_rmse,  " | R²:", rf_r2,  "\n")
cat("XGBoost          => RMSE:", xgb_rmse, " | R²:", xgb_r2, "\n")


## 3 Variables approach

library(dplyr)
library(caret)
library(ranger)
library(xgboost)
library(Metrics)

# --- Prepare Data ---
model_df_hot <- final_all_df %>%
  mutate(hour = hour(as.POSIXct(time))) %>%  # Extract hour from datetime
  select(
    total_energy_usage,
    `Dry Bulb Temperature [°C]`,
    in.cooling_setpoint,
    hour
  ) %>%
  rename(
    temp = `Dry Bulb Temperature [°C]`,
    setpoint = in.cooling_setpoint
  ) %>%
  na.omit()

# --- Train/Test Split ---
set.seed(42)
split_index <- createDataPartition(model_df_hot$total_energy_usage, p = 0.7, list = FALSE)
train_df <- model_df_hot[split_index, ]
test_df  <- model_df_hot[-split_index, ]


# --- Linear Model ---
lm_model <- lm(total_energy_usage ~ ., data = train_df)
lm_preds <- predict(lm_model, newdata = test_df)

lm_rmse <- RMSE(lm_preds, test_df$total_energy_usage)
lm_r2   <- R2(lm_preds, test_df$total_energy_usage)

# --- Random Forest ---
rf_model <- ranger(total_energy_usage ~ ., data = train_df, num.trees = 100)
rf_preds <- predict(rf_model, data = test_df)$predictions

rf_rmse <- RMSE(rf_preds, test_df$total_energy_usage)
rf_r2   <- R2(rf_preds, test_df$total_energy_usage)

# --- XGBoost ---
train_matrix <- model.matrix(total_energy_usage ~ . - 1, data = train_df)
test_matrix  <- model.matrix(total_energy_usage ~ . - 1, data = test_df)

dtrain <- xgb.DMatrix(data = train_matrix, label = train_df$total_energy_usage)
dtest  <- xgb.DMatrix(data = test_matrix)

xgb_model <- xgboost(data = dtrain, nrounds = 100, objective = "reg:squarederror", verbose = 0)
xgb_preds <- predict(xgb_model, newdata = dtest)

xgb_rmse <- RMSE(xgb_preds, test_df$total_energy_usage)
xgb_r2   <- R2(xgb_preds, test_df$total_energy_usage)

# --- Results ---
cat("Linear Model     => RMSE:", lm_rmse,  " | R²:", lm_r2,  "\n")
cat("Random Forest    => RMSE:", rf_rmse,  " | R²:", rf_r2,  "\n")
cat("XGBoost          => RMSE:", xgb_rmse, " | R²:", xgb_r2, "\n")


# # SVM 
# 
# svm_df <- final_all_df %>%
#   mutate(hour = hour(as.POSIXct(time))) %>%
#   select(total_energy_usage, `Dry Bulb Temperature [°C]`, in.cooling_setpoint, hour) %>%
#   rename(
#     temp = `Dry Bulb Temperature [°C]`,
#     setpoint = in.cooling_setpoint
#   ) %>%
#   na.omit()
# 
# 
# set.seed(42)
# split_idx <- createDataPartition(svm_df$total_energy_usage, p = 0.7, list = FALSE)
# train_svm <- svm_df[split_idx, ]
# test_svm  <- svm_df[-split_idx, ]

# library(e1071)
# 
# svm_model <- svm(total_energy_usage ~ ., data = train_svm, kernel = "radial")
# svm_preds <- predict(svm_model, newdata = test_svm)
# 
# svm_rmse <- RMSE(svm_preds, test_svm$total_energy_usage)
# svm_r2   <- R2(svm_preds, test_svm$total_energy_usage)
# 
# 





