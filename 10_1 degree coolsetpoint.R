

test_df_peak_adj <- test_df
test_df_peak_adj$setpoint <- ifelse(test_df_peak_adj$hour %in% 16:21,
                                    test_df_peak_adj$setpoint + 1,
                                    test_df_peak_adj$setpoint)

test_df$predicted_peak_reduced <- predict(rf_model, data = test_df_peak_adj)$predictions

test_df$kwh_saved <- test_df$predicted_warmer - test_df$predicted_peak_reduced
test_df$pct_saved <- test_df$kwh_saved / test_df$predicted_warmer * 100

peak_savings_summary <- test_df %>%
  filter(hour %in% 16:21) %>%
  summarise(
    avg_warmer = mean(predicted_warmer),
    avg_reduced = mean(predicted_peak_reduced),
    avg_kwh_saved = mean(kwh_saved),
    pct_savings = mean(pct_saved)
  )

print(peak_savings_summary)
