library(tidyverse)
library(ggplot2)

final_all_df = read_rds("C:/Users/PRASAD/Desktop/Syracuse/Academic/IST687.M001.IDS/Project/outputs/final_combined_dataset.rds")

set.seed(42)  # For reproducibility
sample_df <- final_all_df %>% sample_n(50000)


library(tidyverse)

# Universal plotting function
plot_energy_relationship <- function(df, var, type = "scatter", bin_y = 5) {
  if (type == "scatter") {
    ggplot(df, aes_string(x = var, y = "total_energy_usage")) +
      geom_point(alpha = 0.1, color = "steelblue") +
      geom_smooth(method = "loess", color = "darkred", se = FALSE) +
      labs(
        title = paste("Total Energy Usage vs", var),
        x = var,
        y = "Total Energy Usage (kWh)"
      ) +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } else if (type == "box") {
    ggplot(df, aes_string(x = paste0("factor(", var, ")"), y = "total_energy_usage")) +
      geom_boxplot(outlier.shape = NA, fill = "lightblue") +
      coord_cartesian(ylim = c(0, bin_y)) +
      labs(
        title = paste("Total Energy Usage by", var),
        x = var,
        y = "Total Energy Usage (kWh)"
      ) +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }
}


# plot 1 
plot_energy_relationship(sample_df, "in.sqft", type = "scatter")

# Plot 2
plot_energy_relationship(sample_df, "in.income", "scatter")

# Plot 3
plot_energy_relationship(sample_df, "in.federal_poverty_level", "scatter")

# Plot 4
plot_energy_relationship(sample_df, "in.hvac_heating_type", "box")

# Plot 5
plot_energy_relationship(sample_df, "in.hvac_cooling_type", "box")

# Plot 6
plot_energy_relationship(sample_df, "in.refrigerator", "box")

# Plot 7
plot_energy_relationship(sample_df, "in.lighting", "box")

# Plot 8
plot_energy_relationship(sample_df, "in.insulation_wall", "box")

# Plot 
plot_energy_relationship(sample_df, "in.plug_loads", "scatter")

# Plot 10 
plot_energy_relationship(sample_df, "`Dry Bulb Temperature [°C]`", "scatter")

# Plot 11 
plot_energy_relationship(sample_df, "`Relative Humidity [%]`", "scatter")

# Plot 12
plot_energy_relationship(sample_df, "`Wind Speed [m/s]`", "scatter")

# Plot 13 
plot_energy_relationship(sample_df, "`Temperature Bin (°C)`", "box")
