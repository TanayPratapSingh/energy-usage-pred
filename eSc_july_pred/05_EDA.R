library(tidyverse)

plot_energy_relationship <- function(df, var, type = "scatter", bin_y = 15) {
  if (!var %in% names(df)) {
    return(ggplot() + labs(title = paste("Variable", var, "not found in dataset.")))
  }
  
  df <- df[!is.na(df[[var]]), ]
  
  if (type == "scatter") {
    ggplot(df, aes_string(x = var, y = "total_energy_usage")) +
      geom_point(alpha = 0.1, color = "steelblue") +
      geom_smooth(method = "loess", color = "darkred", se = FALSE) +
      labs(
        title = paste("Total Energy Usage vs", var),
        x = var,
        y = "Total Energy Usage (kWh)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } else if (type == "box") {
    if (var == "in.federal_poverty_level") {
      ordered_levels <- c("0-100%", "100-150%", "150-200%", "200-300%", "300-400%", "400%+")
      df[[var]] <- factor(df[[var]], levels = ordered_levels, ordered = TRUE)
    } else {
      df[[var]] <- factor(df[[var]])
    }
    
    ggplot(df, aes_string(x = var, y = "total_energy_usage")) +
      geom_boxplot(outlier.shape = NA, fill = "lightblue") +
      coord_cartesian(ylim = c(0, bin_y)) +
      labs(
        title = paste("Total Energy Usage by", var),
        x = var,
        y = "Total Energy Usage (kWh)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }
}
            

