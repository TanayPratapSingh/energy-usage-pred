generate_clean_july_df <- function(building_id, county_code, output_dir = NULL) {
  library(arrow)
  library(tidyverse)
  
  house_df <- read_parquet("https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/static_house_info.parquet")
  
  house_df_clean <- house_df %>%
    select(
      bldg_id, in.county, in.sqft, in.bedrooms, in.occupants, in.income,
      in.federal_poverty_level, in.cooling_setpoint, in.cooling_setpoint_offset_magnitude,
      in.heating_setpoint, in.hvac_heating_type, in.hvac_cooling_type,
      in.hvac_heating_efficiency, in.hvac_cooling_efficiency, in.ceiling_fan, in.clothes_dryer,
      in.refrigerator, in.dishwasher, in.lighting, in.water_heater_fuel, in.clothes_washer,
      in.insulation_wall, in.insulation_ceiling, in.infiltration, in.plug_loads,
      in.natural_ventilation, in.range_spot_vent_hour, in.bathroom_spot_vent_hour,
      in.has_pv, in.vintage, in.vintage_acs
    ) %>%
    na.omit()
  
  energy_path <- paste0("https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/2023-houseData/", building_id, ".parquet")
  energy_df <- read_parquet(energy_path)
  
  energy_df_clean <- energy_df %>%
    select(
      out.electricity.cooling.energy_consumption,
      out.electricity.heating.energy_consumption,
      out.electricity.lighting_interior.energy_consumption,
      out.electricity.plug_loads.energy_consumption,
      out.electricity.refrigerator.energy_consumption,
      out.electricity.clothes_dryer.energy_consumption,
      out.electricity.clothes_washer.energy_consumption,
      out.electricity.dishwasher.energy_consumption,
      out.electricity.hot_water.energy_consumption,
      time
    ) %>%
    na.omit() %>%
    mutate(
      total_energy_usage = rowSums(select(., -time)),
      time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    ) %>%
    filter(format(time, "%Y-%m") == "2018-07")
  
  weather_path <- paste0("https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/weather/2023-weather-data/", county_code, ".csv")
  weather_df <- read_csv(weather_path)
  
  weather_df_clean <- weather_df %>%
    select(
      date_time,
      `Dry Bulb Temperature [°C]`,
      `Relative Humidity [%]`,
      `Wind Speed [m/s]`
    ) %>%
    na.omit() %>%
    rename(time = date_time) %>%
    mutate(time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
    filter(format(time, "%Y-%m") == "2018-07")
  
  energy_weather_df <- merge(energy_df_clean, weather_df_clean, by = "time")
  energy_weather_df$bldg_id <- building_id
  energy_weather_df <- energy_weather_df[, c("bldg_id", setdiff(names(energy_weather_df), "bldg_id"))]
  
  final_merged_df <- merge(house_df_clean, energy_weather_df, by = "bldg_id")
  
  # Save to file if output_dir is given
  if (!is.null(output_dir)) {
    out_file <- file.path(output_dir, paste0("july_bldg_", building_id, ".csv"))
    write_csv(final_merged_df, out_file)
  }
  
  return(final_merged_df)
}
