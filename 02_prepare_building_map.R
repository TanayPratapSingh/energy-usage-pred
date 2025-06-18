library(tidyverse)
library(arrow)

house_df <- read_parquet("https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/static_house_info.parquet")

id_county_map <- house_df %>%
  select(bldg_id, in.county) %>%
  distinct()

write_csv(id_county_map, "C:/Users/PRASAD/Desktop/Syracuse/Academic/IST687.M001.IDS/Project/building_id_county_map.csv")

