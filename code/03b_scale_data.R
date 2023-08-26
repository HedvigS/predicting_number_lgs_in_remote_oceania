source("01_requirements.R")

data <- read_tsv("output/processed_data/RO_aggregate_SBZR_group.tsv", show_col_types = F) %>% 
  dplyr::filter(!is.na(SBZR_group)) %>% 
  dplyr::rename(Latitude_abs_mean = mean_lat_abs) %>% 
  filter(!is.na(mode_pol_complex))   %>% 
  rename(Annual_precipitation_mean = mean_CCSM_piControl_1760_bio12) %>% 
  rename(Precipitation_seasonality_mean = mean_CCSM_piControl_1760_bio15) %>% 
  rename(Annual_temperature_mean = mean_CCSM_piControl_1760_bio1) %>% 
  rename(Temperature_seasonality_mean = mean_CCSM_piControl_1760_bio4) %>% 
  rename(EA033 = mode_pol_complex) %>% 
  rename(Area_land = sum_area) %>% 
  rename(Shoreline = sum_shoreline) %>% 
  rename(Settlement_date_grouping_finer = settlement_date_grouping_finer) %>% 
  mutate(SBZR_group = str_replace_all(SBZR_group, "Kanaky", "New Caledonia (incl loyalties)")) %>% 
  mutate(SBZR_group = str_replace_all(SBZR_group, "and ", "+ ")) %>% 
  arrange(-lg_count) %>% 
  dplyr::select(SBZR_group, 
                lg_count, 
                Latitude_abs_mean, #1
                Annual_precipitation_mean, #2
                Precipitation_seasonality_mean, #3
                Annual_temperature_mean, #4
                Temperature_seasonality_mean,#5 
                EA033, #6
                Area_land, #7 
                Shoreline, #8
                Settlement_date_grouping_finer, #9
                ratio_coastline_to_area,#10
                NPP_terra_mean,#11
                NPP_aqua_mean #12
      ) 

#log10 size variables to take out the oversized effect of large island groups, like south island aotearoa etc
data$Area_land <- log10(data$Area_land)
data$Shoreline <- log10(data$Shoreline)
data$ratio_coastline_to_area <- data$Shoreline / data$Area_land

#scaling variables
data$Area_land <- scale(data$Area_land)[,1]
data$Shoreline <- scale(data$Shoreline)[,1] 
data$ratio_coastline_to_area <- scale(data$ratio_coastline_to_area)[,1]
data$Annual_precipitation_mean <- scale(data$Annual_precipitation_mean)[,1] 
data$Precipitation_seasonality_mean <- scale(data$Precipitation_seasonality_mean)[,1]
data$Annual_temperature_mean <- scale(data$Annual_temperature_mean)[,1]
data$Temperature_seasonality_mean <- scale(data$Temperature_seasonality_mean)[,1]
data$Settlement_date_grouping_finer <- scale(data$Settlement_date_grouping_finer)[,1] 
data$EA033 <- scale(data$EA033)[,1]
data$Latitude_abs_mean <- scale(data$Latitude_abs_mean)[,1]
data$NPP_terra_mean <- scale(data$NPP_terra_mean)[,1] 
data$NPP_aqua_mean <- scale(data$NPP_aqua_mean)[,1]

data %>% 
  write_tsv("output/processed_data/RO_aggregate_SBZR_group_scaled.tsv")


##Medium group

data <- read_tsv("output/processed_data/RO_aggregate_medium_island.tsv", show_col_types = F) %>% 
  dplyr::filter(!is.na(Medium_only_merged_for_shared_language)) %>% 
  dplyr::rename(Latitude_abs_mean = mean_lat_abs) %>% 
  filter(!is.na(mode_pol_complex))   %>% 
  rename(Annual_precipitation_mean = mean_CCSM_piControl_1760_bio12) %>% 
  rename(Precipitation_seasonality_mean = mean_CCSM_piControl_1760_bio15) %>% 
  rename(Annual_temperature_mean = mean_CCSM_piControl_1760_bio1) %>% 
  rename(Temperature_seasonality_mean = mean_CCSM_piControl_1760_bio4) %>% 
  rename(EA033 = mode_pol_complex) %>% 
  rename(Area_land = sum_area) %>% 
  rename(Shoreline = sum_shoreline) %>% 
  rename(Settlement_date_grouping_finer = settlement_date_grouping_finer) %>% 
  mutate(Medium_only_merged_for_shared_language = str_replace_all(Medium_only_merged_for_shared_language, "and ", "+ ")) %>% 
  arrange(-lg_count) %>% 
  dplyr::select(Medium_only_merged_for_shared_language, 
                lg_count, 
                Latitude_abs_mean, #1
                Annual_precipitation_mean, #2
                Precipitation_seasonality_mean, #3
                Annual_temperature_mean, #4
                Temperature_seasonality_mean,#5 
                EA033, #6
                Area_land, #7 
                Shoreline, #8
                Settlement_date_grouping_finer, #9
                ratio_coastline_to_area,#10
                NPP_terra_mean,#12
                NPP_aqua_mean#12
)

#log10 size variables to take out the oversized effect of large island groups, like south island aotearoa etc
data$Area_land <- log10(data$Area_land)
data$Shoreline <- log10(data$Shoreline)
data$ratio_coastline_to_area <- data$Shoreline / data$Area_land

#setting all values to between 0 and 1 to make coef easier to interpret. adding 1 so that there aren't actually 0's since the glm.nb otherwise complains about not being able to take the sqrt of 0.
data$Area_land <- scale(data$Area_land)[,1]
data$Shoreline <- scale(data$Shoreline)[,1] 
data$ratio_coastline_to_area <- scale(data$ratio_coastline_to_area)[,1]
data$Annual_precipitation_mean <- scale(data$Annual_precipitation_mean)[,1] 
data$Precipitation_seasonality_mean <- scale(data$Precipitation_seasonality_mean)[,1]
data$Annual_temperature_mean <- scale(data$Annual_temperature_mean)[,1]
data$Temperature_seasonality_mean <- scale(data$Temperature_seasonality_mean)[,1]
data$Settlement_date_grouping_finer <- scale(data$Settlement_date_grouping_finer)[,1]
data$EA033 <- scale(data$EA033)[,1]
data$Latitude_abs_mean <- scale(data$Latitude_abs_mean)[,1]
data$NPP_terra_mean <- scale(data$NPP_terra_mean)[,1]
data$NPP_aqua_mean <- scale(data$NPP_aqua_mean)[,1]

data %>% 
  write_tsv("output/processed_data/RO_aggregate_medium_group_scaled.tsv")

##COUNTRY

data <- read_tsv("output/processed_data/RO_aggregate_COUNTRY_group.tsv", show_col_types = F) %>% 
  dplyr::filter(!is.na(`COUNTRY NAME`)) %>% 
  dplyr::rename(Latitude_abs_mean = mean_lat_abs) %>% 
  filter(!is.na(mode_pol_complex))   %>% 
  rename(Annual_precipitation_mean = mean_CCSM_piControl_1760_bio12) %>% 
  rename(Precipitation_seasonality_mean = mean_CCSM_piControl_1760_bio15) %>% 
  rename(Annual_temperature_mean = mean_CCSM_piControl_1760_bio1) %>% 
  rename(Temperature_seasonality_mean = mean_CCSM_piControl_1760_bio4) %>% 
  rename(EA033 = mode_pol_complex) %>% 
  rename(Area_land = sum_area) %>% 
  rename(Shoreline = sum_shoreline) %>% 
  rename(Settlement_date_grouping_finer = settlement_date_grouping_finer) %>% 
  arrange(-lg_count) %>% 
  dplyr::select(`COUNTRY NAME`, 
                lg_count, 
                Latitude_abs_mean, #1
                Annual_precipitation_mean, #2
                Precipitation_seasonality_mean, #3
                Annual_temperature_mean, #4
                Temperature_seasonality_mean,#5 
                EA033, #6
                Area_land, #7 
                Shoreline, #8
                Settlement_date_grouping_finer, #9
                ratio_coastline_to_area,#10
                NPP_terra_mean,#12
                NPP_aqua_mean#12
  )

#log10 size variables to take out the oversized effect of large island groups, like south island aotearoa etc
data$Area_land <- log10(data$Area_land)
data$Shoreline <- log10(data$Shoreline)
data$ratio_coastline_to_area <- data$Shoreline / data$Area_land

#setting all values to between 0 and 1 to make coef easier to interpret. adding 1 so that there aren't actually 0's since the glm.nb otherwise complains about not being able to take the sqrt of 0.
data$Area_land <- scale(data$Area_land)[,1]
data$Shoreline <- scale(data$Shoreline)[,1]
data$ratio_coastline_to_area <- scale(data$ratio_coastline_to_area)[,1]
data$Annual_precipitation_mean <- scale(data$Annual_precipitation_mean)[,1] 
data$Precipitation_seasonality_mean <- scale(data$Precipitation_seasonality_mean)[,1]
data$Annual_temperature_mean <- scale(data$Annual_temperature_mean)[,1]
data$Temperature_seasonality_mean <- scale(data$Temperature_seasonality_mean)[,1]
data$Settlement_date_grouping_finer <- scale(data$Settlement_date_grouping_finer)[,1] 
data$EA033 <- scale(data$EA033)[,1]
data$Latitude_abs_mean <- scale(data$Latitude_abs_mean)[,1]
data$NPP_terra_mean <- scale(data$NPP_terra_mean)[,1]
data$NPP_aqua_mean <- scale(data$NPP_aqua_mean)[,1]

data %>%
  write_tsv("output/processed_data/RO_aggregate_country_group_scaled.tsv")