source("01_requirements.R")

data <- read_tsv("output/processed_data/RO_Hedvig_aggregate_marck_group.tsv", show_col_types = F) %>% 
  dplyr::filter(!is.na(Marck_group)) %>% 
  dplyr::select(group = Marck_group, everything()) %>% 
  dplyr::mutate(Latitude_abs_mean = abs(mean_lat)) %>% 
  filter(!is.na(mean_pol_complex))   %>% 
  rename(Annual_precipitation_mean = mean_CCSM_piControl_1760_bio12) %>% 
  rename(Precipitation_seasonality_mean = mean_CCSM_piControl_1760_bio15) %>% 
  rename(Annual_temperature_mean = mean_CCSM_piControl_1760_bio1) %>% 
  rename(Temperature_seasonality_mean = mean_CCSM_piControl_1760_bio4) %>% 
  rename(EA033 = mean_pol_complex) %>% 
  rename(Area_land = sum_area) %>% 
  rename(Shoreline = sum_shoreline) %>% 
  rename(Settlement_date_grouping_finer = settlement_date_grouping_finer) %>% 
  rename(Isolation = dist) %>% 
  mutate(group = str_replace_all(group, "Kanaky", "New Caledonia (incl loyalties)")) %>% 
  mutate(group = str_replace_all(group, "and", "+")) %>% 
  arrange(-lg_count) %>% 
  dplyr::select(group, 
                lg_count, 
                Latitude_abs_mean, #1
                Annual_precipitation_mean, #2
                Precipitation_seasonality_mean, #3
                Annual_temperature_mean, #4
                Temperature_seasonality_mean,#5 
                EA033, #6
                Area_land, #7 
                Shoreline, #8
                Isolation, #9
                Settlement_date_grouping_finer, #10
                ratio_coastline_to_area) #11

#log10 size variables to take out the oversized effect of large island groups, like south island aotearoa etc
data$Area_land <- log10(data$Area_land)
data$Shoreline <- log10(data$Shoreline)
data$ratio_coastline_to_area <- data$Shoreline / data$Area_land
data$Isolation <- log10(data$Isolation)

#setting all values to between 0 and 1 to make coef easier to interpret. adding 1 so that there aren't actually 0's since the glm.nb otherwise complains about not being able to take the sqrt of 0.
data$Area_land <- modEvA::range01(data$Area_land) +1
data$Shoreline <- modEvA::range01(data$Shoreline) +1
data$ratio_coastline_to_area <- modEvA::range01(data$ratio_coastline_to_area) +1
data$Isolation <- modEvA::range01(data$Isolation) +1
data$Annual_precipitation_mean <- modEvA::range01(data$Annual_precipitation_mean) +1
data$Precipitation_seasonality_mean <- modEvA::range01(data$Precipitation_seasonality_mean) +1
data$Annual_temperature_mean <- modEvA::range01(data$Annual_temperature_mean) +1
data$Temperature_seasonality_mean <- modEvA::range01(data$Temperature_seasonality_mean) +1
data$Settlement_date_grouping_finer <- modEvA::range01(data$Settlement_date_grouping_finer) +1
data$EA033 <- modEvA::range01(data$EA033) +1
data$Latitude_abs_mean <- modEvA::range01(data$Latitude_abs_mean) +1

data %>% 
  write_tsv("output/processed_data/RO_Hedvig_aggregate_marck_group_scaled.tsv")

png(filename = "output/plots/SLOM_marck_all_variables.png", width = 10, height = 10, units = "in", res = 300)
data %>%   
  dplyr::select(lg_count,EA033,  Settlement_date_grouping_finer, Area_land ,Shoreline, ratio_coastline_to_area, Isolation, Latitude_abs_mean,Annual_temperature_mean, Temperature_seasonality_mean, Annual_precipitation_mean, Precipitation_seasonality_mean) %>% 
  pairs.panels(method = "pearson", # correlation method
               hist.col = "#a3afd1",# "#a9d1a3","",""),
               density = TRUE,  # show density plots
               ellipses = F, # show correlation ellipses
               cex.labels= 1,
               #           smoother= T,
               cor=T,
               lm=T,
               ci = T, 
               cex.cor = 1,stars = T)
x <- dev.off()
