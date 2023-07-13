source("01_requirements.R")

data <- read_tsv("output/processed_data/RO_Hedvig_aggregate_marck_group.tsv", show_col_types = F) %>% 
  dplyr::filter(!is.na(Marck_group)) %>% 
  dplyr::rename(Latitude_abs_mean = mean_lat_abs) %>% 
  filter(!is.na(mean_pol_complex))   %>% 
  rename(Annual_precipitation_mean = mean_CCSM_piControl_1760_bio12) %>% 
  rename(Precipitation_seasonality_mean = mean_CCSM_piControl_1760_bio15) %>% 
  rename(Annual_temperature_mean = mean_CCSM_piControl_1760_bio1) %>% 
  rename(Temperature_seasonality_mean = mean_CCSM_piControl_1760_bio4) %>% 
  rename(EA033 = mean_pol_complex) %>% 
  rename(Area_land = sum_area) %>% 
  rename(Shoreline = sum_shoreline) %>% 
  rename(Area_water = sum_water_area) %>% 
  rename(Settlement_date_grouping_finer = settlement_date_grouping_finer) %>% 
  rename(Isolation = dist) %>% 
  mutate(Marck_group = str_replace_all(Marck_group, "Kanaky", "New Caledonia (incl loyalties)")) %>% 
  mutate(Marck_group = str_replace_all(Marck_group, "and", "+")) %>% 
  arrange(-lg_count) %>% 
  dplyr::select(Marck_group, 
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
                ratio_coastline_to_area,#11
                Area_water,#12
                NPP_terra_mean,#13
      #          NPP_terra_var,#14
                NPP_aqua_mean #15
       #         NPP_aqua_var16
      ) #

#log10 size variables to take out the oversized effect of large island groups, like south island aotearoa etc
data$Area_land <- log10(data$Area_land)
data$Area_water <- log10(data$Area_water)
data$Shoreline <- log10(data$Shoreline)
data$ratio_coastline_to_area <- data$Shoreline / data$Area_land
data$Isolation <- log10(data$Isolation)

#setting all values to between 0 and 1 to make coef easier to interpret. adding 1 so that there aren't actually 0's since the glm.nb otherwise complains about not being able to take the sqrt of 0.
data$Area_land <- modEvA::range01(data$Area_land) 
data$Area_water <- modEvA::range01(data$Area_water) 
data$Shoreline <- modEvA::range01(data$Shoreline) 
data$ratio_coastline_to_area <- modEvA::range01(data$ratio_coastline_to_area) 
data$Isolation <- modEvA::range01(data$Isolation) 
data$Annual_precipitation_mean <- modEvA::range01(data$Annual_precipitation_mean) 
data$Precipitation_seasonality_mean <- modEvA::range01(data$Precipitation_seasonality_mean) 
data$Annual_temperature_mean <- modEvA::range01(data$Annual_temperature_mean) 
data$Temperature_seasonality_mean <- modEvA::range01(data$Temperature_seasonality_mean) 
data$Settlement_date_grouping_finer <- modEvA::range01(data$Settlement_date_grouping_finer) 
data$EA033 <- modEvA::range01(data$EA033) 
data$Latitude_abs_mean <- modEvA::range01(data$Latitude_abs_mean) 
data$NPP_terra_mean <- modEvA::range01(data$NPP_terra_mean) 
#data$NPP_terra_var <- modEvA::range01(data$NPP_terra_var) 
data$NPP_aqua_mean <- modEvA::range01(data$NPP_aqua_mean) 
#data$NPP_aqua_var <- modEvA::range01(data$NPP_aqua_var) 

data %>% 
  write_tsv("output/processed_data/RO_Hedvig_aggregate_marck_group_scaled.tsv")

colnames(data) <- str_replace_all(colnames(data), "_", "\n")

png(filename = "output/plots/SLOM_marck_all_variables.png", width = 10, height = 10, units = "in", res = 300)
data %>%   
  dplyr::select("lg\ncount","EA033",  "Settlement\ndate\ngrouping\nfiner", "Area\nland" ,"Area\nwater", "Shoreline", "ratio\ncoastline\nto\narea", 
                "Isolation", "Latitude\nabs\nmean","Annual\ntemperature\nmean", "Temperature\nseasonality\nmean", "Annual\nprecipitation\nmean", "Precipitation\nseasonality\nmean", "NPP\nterra\nmean", 
                #"NPP\nterra\nvar", "NPP\naqua\nmean" 
                #"NPP\naqua\nvar"
                ) %>% 
  pairs.panels(method = "pearson", # correlation method
               hist.col = "#a3afd1",# "#a9d1a3","",""),
               density = TRUE,  # show density plots
               ellipses = F, # show correlation ellipses
               cex.labels= 0.7,
               #           smoother= T,
               cor=T,
               lm=T,
               ci = T, 
               cex.cor = 1,stars = T)
x <- dev.off()

##Medium group

data <- read_tsv("output/processed_data/RO_Hedvig_aggregate_medium_island.tsv", show_col_types = F) %>% 
  dplyr::filter(!is.na(Medium_only_merged_for_shared_language)) %>% 
  dplyr::rename(Latitude_abs_mean = mean_lat_abs) %>% 
  filter(!is.na(mean_pol_complex))   %>% 
  rename(Annual_precipitation_mean = mean_CCSM_piControl_1760_bio12) %>% 
  rename(Precipitation_seasonality_mean = mean_CCSM_piControl_1760_bio15) %>% 
  rename(Annual_temperature_mean = mean_CCSM_piControl_1760_bio1) %>% 
  rename(Temperature_seasonality_mean = mean_CCSM_piControl_1760_bio4) %>% 
  rename(EA033 = mean_pol_complex) %>% 
  rename(Area_land = sum_area) %>% 
  rename(Shoreline = sum_shoreline) %>% 
  rename(Area_water = sum_water_area) %>% 
  rename(Settlement_date_grouping_finer = settlement_date_grouping_finer) %>% 
  rename(Isolation = dist) %>% 
  mutate(Medium_only_merged_for_shared_language = str_replace_all(Medium_only_merged_for_shared_language, "and", "+")) %>% 
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
                Isolation, #9
                Settlement_date_grouping_finer, #10
                ratio_coastline_to_area,#11
                Area_water,#12
                NPP_terra_mean,#13
#                NPP_terra_var,#14
                NPP_aqua_mean#15
 #               NPP_aqua_var) #16
)

#log10 size variables to take out the oversized effect of large island groups, like south island aotearoa etc
data$Area_land <- log10(data$Area_land)
data$Area_water <- log10(data$Area_water)
data$Shoreline <- log10(data$Shoreline)
data$ratio_coastline_to_area <- data$Shoreline / data$Area_land
data$Isolation <- log10(data$Isolation)

#setting all values to between 0 and 1 to make coef easier to interpret. adding 1 so that there aren't actually 0's since the glm.nb otherwise complains about not being able to take the sqrt of 0.
data$Area_land <- modEvA::range01(data$Area_land) 
data$Area_water <- modEvA::range01(data$Area_water) 
data$Shoreline <- modEvA::range01(data$Shoreline) 
data$ratio_coastline_to_area <- modEvA::range01(data$ratio_coastline_to_area) 
data$Isolation <- modEvA::range01(data$Isolation) 
data$Annual_precipitation_mean <- modEvA::range01(data$Annual_precipitation_mean) 
data$Precipitation_seasonality_mean <- modEvA::range01(data$Precipitation_seasonality_mean) 
data$Annual_temperature_mean <- modEvA::range01(data$Annual_temperature_mean) 
data$Temperature_seasonality_mean <- modEvA::range01(data$Temperature_seasonality_mean) 
data$Settlement_date_grouping_finer <- modEvA::range01(data$Settlement_date_grouping_finer) 
data$EA033 <- modEvA::range01(data$EA033) 
data$Latitude_abs_mean <- modEvA::range01(data$Latitude_abs_mean) 
data$NPP_terra_mean <- modEvA::range01(data$NPP_terra_mean) 
#data$NPP_terra_var <- modEvA::range01(data$NPP_terra_var) 
data$NPP_aqua_mean <- modEvA::range01(data$NPP_aqua_mean) 
#data$NPP_aqua_var <- modEvA::range01(data$NPP_aqua_var) 

data %>% 
  write_tsv("output/processed_data/RO_Hedvig_aggregate_medium_group_scaled.tsv")

colnames(data) <- str_replace_all(colnames(data), "_", "\n")

png(filename = "output/plots/SLOM_medium_all_variables.png", width = 10, height = 10, units = "in", res = 300)
data %>%   
  dplyr::select("lg\ncount","EA033",  "Settlement\ndate\ngrouping\nfiner", "Area\nland" ,"Area\nwater", "Shoreline", "ratio\ncoastline\nto\narea", 
                "Isolation", "Latitude\nabs\nmean","Annual\ntemperature\nmean", "Temperature\nseasonality\nmean", "Annual\nprecipitation\nmean", "Precipitation\nseasonality\nmean", "NPP\nterra\nmean",
                #"NPP\nterra\nvar", "NPP\naqua\nmean" 
                #"NPP\naqua\nvar"
                ) %>% 
  pairs.panels(method = "pearson", # correlation method
               hist.col = "#a3afd1",# "#a9d1a3","",""),
               density = TRUE,  # show density plots
               ellipses = F, # show correlation ellipses
               cex.labels= 0.7,
               #           smoother= T,
               cor=T,
               lm=T,
               ci = T, 
               cex.cor = 1,stars = T)
x <- dev.off()
