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
  rename(Area_incl_water = sum_water_area) %>% 
  rename(Area_land = sum_area) %>% 
  rename(Shoreline = sum_shoreline) %>% 
  rename(Settlement_date_grouping_finer = settlement_date_grouping_finer) %>% 
  rename(Net_Primary_Production_mean = mean_NPP) %>% 
  rename(Net_Primary_Production_Predictability_mean = mean_NetPrimaryProductionPredictability) %>% 
  rename(Isolation = dist) %>% 
  mutate(group = str_replace_all(group, "Kanaky", "New Caledonia (incl loyalties)")) %>% 
  mutate(group = str_replace_all(group, "and", "+")) %>% 
  arrange(-lg_count) %>% 
  dplyr::select(group, lg_count, Latitude_abs_mean, Annual_precipitation_mean, Precipitation_seasonality_mean, Annual_temperature_mean, Temperature_seasonality_mean, EA033, Area_land, Shoreline, Isolation, Settlement_date_grouping_finer, ratio_coastline_to_area)

#log10 size variables
data$Area_land <- log10(data$Area_land)
data$Shoreline <- log10(data$Shoreline)
data$ratio_coastline_to_area <- data$Shoreline / data$Area_land
data$Isolation <- log10(data$Isolation)

#data$lg_count <- log10(data$lg_count)


#normalise by standard deviation

data$Area_land <- scale(x = data$Area_land)[,1]
data$Shoreline <- scale(x = data$Shoreline)[,1]
data$ratio_coastline_to_area <- scale(x = data$ratio_coastline_to_area)[,1]
data$Isolation <- scale(x = data$Isolation)[,1]
data$Annual_precipitation_mean <- scale(x = data$Annual_precipitation_mean)[,1]
data$Precipitation_seasonality_mean <- scale(x = data$Precipitation_seasonality_mean)[,1]
data$Annual_temperature_mean <- scale(x = data$Annual_temperature_mean)[,1]
data$Temperature_seasonality_mean <- scale(x = data$Temperature_seasonality_mean)[,1]
data$Settlement_date_grouping_finer <- scale(x = data$Settlement_date_grouping_finer)[,1]
data$EA033 <- scale(x = data$EA033)[,1]
data$Latitude_abs_mean <- scale(x = data$Latitude_abs_mean)[,1]


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
               cex.cor = 2,stars = T)


##full model
full_model <- glm.nb(data = data, lg_count  ~  Annual_precipitation_mean * Precipitation_seasonality_mean +
                       Annual_temperature_mean * Temperature_seasonality_mean +
                       Latitude_abs_mean  +
                       EA033 +  Isolation + 
                       Shoreline * Settlement_date_grouping_finer +
                       Area_land * Settlement_date_grouping_finer  +
                       ratio_coastline_to_area* Settlement_date_grouping_finer                 ,  
                     control = list(maxit = 30000, epsilon = 1e-7, trace = 3))


broomed_full_model <-  broom::tidy(full_model) %>% 
  mutate(estimate = round(as.numeric(estimate), digits = 7)) %>% 
  mutate(p.value = round(p.value, digit = 5)) %>% 
  mutate(statistic_abs = abs(statistic)) %>% 
  arrange(-statistic_abs) %>% 
  dplyr::select(term, estimate, p.value, statistic = statistic_abs) %>% 
  mutate(estimate_abs = abs(estimate))

anova_full_model <- anova(full_model, test = "Chisq") %>% 
  rownames_to_column("variable") %>% 
  mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
  mutate(interactions = str_count(variable, ":") + 1) %>% 
  arrange(-interactions, -`Pr(>Chi)`) %>% 
  mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>% 
  dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())

non_sigs_left <- anova_full_model %>% 
  filter(sig == "Non Sig") %>% 
  nrow()

old_model <- full_model


while(non_sigs_left != 0){

  non_sigs_left <-  anova(old_model, test = "Chisq") %>% 
    filter(`Pr(>Chi)`  >= 0.05) %>% nrow()
  
  anova(old_model, test = "Chisq") %>% 
  slice_max(order_by = `Pr(>Chi)`) %>% View()
  
  }



variable_highest_p_value <- anova_full_model[1,1]

sig_vars <- anova_full_model %>% 
  filter(sig == "Sig") %>% 
  dplyr::select(variable) %>% 
  as.matrix() %>% 
  as.vector() %>% 
  paste(collapse = ",")

if(variable_highest_p_value %in% sig_vars){
  cat("The variable with the highest p-value is involved in an interaction!!!")
}

model_1 <- update(full_model,  paste(". ~ . -", variable_highest_p_value))

anova_model_1 <- anova(model_1, test = "Chisq") %>% 
  rownames_to_column("variable") %>% 
  mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
  mutate(interactions = str_count(variable, ":") + 1) %>% 
  arrange(-interactions, -`Pr(>Chi)`) %>% 
  mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>% 
  dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())

#View(anova_model_1)
variable_highest_p_value <- anova_model_1[1,1]

sig_vars <- anova_model_1 %>% 
  filter(sig == "Sig") %>% 
  dplyr::select(variable) %>% 
  as.matrix() %>% 
  as.vector() %>% 
  paste(collapse = ",")

if(variable_highest_p_value %in% sig_vars){
  cat("The variable with the highest p-value is involved in an interaction!!!")
}
