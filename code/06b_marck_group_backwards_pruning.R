source("01_requirements.R")

data <- read_tsv("output/processed_data/RO_Hedvig_aggregate_marck_group_scaled.tsv", show_col_types = F) 

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
  mutate(estimate_abs = abs(estimate)) %>% 
  mutate(sig = ifelse(`p.value` <0.05, "Sig", "Non Sig")) 
  
anova_full_model <- anova(full_model, test = "Chisq") %>% 
  rownames_to_column("variable") %>% 
  mutate(interactions = str_count(variable, ":") + 1) %>% 
  arrange(-interactions, -`Pr(>Chi)`) %>% 
  mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig"))

write_tsv(x = broomed_full_model, file = "output/results/marck_full_model_broomed.tsv")
write_tsv(x = anova_full_model, file = "output/results/marck_full_model_anova.tsv")

##backwards pruning
#variable_highest_p_value <- anova_full_model[1,1]

to_drop_df <- anova_full_model %>% 
  slice_max(order_by = `Pr(>Chi)`) 

#if there's more than one to kick out, kick out the one that is an interaction
if(nrow(to_drop_df) != 1 & filter(to_drop_df, interactions == 2) %>% nrow()){
  
  to_drop_var <-   filter(to_drop_df, interactions == 2) %>% 
    .$variable
  }

#if there aren't interactions, randomly pick one of the same valued ones
if(nrow(to_drop_df) != 1 & (filter(to_drop_df, interactions == 2) %>% nrow() < 1)){
  
  to_drop_var <-   to_drop_df %>% 
    sample_n(size = 1) %>% 
    .$variable
}





