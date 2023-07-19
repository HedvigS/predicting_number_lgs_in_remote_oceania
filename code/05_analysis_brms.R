source("01_requirements.R")
source("01_requirements_brms.R")

source("fun_def_brms_analysis_5.R")

###MARCK
data <- read_tsv("output/processed_data/RO_Hedvig_aggregate_marck_group_scaled.tsv", show_col_types = F) %>%  
  rename(group = Marck_group)

data %>% colnames()

#inspired by
#https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/counting-and-classification.html#binomial-regression

#model specs
formula <- "lg_count  ~   Annual_precipitation_mean+ 
  Precipitation_seasonality_mean + 
  Annual_temperature_mean + 
  Temperature_seasonality_mean + 
  Latitude_abs_mean +
  NPP_terra_mean +
  NPP_aqua_mean +
#  Carrying_capactiy_PC1 + 
#  Carrying_capactiy_PC2 +
#  Carrying_capactiy_PC1:Shoreline +
#  Carrying_capactiy_PC2:Shoreline +
  EA033 + 
  Shoreline +
  Settlement_date_grouping_finer"

group = "Marck"


fun_hedvig_brms_predicting(data = data, formula = formula, group = group)


#MEDIUM

data <- read_tsv("output/processed_data/RO_Hedvig_aggregate_medium_group_scaled.tsv", show_col_types = F) %>%  
  rename(group = Medium_only_merged_for_shared_language)

formula <- "lg_count  ~ Carrying_capactiy_PC1 + 
  Carrying_capactiy_PC2 +
  Carrying_capactiy_PC3 +
  Carrying_capactiy_PC1:Shoreline:EA033 +
  Carrying_capactiy_PC2:Shoreline:EA033 +
  Carrying_capactiy_PC3:Shoreline:EA033 +
  EA033 + 
  Shoreline +
  Settlement_date_grouping_finer"

group = "medium"

fun_hedvig_brms_predicting(data = data, formula = formula, group = group)

