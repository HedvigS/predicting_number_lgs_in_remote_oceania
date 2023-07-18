source("01_requirements.R")
source("01_requirements_brms.R")

source("fun_def_brms_analysis_5.R")

###MARCK
data <- read_tsv("output/processed_data/RO_Hedvig_aggregate_marck_group_scaled.tsv", show_col_types = F) %>%  
  mutate("Combined_CCPC1_Size_EA033" = Carrying_capactiy_PC1*Shoreline*EA033) %>% 
  mutate("Combined_CCPC2_Size_EA033" = Carrying_capactiy_PC2*Shoreline*EA033) %>% 
  rename(group = Marck_group)

#inspired by
#https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/counting-and-classification.html#binomial-regression


#model specs
formula <- "lg_count  ~ Carrying_capactiy_PC1 + 
  Carrying_capactiy_PC2 +
  EA033 + 
  Shoreline +
  Settlement_date_grouping_finer"

group = "Marck"


fun_hedvig_brms_predicting(data = data, formula = formula, group = group)


#MEDIUM

data <- read_tsv("output/processed_data/RO_Hedvig_aggregate_medium_group_scaled.tsv", show_col_types = F) %>%  
  mutate("Combined_CCPC1_Size_EA033" = Carrying_capactiy_PC1*Shoreline*EA033) %>% 
  mutate("Combined_CCPC2_Size_EA033" = Carrying_capactiy_PC2*Shoreline*EA033) %>% 
  mutate("Combined_CCPC3_Size_EA033" = Carrying_capactiy_PC2*Shoreline*EA033) %>% 
  rename(group = Medium_only_merged_for_shared_language)

formula <- "lg_count  ~ Carrying_capactiy_PC1 + 
  Carrying_capactiy_PC2 +
  Carrying_capactiy_PC3 +
  EA033 + 
  Shoreline +
  Settlement_date_grouping_finer"

group = "medium"

fun_hedvig_brms_predicting(data = data, formula = formula, group = group)

