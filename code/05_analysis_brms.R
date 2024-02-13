source("01_requirements.R")
source("01_requirements_brms.R")
source("fun_def_brms_analysis.R")

###SBZR
data <- read_tsv("output/processed_data/RO_aggregate_SBZR_group_scaled.tsv", show_col_types = F) %>%  
  rename(group = SBZR_group) 

#inspired by
#https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/counting-and-classification.html#binomial-regression

#model specs
formula <- lg_count  ~   environ_PC1*Shoreline +
  environ_PC2*Shoreline +
  mo(EA033) + 
  Shoreline*mo(Settlement_date_grouping_finer)

group = "SBZR"

# After the full model is rendered, we drop out one observation at a time and use those coefs to predict values for all observations. This illustrates the contribution of the dropped out data-point for the model fit. It is not possible to treat time as monotonic when doing this because there is only one member of time settlement group 12 (Laguas yan Gåni). When it is dropped out, and we then try and predict for all observations the model breaks as it doesn't know what to do with the level 12 for time. Therefore, for the part where we drop out one obs at a time specifically we treat time as continous instead of monotonic.

formula_drop_one_out <- lg_count  ~   environ_PC1*Shoreline +
  environ_PC2*Shoreline +
  mo(EA033) + 
  Shoreline*Settlement_date_grouping_finer


fun_brms_predicting(data = data, formula = formula, group = group)


#MEDIUM

data <- read_tsv("output/processed_data/RO_aggregate_medium_group_scaled.tsv", show_col_types = F) %>%  
  rename(group = Medium_only_merged_for_shared_language)

formula <- lg_count  ~  environ_PC1*Shoreline +
  environ_PC2*Shoreline +
  environ_PC3*Shoreline +
  mo(EA033) + 
  Shoreline*mo(Settlement_date_grouping_finer)

group = "medium"

formula_drop_one_out <- lg_count  ~  environ_PC1*Shoreline +
  environ_PC2*Shoreline +
  environ_PC3*Shoreline +
  mo(EA033) + 
  Shoreline*Settlement_date_grouping_finer

fun_brms_predicting(data = data, formula = formula, group = group)

