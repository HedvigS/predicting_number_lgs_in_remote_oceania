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
  Shoreline*Settlement_date_grouping_finer

group = "SBZR"

fun_brms_predicting(data = data, formula = formula, group = group)


#MEDIUM

data <- read_tsv("output/processed_data/RO_aggregate_medium_group_scaled.tsv", show_col_types = F) %>%  
  rename(group = Medium_only_merged_for_shared_language)

formula <- "lg_count  ~  environ_PC1*Shoreline +
  environ_PC2*Shoreline +
  environ_PC3*Shoreline +
  EA033 + 
  Shoreline*Settlement_date_grouping_finer"

group = "medium"

fun_brms_predicting(data = data, formula = formula, group = group)

