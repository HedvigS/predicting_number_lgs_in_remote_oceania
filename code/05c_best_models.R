source("01_requirements.R")
source("fun_def_brms_analysis.R")

#inspired by
#https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/counting-and-classification.html#binomial-regression

###SBZR
data <- read_tsv("output/processed_data/RO_aggregate_SBZR_group_scaled.tsv", show_col_types = F) %>%  
  rename(group = SBZR_group) %>% 
  mutate(spatial_id = group) %>% 
  mutate(phylo_id = group)

group = "SBZR"

phylo_vcv <- readRDS("output/processed_data/tree_SBZR_vcv.rds")
spatial_vcv <- readRDS("output/processed_data/spatial_vcv_SBZR.rds")
data2 = list(phylo_vcv = phylo_vcv, spatial_vcv = spatial_vcv)

#phylo
formula <- lg_count  ~    (1 | gr(phylo_id, cov = phylo_vcv)) +
  environ_PC1*Shoreline +
  environ_PC2*Shoreline +
  Shoreline*mo(EA033) + 
  Shoreline*mo(Settlement_date_grouping_finer)

fun_brms_predicting(data = data, data2 = data2, formula = formula, group = group, control = "phylo", drop_one_out = TRUE)

#none 
formula <- lg_count  ~    environ_PC1*Shoreline +
  environ_PC2*Shoreline +
  Shoreline*mo(EA033) + 
  Shoreline*mo(Settlement_date_grouping_finer)

fun_brms_predicting(data = data, data2 = NULL, formula = formula, group = group, control = "none", drop_one_out = TRUE)

##############################################################################
#MEDIUM
data <- read_tsv("output/processed_data/RO_aggregate_medium_group_scaled.tsv", show_col_types = F) %>%  
  rename(group = Medium_only_merged_for_shared_language) %>% 
  mutate(spatial_id = group) %>% 
  mutate(phylo_id = group)

group = "medium"

phylo_vcv <- readRDS("output/processed_data/tree_medium_vcv.rds")
spatial_vcv <- readRDS("output/processed_data/spatial_vcv_medium.rds")
data2 = list(phylo_vcv = phylo_vcv, spatial_vcv = spatial_vcv)

#spatialphylo
formula <- lg_count  ~    (1 | gr(phylo_id, cov = phylo_vcv)) +
  (1 | gr(spatial_id, cov = spatial_vcv)) +
  environ_PC1*Shoreline +
  environ_PC2*Shoreline +
  environ_PC3*Shoreline +
  Shoreline*mo(EA033) + 
  Shoreline*mo(Settlement_date_grouping_finer)

fun_brms_predicting(data = data, data2 = data2, formula = formula, group = group, control = "spatialphylo", drop_one_out = TRUE )

#none
formula <- lg_count  ~    environ_PC1*Shoreline +
  environ_PC2*Shoreline +
  environ_PC3*Shoreline +
  Shoreline*mo(EA033) + 
  Shoreline*mo(Settlement_date_grouping_finer)

#control = "none"

fun_brms_predicting(data = data, data2 = NULL, formula = formula, group = group, control = "none", drop_one_out = TRUE)
