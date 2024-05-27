source("01_requirements.R")
source("01_requirements_brms.R")

###SBZR
data <- read_tsv("output/processed_data/RO_aggregate_SBZR_group_scaled.tsv", show_col_types = F) %>%  
  rename(group = SBZR_group) %>% 
  mutate(spatial_id = group) %>% 
  mutate(phylo_id = group)

group = "SBZR"

phylo_vcv <- readRDS("output/processed_data/tree_SBZR_vcv.rds")
spatial_vcv <- readRDS("output/processed_data/spatial_vcv_SBZR.rds")
data2 = list(phylo_vcv = phylo_vcv, spatial_vcv = spatial_vcv)

formula <- lg_count  ~    (1 | gr(phylo_id, cov = phylo_vcv)) +
  (1 | gr(spatial_id, cov = spatial_vcv)) +
  environ_PC1*Shoreline +
  environ_PC2*Shoreline + 
  Shoreline*mo(EA033) + 
  Shoreline*mo(Settlement_date_grouping_finer)


output_poisson <-  brms::brm(data = data, 
                             data2 = data2,
                             family = poisson,
                             formula = formula,
                             iter = iter, 
                             silent = 2,
                             refresh = 0,
                             warmup = warmup, 
                             chains = chains, 
                             cores = cores,
                             seed = seed,
                             control = list(adapt_delta = 0.9),
                             backend="cmdstanr") 
