source("01_requirements.R")
source("01_requirements_brms.R")
source("fun_def_brms_analysis.R")

phy_prec_mat            <- read_tsv("output/processed_data/phy_prec_mat.tsv", na = "", show_col_types = F)
spatial_prec_mat_medium <- read_tsv("output/processed_data/spatial_prec_mat_medium.tsv", na = "", show_col_types = F)

tree <- ape::read.tree("output/processed_data/tree_glottolog_newick.tree")
tree_vcv <- ape::vcv.phylo(tree, corr = FALSE)

data <- read_tsv("output/processed_data/RO_aggregate_medium_group_scaled.tsv", show_col_types = F) %>%  
  rename(group = Medium_only_merged_for_shared_language)

formula <- lg_count  ~    (1 | gr(tree, cov = tree_vcv)) +
  environ_PC1*Shoreline +
  environ_PC2*Shoreline +
  environ_PC3*Shoreline +
  mo(EA033) + 
  Shoreline*mo(Settlement_date_grouping_finer) 

group = "medium"
data2 = list(tree_vcv = tree_vcv, tree = tree)


tree = tree
iter = 30000
warmup = 1000
chains = 4
cores = 4
seed = 10
ndraws = 10000

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
