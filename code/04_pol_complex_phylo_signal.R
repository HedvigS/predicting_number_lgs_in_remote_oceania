source("01_requirements.R")

scrub_characters <- function(x){
  x %>%   
    str_replace_all("ē", "e") %>% 
    str_replace_all("ā", "a") %>% 
    str_replace_all("å", "a") %>%   
    str_replace_all("\\(", "") %>%   
    str_replace_all("\\)", "") %>%   
    str_replace_all("\\+", "") %>%   
    str_replace_all("‘", "") %>%   
    str_replace_all(" ", "_") %>%   
    str_replace_all("__", "_") %>%   
    str_replace_all("-", "") %>%   
    stringi::stri_enc_toascii() 
  
}

tree_medium <- ape::read.tree("output/processed_data/tree_medium.tree")
tree_SBZR <- ape::read.tree("output/processed_data/tree_sbzr.tree")

tree_medium$tip.label <- tree_medium$tip.label %>% scrub_characters
tree_SBZR$tip.label <- tree_SBZR$tip.label %>% scrub_characters

data_medium <- read_tsv("output/processed_data/RO_aggregate_medium_group_scaled.tsv", show_col_types = F) %>% 
  mutate(Medium_only_merged_for_shared_language = Medium_only_merged_for_shared_language %>% scrub_characters) %>% 
  mutate(spatial_id = Medium_only_merged_for_shared_language) %>% 
  mutate(phylo_id = Medium_only_merged_for_shared_language) %>% 
  mutate(group = Medium_only_merged_for_shared_language) %>% 
  column_to_rownames("Medium_only_merged_for_shared_language") 

vec_medium_pol_complex <- data_medium %>% 
  dplyr::select("EA033") %>% 
  as.matrix()

vec_medium_lg_count <- data_medium %>% 
  dplyr::select("lg_count") %>% 
  as.matrix()

data_SBZR <- read_tsv("output/processed_data/RO_aggregate_SBZR_group_scaled.tsv", show_col_types = F)   %>% 
  mutate(SBZR_group = SBZR_group %>% scrub_characters) %>% 
  column_to_rownames("SBZR_group") 

vec_SBZR_pol_complex <-  data_SBZR %>% 
  dplyr::select("EA033") %>% 
  as.matrix()

vec_SBZR_lg_count <-data_SBZR %>% 
  dplyr::select("lg_count") %>% 
  as.matrix()

medium_results_pol_complex <- phytools::phylosig(tree = tree_medium, x = vec_medium_pol_complex, method = "K")
phytools::dotTree(tree = tree_medium, x = vec_medium_pol_complex)
medium_results_pol_complex

medium_results_lg_count <- phytools::phylosig(tree = tree_medium, x = vec_medium_lg_count, method = "K")
phytools::dotTree(tree = tree_medium, x = vec_medium_lg_count)
medium_results_lg_count

SBZR_results_pol_complex <- phytools::phylosig(tree = tree_SBZR, x = vec_SBZR_pol_complex, method = "K")
phytools::dotTree(tree = tree_SBZR, x = vec_SBZR_pol_complex)
SBZR_results_pol_complex

SBZR_results_lg_count <- phytools::phylosig(tree = tree_medium, x = vec_medium_lg_count, method = "K")
phytools::dotTree(tree = tree_SBZR, x = vec_SBZR_lg_count)
SBZR_results_lg_count 



phylo_vcv <- readRDS("output/processed_data/tree_medium_vcv.rds")
spatial_vcv <- readRDS("output/processed_data/spatial_vcv_medium.rds")

rownames(phylo_vcv) <- rownames(phylo_vcv) %>% scrub_characters()
colnames(phylo_vcv) <- colnames(phylo_vcv) %>% scrub_characters()

data2 = list(phylo_vcv = phylo_vcv, spatial_vcv = spatial_vcv)


data_medium %>% colnames()
#spatial phylo
formula <-   EA033   ~    (1 | gr(phylo_id, cov = phylo_vcv)) 

iter = 30000
warmup = 1000
chains = 4
cores = 4
seed = 10

model_fun <- function(data, formula){
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
}


output_poisson %>% summary() 

