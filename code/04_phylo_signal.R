source("01_requirements.R")

tree_SBZR <- ape::read.tree("output/processed_data/tree_sbzr.tree")
tree_SBZR$tip.label <- str_replace_all(tree_SBZR$tip.label, "_", " ")
tree_SBZR$tip.label <- str_replace_all(tree_SBZR$tip.label, "-Wallis", "(Wallis)")


data_SBZR <- read_tsv("output/processed_data/RO_aggregate_SBZR_group_scaled.tsv", show_col_types = F) %>% 
  column_to_rownames("SBZR_group") %>% 
  dplyr::select(EA033) %>% 
  as.matrix()

tree_medium <- ape::read.tree("output/processed_data/tree_medium.tree")
tree_medium$tip.label <- str_replace_all(tree_medium$tip.label, "_", " ")
tree_medium$tip.label <- str_replace_all(tree_medium$tip.label, "-Wallis", "(Wallis)")
tree_medium$tip.label <- str_replace_all(tree_medium$tip.label, "-New Caledonia main island", "(New Caledonia main island)")
tree_medium$tip.label <- str_replace_all(tree_medium$tip.label, "-greater", "(greater)")
tree_medium$tip.label <- str_replace_all(tree_medium$tip.label, "-south", "(south)")

data_medium <- read_tsv("output/processed_data/RO_aggregate_medium_group_scaled.tsv", show_col_types = F) %>% 
  column_to_rownames("Medium_only_merged_for_shared_language") %>% 
  dplyr::select(EA033) %>% 
  as.matrix()



#testing
sbzr_phylosig_k <- phytools::phylosig(tree = tree_SBZR, x = data_SBZR, method = "K")
medium_phylosig_k <- phytools::phylosig(tree = tree_medium, x = data_medium, method = "K")

sbzr_phylosig_lambda <- phytools::phylosig(tree = tree_SBZR, x = data_SBZR, method = "lambda")
medium_phylosig_lambda <- phytools::phylosig(tree = tree_medium, x = data_medium, method = "lambda")



