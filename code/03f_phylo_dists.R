source("01_requirements.R")
source("fun_keep_as_tip.R")

glottolog_df <- read.delim("data/glottolog_language_table_wide_df.tsv", sep = "\t") %>% 
  mutate(Language_level_ID = ifelse(is.na(Language_level_ID), Glottocode, Language_level_ID)) %>% 
  dplyr::select(Language_level_ID, Glottocode, subclassification, level)
  
df_line <- glottolog_df %>% 
  filter(Language_level_ID == "mala1545") %>% 
  dplyr::select("subclassification") 

tree_full <- ape::read.tree(text = df_line$subclassification)

polygons <- read_csv("data/RO_polygons_grouped_with_languages.csv", 
                     show_col_types = F) %>% 
  filter(!is.na(glottocodes)) %>%
  filter(glottocodes != "") %>% 
  mutate(glottocodes = str_split(glottocodes, ",")) %>%
  unnest(glottocodes) %>% 
  mutate(Glottocode = trimws(glottocodes)) %>% 
  distinct(Glottocode) %>% 
  left_join(glottolog_df, by = "Glottocode") 

nodes_and_tips <- c(tree_full$tip.label, tree_full$node.label)

tree_pruned <- keep_as_tip(tree_full, tips_and_nodes_to_keep = nodes_and_tips[nodes_and_tips %in% polygons$Language_level_ID])

tree_pruned <- compute.brlen(tree_pruned, method = 1)

tree_pruned$edge.length = tree_pruned$edge.length / 1000

tree_pruned %>% ape::write.tree("output/processed_data/tree_glottolog_newick.tree")

# https://github.com/grambank/grambank-analysed/blob/main/R_grambank/spatiophylogenetic_modelling/analysis/make_precisionmatrices.R

## Calculate precision using typical variance rescaling
# Here, we calculate the precison matrix, including all nodes and tips
# By including the nodes, we create a sparse matrix, which results in significant
# time improvements within INLA. Note we don't want to scale the phylogeny
# because we are doing that ourselves in a moment
phy_inv_nodes = MCMCglmm::inverseA(tree_pruned,
                                   nodes = "TIPS",
                                   scale = FALSE)$Ainv

# Next, we invert the precison matrix - creating the covariance matrix
# and standardize by the typical variance, to ensure variance is scaled to 1
phy_covar_nodes = solve(phy_inv_nodes)
typical_phylogenetic_variance = exp(mean(log(diag(phy_covar_nodes))))
phy_cov_std = phy_covar_nodes / typical_phylogenetic_variance

dimnames(phy_cov_std) = dimnames(phy_inv_nodes)

tree_scaled <- tree_pruned

tree_scaled$edge.length <- tree_scaled$edge.length / typical_phylogenetic_variance
phy_prec_mat_new <- MCMCglmm::inverseA(tree_scaled,
                                       nodes = "TIPS",
                                       scale = FALSE)$Ainv

# double check the matrix is more or less the same
phy_prec_mat = solve(phy_cov_std)
dimnames(phy_prec_mat) = dimnames(phy_inv_nodes)

## matrices are identical up to tolerance
all.equal(as.matrix(phy_prec_mat), as.matrix(phy_prec_mat_new))

## using this new phylo precision matrix should perform much better
phy_prec_mat <- phy_prec_mat_new %>% as.matrix() 

colnames(phy_prec_mat) <- rownames(phy_prec_mat)

phy_prec_mat %>% 
  as.data.frame() %>% 
  rownames_to_column("Rownames") %>% 
write_tsv("output/processed_data/phy_prec_mat.tsv", na = "")

