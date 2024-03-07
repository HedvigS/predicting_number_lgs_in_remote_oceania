source("01_requirements.R")

glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv", 
                        show_col_types = F)

polygons <- read_csv("data/RO_polygons_grouped_with_languages.csv", 
                     show_col_types = F) %>% 
  filter(!is.na(glottocodes)) %>%
  filter(glottocodes != "") %>% 
  mutate(glottocodes = str_split(glottocodes, ",")) %>%
  unnest(glottocodes) %>% 
  mutate(glottocode = trimws(glottocodes)) 
  
lgs <- polygons$glottocode %>% unique()

#read in and prune tree
gray_2009_mcct <- ape::read.tree("data/trees/gray_et_al2009/original/a400-m1pcv-time.mcct.trees.gz")

taxa <- read_csv(file = "data/trees/gray_et_al2009/taxa.csv", show_col_types = F) %>% 
  rename(Glottocode = glottocode) #to conform to what glottolog does elsewhere 

tree_removed_dups <- drop.tip(gray_2009_mcct, tip = gray_dup_to_remove)

Gray_et_al_tree_tip.label_df <- tree_removed_dups$tip.label %>% 
  as.data.frame() %>% 
  rename(taxon = ".") %>% 
  left_join(taxa, by = "taxon") %>% 
  left_join(glottolog_df, by = "Glottocode") 

tree_removed_dups$tip.label <- Gray_et_al_tree_tip.label_df$taxon

overlap <- intersect(tree_removed_dups$tip.label, lgs)

gray_2009_mcct <- ape::keep.tip(phy = tree_removed_dups,
                                tip = overlap)

gray_2009_mcct$edge.length = gray_2009_mcct$edge.length / 1000

# https://github.com/grambank/grambank-analysed/blob/main/R_grambank/spatiophylogenetic_modelling/analysis/make_precisionmatrices.R

## Calculate precision using typical variance rescaling
# Here, we calculate the precison matrix, including all nodes and tips
# By including the nodes, we create a sparse matrix, which results in significant
# time improvements within INLA. Note we don't want to scale the phylogeny
# because we are doing that ourselves in a moment
phy_inv_nodes = MCMCglmm::inverseA(gray_2009_mcct,
                                   nodes = "ALL",
                                   scale = FALSE)$Ainv

# Next, we invert the precison matrix - creating the covariance matrix
# and standardize by the typical variance, to ensure variance is scaled to 1
phy_covar_nodes = solve(phy_inv_nodes)
typical_phylogenetic_variance = exp(mean(log(diag(phy_covar_nodes))))
phy_cov_std = phy_covar_nodes / typical_phylogenetic_variance
