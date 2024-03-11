source("01_requirements.R")
source("fun_keep_as_tip.R")

tree_fn <- "output/processed_data/glottolog_tree_mala1545.tree"
if(!file.exists(tree_fn)){
  source("03f_pruning_tree.R")
}

tree_pruned <- ape::read.tree("output/processed_data/glottolog_tree_mala1545.tree")



tree_dists <- adephylo::distTips(tree_full, )


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









phy_prec_mat %>% 
  as.data.frame() %>% 
  rownames_to_column("Rownames") %>% 
write_tsv("output/processed_data/phy_prec_mat.tsv", na = "")

