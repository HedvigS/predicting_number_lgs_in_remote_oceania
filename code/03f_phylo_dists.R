source("01_requirements.R")
source("fun_keep_as_tip.R")

glottolog_df <- read.delim("output/processed_data/glottolog_language_table_wide_df.tsv", sep = "\t") %>% 
  mutate(Language_level_ID = ifelse(is.na(Language_level_ID), Glottocode, Language_level_ID)) %>% 
  dplyr::select(Language_level_ID, Glottocode, subclassification, level, classification)

tree_fn <- "output/processed_data/glottolog_tree_mala1545.tree"
if(!file.exists(tree_fn)){
  source("03f_pruning_tree.R")
}

tree_pruned <- ape::read.tree("output/processed_data/glottolog_tree_mala1545.tree")

tree_dists <- adephylo::distTips(tree_pruned, method = "patristic")

polygons <- read_csv("data/RO_polygons_grouped_with_languages.csv", 
                     show_col_types = F) %>% 
  filter(!is.na(glottocodes)) %>%
  filter(glottocodes != "") %>% 
  mutate(glottocodes = str_split(glottocodes, ",")) %>%
  unnest(glottocodes) %>% 
  mutate(Glottocode = trimws(glottocodes)) %>% 
  left_join(glottolog_df, by = "Glottocode") 

left <- polygons %>% 
  dplyr::select(Var1 = Language_level_ID, 
                SBZR_group_Var1 = SBZR_group, 
                Medium_only_merged_for_shared_language_Var1 = Medium_only_merged_for_shared_language) %>% 
  distinct()

right <- polygons %>% 
  dplyr::select(Var2 = Language_level_ID, 
                SBZR_group_Var2 = SBZR_group, 
                Medium_only_merged_for_shared_language_Var2 = Medium_only_merged_for_shared_language) %>% 
  distinct()

tree_dists_list <- tree_dists %>% 
  as.matrix() %>% 
  reshape2::melt() %>% 
  left_join(left, by = "Var1", relationship = "many-to-many") %>% 
  left_join(right, by = "Var2", relationship = "many-to-many") %>% 
  distinct()

#medium

data <- read_tsv("output/processed_data/RO_aggregate_medium_group_scaled.tsv", show_col_types = F) %>%  
  dplyr::select(Medium_only_merged_for_shared_language)

tree_dists_list_medium <- tree_dists_list %>% 
  distinct(Var1, Var2, Medium_only_merged_for_shared_language_Var1, Medium_only_merged_for_shared_language_Var2, value) %>% 
  filter(!is.na(Medium_only_merged_for_shared_language_Var1)) %>% 
  filter(!is.na(Medium_only_merged_for_shared_language_Var2)) %>% 
    group_by(Medium_only_merged_for_shared_language_Var1, Medium_only_merged_for_shared_language_Var2) %>% 
  summarise(value = median(value), .groups = "drop") %>% 
  reshape2::dcast(Medium_only_merged_for_shared_language_Var1 ~ 
                    Medium_only_merged_for_shared_language_Var2, value.var = "value") %>% 
  column_to_rownames("Medium_only_merged_for_shared_language_Var1") %>% 
  as.matrix()

col_vec <- colnames(tree_dists_list_medium) %in% data$Medium_only_merged_for_shared_language
row_vec <- rownames(tree_dists_list_medium) %in% data$Medium_only_merged_for_shared_language

tree_dists_list_medium <- tree_dists_list_medium[row_vec, col_vec]

tree_medium <- ape::nj(X = tree_dists_list_medium)
tree_medium <- phytools::midpoint_root(tree_medium)
tree_medium <- ape::compute.brlen(tree_medium, method = "grafen")

png("output/plots/island_group_tree_medium.png", width = 20, height = 30, units = "cm", res = 300)
plot(ladderize(tree_medium), cex = 0.9, edge.color = "#4c1c80", no.margin = T, edge.width = 2.7, label.offset = 0.03)
x <- dev.off()

png("../latex/island_group_tree_medium.png", width = 20, height = 30, units = "cm", res = 300)
plot(ladderize(tree_medium), cex = 0.9, edge.color = "#4c1c80", no.margin = T, edge.width = 2.7, label.offset = 0.03)
x <- dev.off()

ape::vcv.phylo(tree_medium, corr = FALSE) %>%  
    saveRDS("output/processed_data/tree_medium_vcv.rds")

tree_medium %>% 
  ape::write.tree(file = "output/processed_data/tree_medium.tree")

#SBZR
tree_dists_list_SBZR <- tree_dists_list %>%
  distinct(Var1, Var2, SBZR_group_Var1, SBZR_group_Var2, value) %>% 
  filter(!is.na(SBZR_group_Var1)) %>% 
  filter(!is.na(SBZR_group_Var2)) %>% 
  group_by(SBZR_group_Var1, SBZR_group_Var2) %>% 
  summarise(value = median(value), .groups = "drop") %>% 
  reshape2::dcast(SBZR_group_Var1 ~ 
                    SBZR_group_Var2, value.var = "value") %>% 
  column_to_rownames("SBZR_group_Var1") %>% 
  as.matrix()

data <- read_tsv("output/processed_data/RO_aggregate_SBZR_group_scaled.tsv", show_col_types = F) %>%  
  dplyr::select(SBZR_group)

col_vec <- colnames(tree_dists_list_SBZR) %in% data$SBZR_group
row_vec <- rownames(tree_dists_list_SBZR) %in% data$SBZR_group

tree_dists_list_SBZR <- tree_dists_list_SBZR[row_vec, col_vec]

tree_SBZR <- ape::nj(X = tree_dists_list_SBZR)
tree_SBZR <- phytools::midpoint_root(tree_SBZR)
tree_SBZR <- ape::compute.brlen(tree_SBZR, method = "grafen")

png("output/plots/island_group_tree_SBZR.png", width = 20, height = 30, units = "cm", res = 300)
plot(ladderize(tree_SBZR), cex = 0.9, edge.color= "#22519c", no.margin = T, edge.width = 2.7, label.offset = 0.03)
x <- dev.off()

png("../latex/island_group_tree_SBZR.png", width = 20, height = 30, units = "cm", res = 300)
plot(ladderize(tree_SBZR), cex = 0.9, edge.color= "#22519c", no.margin = T, edge.width = 2.7, label.offset = 0.03)
x <- dev.off()

ape::vcv.phylo(tree_SBZR, corr = FALSE) %>% 
  saveRDS("output/processed_data/tree_SBZR_vcv.rds")

tree_SBZR %>% 
  ape::write.tree(file = "output/processed_data/tree_sbzr.tree")
