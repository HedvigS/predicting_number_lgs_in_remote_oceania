source("01_requirements.R")
source("fun_keep_as_tip.R")

tree_fn <- "output/processed_data/glottolog_tree_mala1545.tree"
if(!file.exists(tree_fn)){
  source("03f_pruning_tree.R")
}

tree_pruned <- ape::read.tree("output/processed_data/glottolog_tree_mala1545.tree")

polygons <- read_csv("data/RO_polygons_grouped_with_languages.csv", 
                     show_col_types = F) %>% 
  filter(!is.na(glottocodes)) %>%
  filter(glottocodes != "") %>% 
  mutate(glottocodes = str_split(glottocodes, ",")) %>%
  unnest(glottocodes) %>% 
  mutate(Glottocode = trimws(glottocodes)) %>% 
  distinct(Glottocode) %>% 
  left_join(glottolog_df, by = "Glottocode") 

tree_dists <- adephylo::distTips(tree_pruned, method = "patristic", tips = polygons$Glottocode)




