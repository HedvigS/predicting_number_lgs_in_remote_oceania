source("01_requirements.R")
source("fun_keep_as_tip.R")

glottolog_df <- read.delim("output/processed_data/glottolog_language_table_wide_df.tsv", sep = "\t") %>% 
  mutate(Language_level_ID = ifelse(is.na(Language_level_ID), Glottocode, Language_level_ID)) %>% 
  dplyr::select(Language_level_ID, Glottocode, subclassification, level, classification)

mala1545_lgs <- glottolog_df %>% 
  filter(str_detect(classification, "mala1545")) %>% 
  filter(level == "language")

df_line <- glottolog_df %>% 
  filter(Language_level_ID == "mala1545") %>% 
  dplyr::select("subclassification") 

tree_full <- ape::read.tree(text = df_line$subclassification)

tree_pruned <- keep_as_tip(tree_full, tips_and_nodes_to_keep = mala1545_lgs$Language_level_ID)

tree_pruned <- ape::compute.brlen(tree_pruned, method = "grafen")

tree_pruned %>% 
  ape::write.tree("output/processed_data/glottolog_tree_mala1545.tree")