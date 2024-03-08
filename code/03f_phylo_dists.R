source("01_requirements.R")

glottolog_df <- read.delim("data/glottolog_language_table_wide_df.tsv", sep = "\t")

  
#  read_tsv("data/glottolog_language_table_wide_df.tsv", 
#                        show_col_types = F)

df_line <- glottolog_df %>% 
  filter(Language_ID == "cent2080") %>% 
  dplyr::select("subclassification") 

df_line$subclassification  %>% str()
tree_full <- ape::read.tree(text = df_line$subclassification)

tree_full %>% str()


df_line$subclassification %>% str()

#get all labels (nodes and tips)
oceanic_tip_labels <- oceanic_tree_full$tip.label
oceanic_node_labels <- oceanic_tree_full$node.label
oceanic_all_labels_df <- c(oceanic_tip_labels, oceanic_node_labels) %>% 
  as.data.frame() %>%
  rename(Language_ID = ".")


polygons <- read_csv("data/RO_polygons_grouped_with_languages.csv", 
                     show_col_types = F) %>% 
  filter(!is.na(glottocodes)) %>%
  filter(glottocodes != "") %>% 
  mutate(glottocodes = str_split(glottocodes, ",")) %>%
  unnest(glottocodes) %>% 
  mutate(Glottocode = trimws(glottocodes)) 




#subtree = TRUE


#read in GB
GB_df <- read_tsv(GB_binary_fn) %>% 
  dplyr::select(Language_ID) %>% 
  filter(Language_ID != "cent2060") %>% #removing proto-languages
  filter(Language_ID != "east2449") %>% #removing proto-languages
  filter(Language_ID != "poly1242") %>% #removing proto-languages
  filter(Language_ID != "ocea1241") #removing proto-languages

#make df of only things that are languages and oceanic
oceanic_lgs <- read_tsv("output/processed_data/glottolog_oceanic_languages_df.tsv", show_col_types = F) %>% 
  dplyr::select("Language_ID") %>% 
  as.matrix() %>% 
  as.vector()

#find overlap between glottolog and GB
overlap <- inner_join(GB_df, oceanic_all_labels_df, by = "Language_ID") %>% 
  as.matrix() %>% 
  as.vector()

pruned_tree <-keep_as_tip(oceanic_tree_full, overlap)

pruned_tree <- compute.brlen(pruned_tree, method = 1)

pruned_tree  %>% ape::write.tree( "output/processed_data/trees/glottolog_tree_newick_GB_pruned.txt")

polytomies_n <- pruned_tree$edge %>% 
  as.data.frame() %>% 
  group_by(V1) %>% 
  summarise(n = n()) %>% 
  filter(n > 2) %>% nrow()

splits <- pruned_tree$edge[,1] %>% length()

message("The Glottolog Oceanic tree (pruned for Grambank matches) has ", splits, " splits. Out of these ",  round(polytomies_n/splits*100, 0), "% are non-binary.")

#prune tree to only languages in oceanic subgroup
oceanic_tree <-keep_as_tip(oceanic_tree_full, oceanic_lgs)

oceanic_tree <- compute.brlen(oceanic_tree, method = 1)



oceanic_tree  %>% ape::write.tree("output/processed_data/trees/glottolog_tree_newick_all_oceanic.txt")







































# https://github.com/grambank/grambank-analysed/blob/main/R_grambank/spatiophylogenetic_modelling/analysis/make_precisionmatrices.R

## Calculate precision using typical variance rescaling
# Here, we calculate the precison matrix, including all nodes and tips
# By including the nodes, we create a sparse matrix, which results in significant
# time improvements within INLA. Note we don't want to scale the phylogeny
# because we are doing that ourselves in a moment
phy_inv_nodes = MCMCglmm::inverseA(gray_2009_mcct_pruned ,
                                   nodes = "ALL",
                                   scale = FALSE)$Ainv

# Next, we invert the precison matrix - creating the covariance matrix
# and standardize by the typical variance, to ensure variance is scaled to 1
phy_covar_nodes = solve(phy_inv_nodes)
typical_phylogenetic_variance = exp(mean(log(diag(phy_covar_nodes))))
phy_cov_std = phy_covar_nodes / typical_phylogenetic_variance
