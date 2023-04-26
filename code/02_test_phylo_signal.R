source("01_requirements.R")
h_load(pkg = c("caper", "ape", "phytools"))

tree <- ape::read.tree("data/trees/gray_et_al_tree_pruned_newick_mcct.txt")

pol_complex_df <- readODS::read_ods("data/Remote_oceania_pol_complex_hedvig_code_latex.ods", sheet = 1) %>%
  dplyr::select(Language_level_ID = glottocode, `Political complexity (EA033)`) %>% 
  mutate(glottocode = ifelse(Language_level_ID == "fiji1243", "fiji1243,kada1285,sout2864,nort2843", Language_level_ID)) %>% 
  mutate(glottocode = ifelse(Language_level_ID == "aust1304",  "aust1304,raiv1237,tubu1240,ruru123", glottocode)) %>% 
  mutate(glottocode = ifelse(Language_level_ID == "maor1246", "maor1246,mori1267", glottocode)) %>% 
  mutate(glottocode = str_split(glottocode, ",")) %>% 
  unnest(cols = c(glottocode)) %>% 
  dplyr::select(Language_level_ID, `Political complexity (EA033)`) %>% 
  distinct(Language_level_ID, .keep_all = T) %>% 
  as.data.frame()

vec <- as.vector(x = pol_complex_df$`Political complexity (EA033)`)
names(vec) <- pol_complex_df$Language_level_ID


output <- phytools::phylosig(tree = tree, x = vec, method = "lambda")

output$lambda