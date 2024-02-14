source("01_requirements.R")
source("fun_def_varcov_spatial.R")  

polygons <- read_csv("data/RO_polygons_grouped_with_languages.csv", show_col_types = FALSE) %>% 
  filter(!is.na(SBZR_group) & !is.na(Medium_only_merged_for_shared_language)) %>% 
  dplyr::select(Longitude, Latitude, Unique_ID, SBZR_group,Medium_only_merged_for_shared_language) 

#left and right for grouping later
left <- polygons %>% 
  dplyr::select(Var1 = Unique_ID, 
                SBZR_group_Var1 = SBZR_group,
                Medium_only_merged_for_shared_language_Var1 = Medium_only_merged_for_shared_language) 


right <- polygons %>% 
  dplyr::select(Var2 = Unique_ID, 
                SBZR_group_Var2 = SBZR_group,
                Medium_only_merged_for_shared_language_Var2 = Medium_only_merged_for_shared_language) 


#matrix for rdist
matrix <- polygons %>% 
  dplyr::select(Longitude, Latitude) %>% 
  as.matrix()

#making spatial dists
dist <- fields::rdist.earth(x1 = matrix, 
                            x2 = matrix, miles = FALSE)

colnames(dist) <- polygons$Unique_ID
rownames(dist) <- polygons$Unique_ID

#melting
dist_long <- dist %>% 
  reshape2::melt() %>% 
  filter(Var1 != Var2) %>% 
  left_join(left,  by = "Var1") %>% 
  left_join(right,  by = "Var2")

#sbzr_group 
dist_SBZR <- dist_long %>% 
  filter(SBZR_group_Var1 != SBZR_group_Var2) %>% 
  group_by(SBZR_group_Var1, SBZR_group_Var2) %>% 
  summarise(min_dist = min(value), .groups = "drop")

#medium
dist_medium <- dist_long %>% 
  filter(Medium_only_merged_for_shared_language_Var1 != Medium_only_merged_for_shared_language_Var2) %>%   group_by(Medium_only_merged_for_shared_language_Var1, Medium_only_merged_for_shared_language_Var2) %>%   summarise(min_dist = min(value), .groups = "drop")


#make vcv
kappa_vec = c(2, 2, 2.5)
sigma_vec =  list(c(1, 1.15), c(1, 2), c(1, 3))

dist_medium[upper.tri(dist_medium, diag = TRUE)] <- NA
dist_SBZR[upper.tri(dist_SBZR, diag = TRUE)] <- NA

dists_vector_medium <- as.vector(dist_medium) %>% na.omit()
dists_vector_SBZR <- as.vector(dist_SBZR) %>% na.omit()

spatial_covar_mat = varcov.spatial(dists.lowertri = dists_vector,
                                   cov.pars = sigma,
                                   kappa = kappa)$varcov


## Repeat the typical variance standardisation from above
typical_variance_spatial = exp(mean(log(diag(spatial_covar_mat))))
spatial_cov_std = spatial_covar_mat / typical_variance_spatial
spatial_prec_mat = solve(spatial_cov_std)
dimnames(spatial_prec_mat) = list(locations_df$Language_ID, locations_df$Language_ID)


