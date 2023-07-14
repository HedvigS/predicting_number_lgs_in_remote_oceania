source("01_requirements.R")

##combining variables that are proxies for "carrying capacity" and different kinds of area-variables by finding the optimal number of pcs with a nscree test.

source("fun_def_find_pcs.R")

OUTPUTDIR <- "output/processed_data/pca"
if(!dir.exists(OUTPUTDIR)){dir.create(OUTPUTDIR)}

###############################################################################
###############################################################################

#Marck + carrying capacity parameters
data_fn_marck <- "output/processed_data/RO_Hedvig_aggregate_marck_group_scaled.tsv"

data <- read_tsv(file = data_fn_marck, show_col_types = F)  %>% 
  column_to_rownames("Marck_group") %>% 
  dplyr::select(Annual_precipitation_mean, 
                Precipitation_seasonality_mean, 
                Annual_temperature_mean, 
                Temperature_seasonality_mean, 
                Latitude_abs_mean,
                NPP_terra_mean,
#                NPP_terra_var,
                NPP_aqua_mean 
 #               NPP_aqua_var
) %>% 
  mutate_all(as.numeric) 

found_pcs <- find_pcs(data = data)

writeLines(
  found_pcs$nScree_summary_string,
  con = file.path(OUTPUTDIR, "PCA_nScree_summary_carrying_marck.txt")
)

carrying_capactiy_PCA_marck_df <- found_pcs$data_pca_df[,1:found_pcs$maxcol]  %>% #because the first col is the group we need to do plus one
    rename(Carrying_capactiy_PC1 = "PC1",
           Carrying_capactiy_PC2 = "PC2",
           Marck_group = ID)

carrying_capactiy_PCA_marck_df$Carrying_capactiy_PC1 <- modEvA::range01(carrying_capactiy_PCA_marck_df$Carrying_capactiy_PC1) +1
carrying_capactiy_PCA_marck_df$Carrying_capactiy_PC2 <- modEvA::range01(carrying_capactiy_PCA_marck_df$Carrying_capactiy_PC2) +1

read_tsv(data_fn_marck, show_col_types = F) %>% 
  full_join(carrying_capactiy_PCA_marck_df, by = "Marck_group") %>% 
  write_tsv(data_fn_marck)




PCA_prop_variance_df <- found_pcs$tidied_pca %>%
  dplyr::distinct(PC, Contribution, Parameter_ID)

# set up our plotting theme - starts with theme_classic and then modify some parts
theme_grambank_pca <- function(base_size = 12, base_family = "") {
  theme_classic(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.ticks.x = element_blank()
    )
}



PCA_prop_variance_df  %>%
  filter(PC == "PC1") %>%
  mutate(Parameter_ID= reorder(Parameter_ID, desc(Contribution))) %>%
  ggplot(aes(Parameter_ID, Contribution, fill = Parameter_ID)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  theme_grambank_pca() 







###############################################################################
###############################################################################
### medium + carrying capacity

data_fn_medium <- "output/processed_data/RO_Hedvig_aggregate_medium_group_scaled.tsv"

data <- read_tsv(file = data_fn_medium, show_col_types = F)  %>% 
  column_to_rownames("Medium_only_merged_for_shared_language") %>% 
  dplyr::select(Annual_precipitation_mean, 
                Precipitation_seasonality_mean, 
                Annual_temperature_mean, 
                Temperature_seasonality_mean, 
                Latitude_abs_mean,
                NPP_terra_mean,
#                NPP_terra_var,
                NPP_aqua_mean 
 #               NPP_aqua_var
) %>% 
  mutate_all(as.numeric) 

found_pcs <- find_pcs(data = data)

writeLines(
  found_pcs$nScree_summary_string,
  con = file.path(OUTPUTDIR, "PCA_nScree_summary_carrying_medium.txt")
)

carrying_capactiy_PCA_medium_df <- found_pcs$data_pca_df[,1:found_pcs$maxcol]  %>% #because the first col is the group we need to do plus one
  rename(Carrying_capactiy_PC1 = "PC1",
         Carrying_capactiy_PC2 = "PC2",
         Carrying_capactiy_PC3 = "PC3",
         Medium_only_merged_for_shared_language = ID)

carrying_capactiy_PCA_medium_df$Carrying_capactiy_PC1 <- modEvA::range01(carrying_capactiy_PCA_medium_df$Carrying_capactiy_PC1) +1
carrying_capactiy_PCA_medium_df$Carrying_capactiy_PC2 <- modEvA::range01(carrying_capactiy_PCA_medium_df$Carrying_capactiy_PC2) +1
carrying_capactiy_PCA_medium_df$Carrying_capactiy_PC3 <- modEvA::range01(carrying_capactiy_PCA_medium_df$Carrying_capactiy_PC3) +1

read_tsv(data_fn_medium, show_col_types = F) %>% 
  full_join(carrying_capactiy_PCA_medium_df, by = "Medium_only_merged_for_shared_language") %>% 
  write_tsv(data_fn_medium)

###############################################################################
###############################################################################