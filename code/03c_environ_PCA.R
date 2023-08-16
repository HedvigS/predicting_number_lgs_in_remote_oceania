source("01_requirements.R")

##combining variables that are proxies for "carrying capacity" and different kinds of area-variables by finding the optimal number of pcs with a nscree test.

source("fun_def_find_pcs.R")

OUTPUTDIR <- "output/processed_data/pca"
if(!dir.exists(OUTPUTDIR)){dir.create(OUTPUTDIR)}

###############################################################################
###############################################################################

#SBZR + carrying capacity parameters
data_fn_SBZR <- "output/processed_data/RO_Hedvig_aggregate_SBZR_group_scaled.tsv"

data <- read_tsv(file = data_fn_SBZR, show_col_types = F)  %>% 
  column_to_rownames("SBZR_group") %>% 
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
  con = file.path(OUTPUTDIR, "PCA_nScree_summary_carrying_SBZR.txt")
)

environ_PCA_SBZR_df <- found_pcs$data_pca_df[,1:found_pcs$maxcol]  %>% #because the first col is the group we need to do plus one
    rename(environ_PC1 = "PC1",
           environ_PC2 = "PC2",
           SBZR_group = ID)

environ_PCA_SBZR_df$environ_PC1 <- scale(environ_PCA_SBZR_df$environ_PC1)[,1]
environ_PCA_SBZR_df$environ_PC2 <- scale(environ_PCA_SBZR_df$environ_PC2)[,1]

environ_PCA_SBZR_df <- data %>% 
  rownames_to_column("SBZR_group") %>% 
  full_join(environ_PCA_SBZR_df, by = "SBZR_group") %>% 
  dplyr::select(SBZR_group, NPP_terra_mean, environ_PC1, environ_PC2)

environ_PCA_SBZR_df <- environ_PCA_SBZR_df %>% 
  dplyr::select(-NPP_terra_mean)

read_tsv(data_fn_SBZR, show_col_types = F) %>% 
  full_join(environ_PCA_SBZR_df, by = "SBZR_group") %>%
  write_tsv(data_fn_SBZR)

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

ggsave("output/plots/PCA_contributions_SBZR_PC1.png", width = 12, height = 10)
ggsave("../latex/PCA_contributions_SBZR_PC1.png", width = 12, height = 10)

PCA_prop_variance_df  %>%
  filter(PC == "PC2") %>%
  mutate(Parameter_ID= reorder(Parameter_ID, desc(Contribution))) %>%
  ggplot(aes(Parameter_ID, Contribution, fill = Parameter_ID)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  theme_grambank_pca() 

ggsave("output/plots/PCA_contributions_SBZR_PC2.png", width = 12, height = 10)

ggsave("../latex/PCA_contributions_SBZR_PC2.png", width = 12, height = 10)




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

environ_PCA_medium_df <- found_pcs$data_pca_df[,1:found_pcs$maxcol]  %>% #because the first col is the group we need to do plus one
  rename(environ_PC1 = "PC1",
         environ_PC2 = "PC2",
         environ_PC3 = "PC3",
         Medium_only_merged_for_shared_language = ID)

environ_PCA_medium_df$environ_PC1 <- scale(environ_PCA_medium_df$environ_PC1)[,1]
environ_PCA_medium_df$environ_PC2 <- scale(environ_PCA_medium_df$environ_PC2)[,1]
environ_PCA_medium_df$environ_PC3 <- scale(environ_PCA_medium_df$environ_PC3)[,1]

#flipping if need be

environ_PCA_medium_df <- data %>% 
  rownames_to_column("Medium_only_merged_for_shared_language") %>% 
  full_join(environ_PCA_medium_df, by = "Medium_only_merged_for_shared_language") %>% 
  dplyr::select(Medium_only_merged_for_shared_language, NPP_terra_mean, environ_PC1, environ_PC2, environ_PC3)

environ_PCA_medium_df <- environ_PCA_medium_df %>% 
  dplyr::select(-NPP_terra_mean)

read_tsv(data_fn_medium, show_col_types = F) %>% 
  full_join(environ_PCA_medium_df, by = join_by(Medium_only_merged_for_shared_language)) %>%
  write_tsv(data_fn_medium)

PCA_prop_variance_df <- found_pcs$tidied_pca %>%
  dplyr::distinct(PC, Contribution, Parameter_ID)

PCA_prop_variance_df  %>%
  filter(PC == "PC1") %>%
  mutate(Parameter_ID= reorder(Parameter_ID, desc(Contribution))) %>%
  ggplot(aes(Parameter_ID, Contribution, fill = Parameter_ID)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  theme_grambank_pca() 

ggsave("output/plots/PCA_contributions_medium_PC1.png", width = 12, height = 10)
ggsave("../latex/PCA_contributions_medium_PC1.png", width = 12, height = 10)

PCA_prop_variance_df  %>%
  filter(PC == "PC2") %>%
  mutate(Parameter_ID= reorder(Parameter_ID, desc(Contribution))) %>%
  ggplot(aes(Parameter_ID, Contribution, fill = Parameter_ID)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  theme_grambank_pca() 

ggsave("output/plots/PCA_contributions_medium_PC2.png", width = 12, height = 10)

ggsave("../latex/PCA_contributions_medium_PC2.png", width = 12, height = 10)



PCA_prop_variance_df  %>%
  filter(PC == "PC3") %>%
  mutate(Parameter_ID= reorder(Parameter_ID, desc(Contribution))) %>%
  ggplot(aes(Parameter_ID, Contribution, fill = Parameter_ID)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  theme_grambank_pca() 

ggsave("output/plots/PCA_contributions_medium_PC3.png", width = 12, height = 10)

ggsave("../latex/PCA_contributions_medium_PC3.png", width = 12, height = 10)




###############################################################################
###############################################################################




#COUNTRY
data_fn_country <- "output/processed_data/RO_Hedvig_aggregate_country_group_scaled.tsv"

data <- read_tsv(file = data_fn_country, show_col_types = F)  %>% 
  column_to_rownames("COUNTRY NAME") %>% 
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
  con = file.path(OUTPUTDIR, "PCA_nScree_summary_carrying_country.txt")
)

environ_PCA_country_df <- found_pcs$data_pca_df[,1:found_pcs$maxcol]  %>% #because the first col is the group we need to do plus one
  rename(environ_PC1 = "PC1",
         environ_PC2 = "PC2",
         `COUNTRY NAME` = ID)

environ_PCA_country_df$environ_PC1 <- scale(environ_PCA_country_df$environ_PC1)[,1]
environ_PCA_country_df$environ_PC2 <- scale(environ_PCA_country_df$environ_PC2)[,1]

read_tsv(data_fn_country, show_col_types = F) %>% 
  full_join(environ_PCA_country_df, by = "COUNTRY NAME") %>% 
  write_tsv(data_fn_country)
