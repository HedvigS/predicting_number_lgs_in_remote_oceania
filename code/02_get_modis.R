source("01_requirements.R")

polygons <- read_csv("data/RO_polygons_grouped_with_languages.csv") %>% 
  dplyr::select(Unique_ID, Marck_group, Medium_only_merged_for_shared_language) 

fns <- list.files("data/modis/output_from_modis/", pattern = "MOD.*.csv")

fns <- list.files("data/modis/output_from_modis/", pattern = ".csv", recursive = F, full.names = T)

all_modis <- fns %>%  map_df(
  function(x) data.table::fread(x ,
                                encoding = 'UTF-8', header = TRUE, 
                                fill = TRUE, blank.lines.skip = TRUE,
                                sep = ",", na.strings = "",
  )   %>% 
    mutate(across(everything(), as.character)) %>% 
    mutate(filename = basename(x)))


modis_with_groups <- all_modis %>% 
 mutate(terra_aqua = ifelse(str_detect(filename, "MOD"), "terra", "aqua")) %>% 
  dplyr::select(Unique_ID = ID, MOD17A3HGF_061_Npp_500m, MYD17A3HGF_061_Npp_500m) %>% 
    mutate(MOD17A3HGF_061_Npp_500m = as.numeric(MOD17A3HGF_061_Npp_500m)) %>% 
    mutate(MYD17A3HGF_061_Npp_500m = as.numeric(MYD17A3HGF_061_Npp_500m)) %>% 
  left_join(polygons, by = "Unique_ID") 

modis_with_groups %>% 
  filter(!is.na(Marck_group)) %>% 
  group_by(Marck_group) %>% 
summarise(mean_MOD17A3HGF_061_Npp_500m = mean(MOD17A3HGF_061_Npp_500m, na.rm = T),
            var_MOD17A3HGF_061_Npp_500m = var(MOD17A3HGF_061_Npp_500m, na.rm = T),
            mean_MYD17A3HGF_061_Npp_500m = mean(MYD17A3HGF_061_Npp_500m, na.rm = T), 
            var_MYD17A3HGF_061_Npp_500m = var(MYD17A3HGF_061_Npp_500m, na.rm = T)) %>% 
  write_tsv("output/processed_data/modis_marck.tsv", na = "")

modis_with_groups %>% 
  filter(!is.na(Medium_only_merged_for_shared_language)) %>% 
  group_by(Medium_only_merged_for_shared_language) %>% 
  summarise(mean_MOD17A3HGF_061_Npp_500m = mean(MOD17A3HGF_061_Npp_500m, na.rm = T),
            var_MOD17A3HGF_061_Npp_500m = var(MOD17A3HGF_061_Npp_500m, na.rm = T),
            mean_MYD17A3HGF_061_Npp_500m = mean(MYD17A3HGF_061_Npp_500m, na.rm = T), 
            var_MYD17A3HGF_061_Npp_500m = var(MYD17A3HGF_061_Npp_500m, na.rm = T)) %>% 
  write_tsv("output/processed_data/modis_Medium_only_merged_for_shared_language.tsv", na = "")

