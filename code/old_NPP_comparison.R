#NPP comparison



modis_mean_NPP_terra <- read_tsv("output/processed_data/modis_with_groups.tsv", col_types = "c") %>%
  mutate(MOD17A3HGF_061_Npp_500m = as.numeric(MOD17A3HGF_061_Npp_500m)) %>% 
  mutate(MYD17A3HGF_061_Npp_500m = as.numeric(MYD17A3HGF_061_Npp_500m)) %>%
    group_by(Smallest_Island_group) %>% 
    summarise(NPP_modis = mean(MOD17A3HGF_061_Npp_500m, na.rm = T))

Dplace_npp <- read_csv("data/dplace_NPP.csv", show_col_types = F) %>% 
  dplyr::select(glottocode = language_glottocode, NPP = code) %>% 
  left_join(Glottolog, by = "glottocode", relationship = "many-to-many") %>% 
  dplyr::select(-glottocode) %>% 
  dplyr::rename(Glottocode = Language_level_ID) %>% 
  inner_join(subregions, by = "Glottocode", relationship = "many-to-many") %>% 
  dplyr::select(-Smallest_Island_group) %>% 
  mutate(Smallest_Island_group = Smallest_Island_group_main)  %>% 
  mutate(Smallest_Island_group = str_split(Smallest_Island_group, ",")) %>%
  unnest(Smallest_Island_group) %>% 
  mutate(Smallest_Island_group = trimws(Smallest_Island_group)) %>% 
  group_by(Smallest_Island_group) %>% 
  summarise(NPP_dplace = mean(NPP))

joined <- full_join(Dplace_npp, modis_mean_NPP_terra)

joined %>% 
  ggplot() +
  geom_point(aes(x = NPP_dplace, y = NPP_modis))
