source("01_requirements.R")

Glottolog <- read_tsv("output/processed_data/glottolog_language_table_wide_df.tsv", show_col_types = F) %>%   
  dplyr::select(glottocode = Glottocode, Language_level_ID, level)

Polygon_lgs_glottocodes_unnested <- read_csv("data/RO_polygons_grouped_with_languages.csv", show_col_types = F) %>% 
  filter(!is.na(glottocodes)) %>% 
  mutate(glottocodes = str_split(glottocodes, ",")) %>%
  unnest(glottocodes) %>% 
  mutate(glottocode = trimws(glottocodes)) %>% 
  distinct() 

Polygon_lgs_count_smallest <- Polygon_lgs_glottocodes_unnested %>% 
  distinct(Smallest_Island_group, glottocode) %>% 
  group_by(Smallest_Island_group) %>% 
  summarise(lg_count_smallest = n()) 

Polygon_lgs_count_medium <- Polygon_lgs_glottocodes_unnested %>% 
  distinct(Medium_only_merged_for_shared_language, glottocode) %>% 
  group_by(Medium_only_merged_for_shared_language) %>%  
  summarise(lg_count_medium = n()) 

Polygon_lgs_count_Marck <- Polygon_lgs_glottocodes_unnested %>%   
  distinct(Marck_group, glottocode) %>% 
  group_by(Marck_group) %>% 
  summarise(lg_count_Marck = n())

#reading in area per smallest island group
water_areas <- read_tsv("data/Water_area.tsv", show_col_types = F)

isolation_marck <- read_tsv("output/processed_data/isolation_RO_geo_dist_isolation_marck_group.tsv", show_col_types = F) %>% 
  dplyr::select(Marck_group = Marck_group_left, dist) 

isolation_medium <- read_tsv("output/processed_data/isolation_RO_geo_dist_isolation_medium_island.tsv", show_col_types = F) %>%
  dplyr::select(Medium_only_merged_for_shared_language = Medium_only_merged_for_shared_language_left, dist)

polygon_geo <- read_csv("data/RO_polygons_grouped_with_languages.csv", na = c("NA", ""),show_col_types = F) %>% 
  filter(!is.na(glottocodes)) %>% 
  mutate(Unique_ID = as.character(Unique_ID)) %>% 
  full_join(water_areas, by = c("Unique_ID", "Smallest_Island_group"))  %>% 
  distinct(Smallest_Island_group, Unique_ID, .keep_all = T) %>% 
  group_by(Smallest_Island_group) %>% 
  summarise(sum_area = sum(`AREA (sq km)`), 
            sum_shoreline = sum(`COASTLINE (km) (perimeter)`), 
            mean_lat = mean(Latitude), 
            mean_long = mean(Longitude), 
            sum_water_area = sum(water_area), 
            Melanesia_or_not = dplyr::first(Melanesia_or_not), 
            max_long = max(Longitude), 
            min_long = min(Longitude), 
            max_lat = max(Latitude), 
            min_lat = min(Latitude))

polygon_geo_grouping_hierarchy <- read_csv("data/RO_polygons_grouped_with_languages.csv", show_col_types = F) %>% 
  filter(!is.na(glottocodes)) %>% 
  distinct(Marck_group, Medium_only_merged_for_shared_language, Smallest_Island_group, glottocodes) %>% 
  full_join(Polygon_lgs_count_medium, by = "Medium_only_merged_for_shared_language") %>% 
  full_join(Polygon_lgs_count_Marck, by = "Marck_group") %>% 
  full_join(Polygon_lgs_count_smallest, by = "Smallest_Island_group") %>% 
  full_join(polygon_geo, by = "Smallest_Island_group")

#distinct colors
n <- length(unique(polygon_geo_grouping_hierarchy$Smallest_Island_group))
color_vector <- sample(distinctColorPalette(n), size = n)
polygon_geo_grouping_hierarchy$smallest_island_color <- color_vector[as.factor(polygon_geo_grouping_hierarchy$Smallest_Island_group)]

n <- length(unique(polygon_geo_grouping_hierarchy$Marck_group))
color_vector <- sample(distinctColorPalette(n), size = n)
polygon_geo_grouping_hierarchy$Marck_group_color <- color_vector[as.factor(polygon_geo_grouping_hierarchy$Marck_group)]

n <- length(unique(polygon_geo_grouping_hierarchy$Medium_only_merged_for_shared_language))
color_vector <- sample(distinctColorPalette(n), size = n)
polygon_geo_grouping_hierarchy$medium_group_color <- color_vector[as.factor(polygon_geo_grouping_hierarchy$Medium_only_merged_for_shared_language)]

write_tsv(polygon_geo_grouping_hierarchy, "output/processed_data/Polygon_hierarchy_stats.tsv")

subregions <- read_tsv("data/oceania_subregions.tsv", show_col_types = F) %>% 
  filter(Near_Remote_Oceania == "Remote Oceania") %>% 
  filter(level == "language") %>% 
  filter(Family_name == "Austronesian") %>%
  filter(Glottocode != "Ngaapuhi") %>% 
  filter(Glottocode != "Ngaati Kohungunu") %>% 
  filter(Glottocode != "Ngaati Porou") %>% 
  filter(Glottocode != "sout2867") %>% 
  filter(Glottocode != "tara1315") %>% 
  filter(Glottocode != "Te Ao Puri") %>% 
  filter(Glottocode != "Tuuhoe") %>% 
  filter(Glottocode != "Tuuwharetoa") %>% 
  filter(Glottocode != "Waikato") %>% 
  filter(Glottocode != "fiji1243") %>% 
  mutate(Smallest_Island_group = str_split(Smallest_Island_group, ",")) %>%
  unnest(Smallest_Island_group) %>% 
  mutate(Smallest_Island_group = trimws(Smallest_Island_group)) %>% 
  dplyr::rename(Glottocode_spec = Glottocode) %>% 
  dplyr::rename(Glottocode = Glottocode_language_level) %>% 
  distinct(Glottocode_spec, Glottocode, Smallest_Island_group, Smallest_Island_group_main) %>% 
  full_join(polygon_geo_grouping_hierarchy, by = "Smallest_Island_group")

pol_complex <- readODS::read_ods("data/Remote_oceania_pol_complex_hedvig_code_latex.ods", sheet = 1) %>%
  dplyr::select(Language_level_ID = glottocode, `Political complexity (EA033)`) %>% 
  mutate(glottocode = ifelse(Language_level_ID == "fiji1243", "fiji1243,kada1285,sout2864,nort2843", Language_level_ID)) %>% 
  mutate(glottocode = ifelse(Language_level_ID == "aust1304",  "aust1304,raiv1237,tubu1240,ruru123", glottocode)) %>% 
  mutate(glottocode = ifelse(Language_level_ID == "maor1246", "maor1246,mori1267", glottocode)) %>% 
  mutate(glottocode = str_split(glottocode, ",")) %>% 
  unnest(cols = c(glottocode)) %>% 
  dplyr::select(Glottocode = glottocode, pol_complex_code_Hedvig = "Political complexity (EA033)" ) %>% 
  left_join(subregions, by = "Glottocode") %>% 
  filter(!is.na(Smallest_Island_group)) %>% 
  distinct(Smallest_Island_group, pol_complex_code_Hedvig) %>% 
  filter(!is.na(pol_complex_code_Hedvig))

EA032 <- read_csv("data/EA032.csv", show_col_types = F) %>% 
  dplyr::select(glottocode = language_glottocode, EA032_code = code) %>% 
  left_join(Glottolog, by = "glottocode") %>% 
  dplyr::select(-glottocode) %>% 
  dplyr::rename(Glottocode = Language_level_ID) %>% 
  inner_join(subregions, by = "Glottocode") %>% 
  dplyr::select(-Smallest_Island_group) %>% 
  mutate(Smallest_Island_group = Smallest_Island_group_main)  %>% 
   mutate(Smallest_Island_group = str_split(Smallest_Island_group, ",")) %>%
   unnest(Smallest_Island_group) %>% 
  mutate(Smallest_Island_group = trimws(Smallest_Island_group)) %>% 
  group_by(Smallest_Island_group) %>% 
  summarise(EA032_code = mean(EA032_code))

Dplace_npp <- read_csv("data/dplace_NPP.csv", show_col_types = F) %>% 
  dplyr::select(glottocode = language_glottocode, NPP = code) %>% 
  left_join(Glottolog, by = "glottocode") %>% 
  dplyr::select(-glottocode) %>% 
  dplyr::rename(Glottocode = Language_level_ID) %>% 
  inner_join(subregions, by = "Glottocode") %>% 
  dplyr::select(-Smallest_Island_group) %>% 
  mutate(Smallest_Island_group = Smallest_Island_group_main)  %>% 
  mutate(Smallest_Island_group = str_split(Smallest_Island_group, ",")) %>%
  unnest(Smallest_Island_group) %>% 
  mutate(Smallest_Island_group = trimws(Smallest_Island_group)) %>% 
  group_by(Smallest_Island_group) %>% 
  summarise(NPP = mean(NPP))

NetPrimaryProductionPredictability <- read_csv("data/NetPrimaryProductionPredictability.csv", show_col_types = F) %>% 
  dplyr::select(glottocode = language_glottocode, Predictability_code = code) %>% 
  left_join(Glottolog, by = "glottocode") %>% 
  dplyr::select(-glottocode) %>% 
  dplyr::rename(Glottocode = Language_level_ID) %>% 
  inner_join(subregions, by = "Glottocode") %>%
  dplyr::select(-Smallest_Island_group) %>% 
  mutate(Smallest_Island_group = Smallest_Island_group_main)  %>% 
  mutate(Smallest_Island_group = str_split(Smallest_Island_group, ",")) %>%
  unnest(Smallest_Island_group) %>% 
  mutate(Smallest_Island_group = trimws(Smallest_Island_group)) %>% 
  group_by(Smallest_Island_group) %>% 
  summarise(Predictability_code = mean(Predictability_code))

ecoClimate_data <- read_tsv("output/processed_data/RO_polygons_grouped_with_languages_with_climate_grouped.tsv", show_col_types = F)

##dates
dates <- read_tsv("data/island_group_settlement_date.tsv", show_col_types = F) %>% 
  rename(settlement_date_grouping_finer = "Time depth settlement group", Smallest_Island_group = `Smaller specific island group`, `Settlement date oldest date` = `Oldest date`) %>% 
  mutate(Smallest_Island_group = str_split(Smallest_Island_group, ",")) %>%
  unnest(Smallest_Island_group) %>% 
  mutate(Smallest_Island_group = trimws(Smallest_Island_group)) %>% 
  group_by(Smallest_Island_group) %>% 
  summarise(oldest_date = max(`Settlement date oldest date`), 
            settlement_date_grouping_finer = min(settlement_date_grouping_finer, na.rm = T), settlement_date_grouping_coarser = min(settlement_date_grouping_coarser, na.rm = T)) 

##All
Island_group_all_sep <- polygon_geo_grouping_hierarchy %>% 
  full_join(polygon_geo, by = c("Smallest_Island_group", "sum_area", "sum_shoreline", "mean_lat", "mean_long",
                                "sum_water_area", "Melanesia_or_not", "max_long", "min_long", "max_lat", "min_lat")) %>% 
  full_join(dates, by = "Smallest_Island_group") %>% 
  full_join(Dplace_npp, by = "Smallest_Island_group") %>%
  full_join(pol_complex, by = "Smallest_Island_group")  %>% 
  full_join(NetPrimaryProductionPredictability, by = "Smallest_Island_group") %>% 
  full_join(ecoClimate_data, by = "Smallest_Island_group") %>% 
  full_join(EA032, by = "Smallest_Island_group") %>% 
  distinct()   

write_tsv(Island_group_all_sep, "output/processed_data/RO_Hedvig_aggregate_all_obs_sep.tsv")

#Aggregation over smallest unit
Island_group_summarised_smallest <- Island_group_all_sep %>% 
  filter(!is.na(settlement_date_grouping_finer)) %>% 
  group_by(Smallest_Island_group) %>% 
  dplyr::summarise(mean_NetPrimaryProductionPredictability = mean(Predictability_code, na.rm = T), 
            max_NetPrimaryProductionPredictability = max(Predictability_code),
            min_NetPrimaryProductionPredictability = min(Predictability_code),
            mean_NPP = mean(NPP, na.rm = T),  
            mean_pol_complex = mean(pol_complex_code_Hedvig, na.rm = T), 
            mean_EA032 = mean(EA032_code, na.rm = T),
            sum_area = sum(sum_area, na.rm = T), 
            sum_water_area = sum(sum_water_area, na.rm = T), 
            Melanesia_or_not = dplyr::first(Melanesia_or_not),
            lg_count_smallest = mean(lg_count_smallest, na.rm = T),
            sum_shoreline = sum(sum_shoreline, na.rm = T),
            mean_lat = mean(mean_lat),
            mean_long = mean(mean_long),
            settlement_date_grouping_finer = min(settlement_date_grouping_finer), 
            settlement_date_grouping_coarser = min(settlement_date_grouping_coarser),
            oldest_date = max(oldest_date),
            color = dplyr::first(smallest_island_color), 
            lg_count = dplyr::first(lg_count_smallest),
            mean_CCSM_piControl_1760_bio1 = mean(mean_CCSM_piControl_1760_bio1),
            mean_CCSM_piControl_1760_bio4 = mean(mean_CCSM_piControl_1760_bio4),
            mean_CCSM_piControl_1760_bio12 = mean(mean_CCSM_piControl_1760_bio12),
            mean_CCSM_piControl_1760_bio15 = mean(mean_CCSM_piControl_1760_bio15)
            ) %>% 
  mutate(ratio_coastline_to_area = sum_shoreline / sum_area)

write_tsv(Island_group_summarised_smallest, "output/processed_data/RO_Hedvig_aggregate_smallest_island.tsv")

Island_group_summarised_smallest$Smallest_Island_group <- fct_reorder(Island_group_summarised_smallest$Smallest_Island_group, Island_group_summarised_smallest$lg_count)

Island_group_summarised_smallest %>% 
  ggplot() +
  geom_bar(aes(x = Smallest_Island_group, y = lg_count, fill = lg_count), stat = "Identity") +
  theme_classic() +
  theme(axis.text.x = element_blank(), legend.position = "None")

ggsave("output/plots/Lg_distrubition_smallest_island_group.png", width = 20, height = 5)


#medium_group
Island_group_summarised_medium <- Island_group_all_sep %>%
  filter(!is.na(settlement_date_grouping_finer)) %>% 
  group_by(Medium_only_merged_for_shared_language) %>% 
  summarise(mean_NetPrimaryProductionPredictability = mean(Predictability_code, na.rm = T), 
            max_NetPrimaryProductionPredictability = max(Predictability_code),
            min_NetPrimaryProductionPredictability = min(Predictability_code),
            mean_NPP = mean(NPP, na.rm = T),  
            mean_pol_complex = mean(pol_complex_code_Hedvig, na.rm = T), 
            mean_EA032 = mean(EA032_code, na.rm = T),
            sum_area = sum(sum_area, na.rm = T), 
            sum_water_area = sum(sum_water_area, na.rm = T),
            Melanesia_or_not = dplyr::first(Melanesia_or_not),
            sum_shoreline = sum(sum_shoreline, na.rm = T),
            mean_lat = mean(mean_lat),
            mean_long = mean(mean_long),
            settlement_date_grouping_finer = min(settlement_date_grouping_finer), 
            settlement_date_grouping_coarser = min(settlement_date_grouping_coarser),
            oldest_date = max(oldest_date),
            color = dplyr::first(medium_group_color), 
            lg_count = dplyr::first(lg_count_medium),
            mean_CCSM_piControl_1760_bio1 = mean(mean_CCSM_piControl_1760_bio1),
            mean_CCSM_piControl_1760_bio4 = mean(mean_CCSM_piControl_1760_bio4),
            mean_CCSM_piControl_1760_bio12 = mean(mean_CCSM_piControl_1760_bio12),
            mean_CCSM_piControl_1760_bio15 = mean(mean_CCSM_piControl_1760_bio15)
            ) %>% 
  left_join(isolation_medium, by = "Medium_only_merged_for_shared_language") %>% 
  mutate(ratio_coastline_to_area = sum_shoreline / sum_area)

write_tsv(Island_group_summarised_medium, "output/processed_data/RO_Hedvig_aggregate_medium_island.tsv")

Island_group_summarised_medium$Medium_only_merged_for_shared_language <- fct_reorder(Island_group_summarised_medium$Medium_only_merged_for_shared_language, Island_group_summarised_medium$lg_count)

Island_group_summarised_medium%>% 
  rename(`Language count` = lg_count) %>% 
  rename(`Island group (merged for language)` = Medium_only_merged_for_shared_language) %>% 
  ggplot() +
  geom_bar(aes(x = `Island group (merged for language)`, y = `Language count`), stat = "Identity", fill = Island_group_summarised_medium$color) +
  theme_classic() +
  theme(axis.text.x = element_blank(), legend.position = "None")

ggsave("output/plots/Lg_distrubition_medium_island_group_lg_merged.png", width = 10, height = 5)

#Marck
Island_group_summarised_Marck_group <- Island_group_all_sep %>% 
  filter(!is.na(settlement_date_grouping_finer)) %>% 
  group_by(Marck_group) %>% 
  summarise(mean_NetPrimaryProductionPredictability = mean(Predictability_code, na.rm = T), 
            max_NetPrimaryProductionPredictability = max(Predictability_code),
            min_NetPrimaryProductionPredictability = min(Predictability_code),
            mean_NPP = mean(NPP, na.rm = T),  
            mean_pol_complex = mean(pol_complex_code_Hedvig, na.rm = T), 
            mean_EA032 = mean(EA032_code, na.rm = T),
            sum_area = sum(sum_area, na.rm = T), 
            sum_water_area = sum(sum_water_area, na.rm = T), 
            Melanesia_or_not = dplyr::first(Melanesia_or_not),
            lg_count_smallest = mean(lg_count_smallest, na.rm = T),
            sum_shoreline = sum(sum_shoreline, na.rm = T),
            mean_lat = mean(mean_lat),
            mean_long = mean(mean_long),
            settlement_date_grouping_finer = min(settlement_date_grouping_finer),
            settlement_date_grouping_coarser = min(settlement_date_grouping_coarser),
            oldest_date = max(oldest_date),
            color = dplyr::first(Marck_group_color), 
            lg_count = dplyr::first(lg_count_Marck),
            mean_CCSM_piControl_1760_bio1 = mean(mean_CCSM_piControl_1760_bio1),
            mean_CCSM_piControl_1760_bio4 = mean(mean_CCSM_piControl_1760_bio4),
            mean_CCSM_piControl_1760_bio12 = mean(mean_CCSM_piControl_1760_bio12),
            mean_CCSM_piControl_1760_bio15 = mean(mean_CCSM_piControl_1760_bio15)
            ) %>% 
  left_join(isolation_marck, by = "Marck_group") %>% 
  mutate(ratio_coastline_to_area = sum_shoreline / sum_area)

write_tsv(Island_group_summarised_Marck_group, "output/processed_data/RO_Hedvig_aggregate_marck_group.tsv")

Island_group_summarised_Marck_group$Marck_group <- fct_reorder(Island_group_summarised_Marck_group$Marck_group, Island_group_summarised_Marck_group$lg_count)

Island_group_summarised_Marck_group %>% 
  rename(`Language count` = lg_count) %>% 
  rename(`Marck group` = Marck_group) %>% 
  ggplot() +
  geom_bar(aes(x = `Marck group`, y = `Language count`), stat = "Identity", fill = Island_group_summarised_Marck_group$color) +
  theme_classic() +
  theme(axis.text.x = element_blank(), legend.position = "None")
#  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = "None")

ggsave("output/plots/Lg_distrubition_Marck_group.png", width = 10, height = 5)