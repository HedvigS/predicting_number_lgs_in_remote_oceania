source("01_requirements.R")

#this scripts takes all relevant data and combines into approrpaite tables for the two island groupings.

#reading in glottolog for language-levelling
Glottolog <- read_tsv("output/processed_data/glottolog_language_table_wide_df.tsv", show_col_types = F) %>%   
  dplyr::select(glottocode = Glottocode, Language_level_ID, level)

#wrangling polygon lists into language counts per island group
Polygon_lgs_glottocodes_unnested <- read_csv("data/RO_polygons_grouped_with_languages.csv", show_col_types = F) %>% 
  filter(!is.na(glottocodes)) %>% 
  filter(glottocodes != "")  %>% 
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

Polygon_lgs_count_SBZR <- Polygon_lgs_glottocodes_unnested %>%   
  distinct(SBZR_group, glottocode) %>% 
  group_by(SBZR_group) %>% 
  summarise(lg_count_SBZR = n())

Polygon_lgs_count_COUNTRY <- Polygon_lgs_glottocodes_unnested %>%   
  distinct(`COUNTRY NAME`, glottocode) %>% 
  group_by(`COUNTRY NAME`) %>% 
  summarise(lg_count_COUNTRY = n())

polygon_geo <- read_csv("data/RO_polygons_grouped_with_languages.csv", na = c("NA", ""),show_col_types = F) %>% 
  filter(!is.na(glottocodes)) %>% 
  mutate(Unique_ID = as.character(Unique_ID)) %>% 
  distinct(Smallest_Island_group, Unique_ID, .keep_all = T) %>% 
  group_by(Smallest_Island_group) %>% 
  summarise(sum_area = sum(`AREA (sq km)`), 
            sum_shoreline = sum(`COASTLINE (km) (perimeter)`), 
            mean_lat = mean(Latitude), 
            mean_long = mean(Longitude), 
            max_long = max(Longitude), 
            min_long = min(Longitude), 
            max_lat = max(Latitude), 
            min_lat = min(Latitude))

polygon_geo_grouping_hierarchy <- read_csv("data/RO_polygons_grouped_with_languages.csv", show_col_types = F) %>% 
  filter(!is.na(glottocodes)) %>% 
  distinct(Marck_group, Medium_only_merged_for_shared_language, `COUNTRY NAME`,SBZR_group, Smallest_Island_group, glottocodes) %>% 
  full_join(Polygon_lgs_count_medium, by = "Medium_only_merged_for_shared_language") %>% 
  full_join(Polygon_lgs_count_Marck, by = "Marck_group") %>% 
  full_join(Polygon_lgs_count_SBZR , by = "SBZR_group") %>% 
  full_join(Polygon_lgs_count_COUNTRY,  by = "COUNTRY NAME") %>% 
    full_join(Polygon_lgs_count_smallest, by = "Smallest_Island_group") %>% 
  full_join(polygon_geo, by = "Smallest_Island_group")

polygon_geo_grouping_hierarchy %>% write_tsv("output/processed_data/polygon_geo_grouping_hierarchy.tsv")

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

n <- length(unique(polygon_geo_grouping_hierarchy$SBZR_group))
color_vector <- sample(distinctColorPalette(n), size = n)
polygon_geo_grouping_hierarchy$SBZR_group_color <- color_vector[as.factor(polygon_geo_grouping_hierarchy$SBZR_group)]

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
  mutate(Glottocode = trimws(Glottocode)) %>% 
  mutate(Glottocode_spec = trimws(Glottocode_spec)) %>% 
  distinct(Glottocode_spec, Glottocode, Smallest_Island_group, Smallest_Island_group_main) %>% 
  full_join(polygon_geo_grouping_hierarchy, 
            by = "Smallest_Island_group",
            relationship = "many-to-many")

subregions %>%
  write_tsv("output/processed_data/subregions.tsv")

##POL COMPLEXITY
pol_complex <- readODS::read_ods("data/Remote_oceania_pol_complex_hedvig_code_latex.ods", sheet = 1) %>%
  dplyr::select(Language_level_ID = glottocode, `Political complexity (EA033)`) %>% 
  mutate(glottocode = ifelse(Language_level_ID == "fiji1243", "fiji1243,kada1285,sout2864,nort2843", Language_level_ID)) %>% 
  mutate(glottocode = ifelse(Language_level_ID == "aust1304",  "aust1304,raiv1237,tubu1240,ruru1237", glottocode)) %>% 
  mutate(glottocode = ifelse(Language_level_ID == "maor1246", "maor1246,mori1267", glottocode)) %>% 
  mutate(glottocode = str_split(glottocode, ",")) %>% 
  unnest(cols = c(glottocode)) %>% 
  dplyr::select(Glottocode = glottocode, pol_complex_code_Hedvig = "Political complexity (EA033)" ) %>% 
  left_join(subregions, by = "Glottocode") %>% 
  filter(!is.na(Smallest_Island_group)) %>% 
  distinct(Smallest_Island_group, pol_complex_code_Hedvig) %>% 
  filter(!is.na(pol_complex_code_Hedvig))

#NPP see 02_get_modis.R
modis_NPP <- read_tsv("output/processed_data/modis_with_groups.tsv", show_col_types = F, col_types = "c") %>% 
  mutate(MOD17A3HGF_061_Npp_500m = as.numeric(MOD17A3HGF_061_Npp_500m)) %>% 
  mutate(MYD17A3HGF_061_Npp_500m = as.numeric(MYD17A3HGF_061_Npp_500m)) %>% 
    dplyr::select(Smallest_Island_group, 
                MOD17A3HGF_061_Npp_500m_terra = MOD17A3HGF_061_Npp_500m, 
                MYD17A3HGF_061_Npp_500m_water = MYD17A3HGF_061_Npp_500m)
                  
#ecoclimate data see 02_ecoClimate_extract.R
ecoClimate_data <- read_tsv("output/processed_data/RO_polygons_grouped_with_languages_with_climate_grouped.tsv", show_col_types = F)

##dates
dates <- read_tsv("data/island_group_settlement_date.tsv", show_col_types = F) %>% 
  rename(settlement_date_grouping_finer = "Time depth settlement group", Smallest_Island_group = `Smaller specific island group`, `Settlement date oldest date` = `Oldest date`) %>% 
  mutate(Smallest_Island_group = str_split(Smallest_Island_group, ",")) %>%
  unnest(Smallest_Island_group) %>% 
  mutate(Smallest_Island_group = trimws(Smallest_Island_group)) %>% 
  group_by(Smallest_Island_group) %>% 
  summarise(oldest_date = max(`Settlement date oldest date`), 
            settlement_date_grouping_finer = min(settlement_date_grouping_finer, na.rm = T)) %>% 
  ungroup()  

max_group <- max(dates$settlement_date_grouping_finer) + 1

dates$settlement_date_grouping_finer <- abs(max_group - dates$settlement_date_grouping_finer)

##All which are at smallest island group level
Island_group_all_sep <- polygon_geo_grouping_hierarchy %>% 
  full_join(polygon_geo, by = c("Smallest_Island_group", "sum_area", "sum_shoreline", "mean_lat", "mean_long",
                                "max_long", "min_long", "max_lat", "min_lat")) %>% 
  full_join(dates, by = "Smallest_Island_group") %>% 
  full_join(modis_NPP, by = "Smallest_Island_group", relationship = "many-to-many") %>% 
  full_join(pol_complex, by = "Smallest_Island_group", relationship = "many-to-many")  %>% 
  full_join(ecoClimate_data, by = "Smallest_Island_group") %>% 
  distinct()   

write_tsv(Island_group_all_sep, "output/processed_data/RO_Hedvig_aggregate_all_obs_sep.tsv")

#Aggregation over smallest unit
Island_group_summarised_smallest <- Island_group_all_sep %>% 
  filter(!is.na(settlement_date_grouping_finer)) %>% 
  group_by(Smallest_Island_group) %>% 
  dplyr::summarise(
            mode_pol_complex = getmode(pol_complex_code_Hedvig), 
            sum_area = sum(sum_area, na.rm = T), 
            lg_count_smallest = mean(lg_count_smallest, na.rm = T),
            sum_shoreline = sum(sum_shoreline, na.rm = T),
            mean_lat = mean(mean_lat),
            mean_long = mean(mean_long),
            settlement_date_grouping_finer = max(settlement_date_grouping_finer), 
            oldest_date = max(oldest_date),
            color = dplyr::first(smallest_island_color), 
            lg_count = dplyr::first(lg_count_smallest),
            NPP_terra_mean = mean(MOD17A3HGF_061_Npp_500m_terra, na.rm = T),
            NPP_aqua_mean = mean(MYD17A3HGF_061_Npp_500m_water, na.rm = T),
            mean_CCSM_piControl_1760_bio1 = mean(mean_CCSM_piControl_1760_bio1, na.rm = T),
            mean_CCSM_piControl_1760_bio4 = mean(mean_CCSM_piControl_1760_bio4, na.rm = T),
            mean_CCSM_piControl_1760_bio12 = mean(mean_CCSM_piControl_1760_bio12, na.rm = T),
            mean_CCSM_piControl_1760_bio15 = mean(mean_CCSM_piControl_1760_bio15, na.rm = T)
            ) %>% 
  mutate(ratio_coastline_to_area = sum_shoreline / sum_area,
         mean_lat_abs = abs(mean_lat))

write_tsv(Island_group_summarised_smallest, "output/processed_data/RO_Hedvig_aggregate_smallest_island.tsv")

#medium_group
Island_group_summarised_medium <- Island_group_all_sep %>%
  filter(!is.na(settlement_date_grouping_finer)) %>% 
  group_by(Medium_only_merged_for_shared_language) %>% 
  dplyr::summarise(
        mode_pol_complex = getmode(pol_complex_code_Hedvig), 
        sum_area = sum(sum_area, na.rm = T), 
        lg_count_smallest = mean(lg_count_smallest, na.rm = T),
        sum_shoreline = sum(sum_shoreline, na.rm = T),
        mean_lat = mean(mean_lat),
        mean_long = mean(mean_long),
        settlement_date_grouping_finer = max(settlement_date_grouping_finer), 
        oldest_date = max(oldest_date),
        color = dplyr::first(medium_group_color), 
        lg_count = dplyr::first(lg_count_medium),
        NPP_terra_mean = mean(MOD17A3HGF_061_Npp_500m_terra, na.rm = T),
        NPP_aqua_mean = mean(MYD17A3HGF_061_Npp_500m_water, na.rm = T),
        mean_CCSM_piControl_1760_bio1 = mean(mean_CCSM_piControl_1760_bio1, na.rm = T),
        mean_CCSM_piControl_1760_bio4 = mean(mean_CCSM_piControl_1760_bio4, na.rm = T),
        mean_CCSM_piControl_1760_bio12 = mean(mean_CCSM_piControl_1760_bio12, na.rm = T),
        mean_CCSM_piControl_1760_bio15 = mean(mean_CCSM_piControl_1760_bio15, na.rm = T)
) %>% 
  mutate(ratio_coastline_to_area = sum_shoreline / sum_area,
         mean_lat_abs = abs(mean_lat))

write_tsv(Island_group_summarised_medium, "output/processed_data/RO_Hedvig_aggregate_medium_island.tsv")


#Marck
Island_group_summarised_Marck_group <- Island_group_all_sep %>% 
  filter(!is.na(settlement_date_grouping_finer)) %>% 
  group_by(Marck_group) %>% 
  dplyr::summarise(
    mode_pol_complex = getmode(pol_complex_code_Hedvig), 
    sum_area = sum(sum_area, na.rm = T), 
    lg_count_smallest = mean(lg_count_smallest, na.rm = T),
    sum_shoreline = sum(sum_shoreline, na.rm = T),
    mean_lat = mean(mean_lat),
    mean_long = mean(mean_long),
    settlement_date_grouping_finer = max(settlement_date_grouping_finer), 
    oldest_date = max(oldest_date),
    color = dplyr::first(Marck_group_color), 
    lg_count = dplyr::first(lg_count_Marck),
    NPP_terra_mean = mean(MOD17A3HGF_061_Npp_500m_terra, na.rm = T),
    NPP_aqua_mean = mean(MYD17A3HGF_061_Npp_500m_water, na.rm = T),
    mean_CCSM_piControl_1760_bio1 = mean(mean_CCSM_piControl_1760_bio1),
    mean_CCSM_piControl_1760_bio4 = mean(mean_CCSM_piControl_1760_bio4),
    mean_CCSM_piControl_1760_bio12 = mean(mean_CCSM_piControl_1760_bio12),
    mean_CCSM_piControl_1760_bio15 = mean(mean_CCSM_piControl_1760_bio15),
  ) %>% 
  mutate(ratio_coastline_to_area = sum_shoreline / sum_area,
         mean_lat_abs = abs(mean_lat))

write_tsv(Island_group_summarised_Marck_group, "output/processed_data/RO_Hedvig_aggregate_marck_group.tsv")


#SBZR group
Island_group_summarised_SBZR_group <- Island_group_all_sep %>% 
  filter(!is.na(settlement_date_grouping_finer)) %>% 
  group_by(SBZR_group) %>% 
  dplyr::summarise(
    mode_pol_complex = getmode(pol_complex_code_Hedvig), 
    sum_area = sum(sum_area, na.rm = T), 
    lg_count_smallest = mean(lg_count_smallest, na.rm = T),
    sum_shoreline = sum(sum_shoreline, na.rm = T),
    mean_lat = mean(mean_lat),
    mean_long = mean(mean_long),
    settlement_date_grouping_finer = max(settlement_date_grouping_finer), 
    oldest_date = max(oldest_date),
    color = dplyr::first(SBZR_group_color), 
    lg_count = dplyr::first(lg_count_SBZR),
    NPP_terra_mean = mean(MOD17A3HGF_061_Npp_500m_terra, na.rm = T),
    NPP_aqua_mean = mean(MYD17A3HGF_061_Npp_500m_water, na.rm = T),
    mean_CCSM_piControl_1760_bio1 = mean(mean_CCSM_piControl_1760_bio1),
    mean_CCSM_piControl_1760_bio4 = mean(mean_CCSM_piControl_1760_bio4),
    mean_CCSM_piControl_1760_bio12 = mean(mean_CCSM_piControl_1760_bio12),
    mean_CCSM_piControl_1760_bio15 = mean(mean_CCSM_piControl_1760_bio15),
  ) %>% 
  mutate(ratio_coastline_to_area = sum_shoreline / sum_area,
         mean_lat_abs = abs(mean_lat))

write_tsv(Island_group_summarised_SBZR_group, "output/processed_data/RO_Hedvig_aggregate_SBZR_group.tsv")


##COUNTRY



Island_group_summarised_COUNTRY <- Island_group_all_sep %>% 
  filter(!is.na(settlement_date_grouping_finer)) %>% 
  group_by(`COUNTRY NAME`) %>% 
  dplyr::summarise(
    mode_pol_complex = getmode(pol_complex_code_Hedvig), 
    sum_area = sum(sum_area, na.rm = T), 
    lg_count_smallest = mean(lg_count_smallest, na.rm = T),
    sum_shoreline = sum(sum_shoreline, na.rm = T),
    mean_lat = mean(mean_lat),
    mean_long = mean(mean_long),
    settlement_date_grouping_finer = max(settlement_date_grouping_finer), 
    oldest_date = max(oldest_date),
    lg_count = dplyr::first(lg_count_COUNTRY),
    NPP_terra_mean = mean(MOD17A3HGF_061_Npp_500m_terra, na.rm = T),
    NPP_aqua_mean = mean(MYD17A3HGF_061_Npp_500m_water, na.rm = T),
    mean_CCSM_piControl_1760_bio1 = mean(mean_CCSM_piControl_1760_bio1),
    mean_CCSM_piControl_1760_bio4 = mean(mean_CCSM_piControl_1760_bio4),
    mean_CCSM_piControl_1760_bio12 = mean(mean_CCSM_piControl_1760_bio12),
    mean_CCSM_piControl_1760_bio15 = mean(mean_CCSM_piControl_1760_bio15),
  ) %>% 
  mutate(ratio_coastline_to_area = sum_shoreline / sum_area,
         mean_lat_abs = abs(mean_lat))

write_tsv(Island_group_summarised_COUNTRY, "output/processed_data/RO_Hedvig_aggregate_COUNTRY_group.tsv")
