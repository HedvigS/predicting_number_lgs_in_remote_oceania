source("01_requirements.R")

subregions <- read.delim("data/oceania_subregions.tsv", sep = "\t") %>% 
  dplyr::select(-Smallest_Island_group)

subregions <- read.delim("data/RO_polygons_grouped_with_languages.csv", sep = ",") %>% 
  filter(!is.na(glottocodes)) %>%
  filter(glottocodes != "") %>% 
  mutate(glottocodes = str_split(glottocodes, ",")) %>%
  unnest(glottocodes) %>% 
  mutate(Glottocode = trimws(glottocodes)) %>% 
  distinct(Smallest_Island_group, Glottocode) %>% 
  group_by(Glottocode) %>% 
  summarise(Smallest_Island_group = paste(Smallest_Island_group, collapse = ", ")) %>% 
  full_join(subregions, by = "Glottocode") %>% 
  filter(!is.na(Glottocode)) %>% 
  filter(Glottocode != "") 
  
subregions %>%
  write_tsv("data/oceania_subregions.tsv")
  
  
##naming the new marck groups, SBZR  
  
polygons_df <- read.delim("data/RO_polygons_grouped_with_languages.csv", sep = ",") %>% 
  dplyr::select(-SBZR_group, -cost_area_id) 

polygons_df_distinct <- polygons_df %>% 
  distinct(Marck_group, Smallest_Island_group)

SBZR_df <- read_tsv("data/polygons_table_new_group.tsv", show_col_types = F) %>% 
    distinct(Unique_ID, cost_area_id)
  
SBZR_df_names <- SBZR_df %>% 
    full_join(polygons_df, by = join_by(Unique_ID)) %>% 
    filter(!is.na(glottocodes)) %>% 
    filter(glottocodes != "")  %>% 
    distinct(Smallest_Island_group, cost_area_id) %>% 
    group_by(cost_area_id) %>% 
    mutate(cost_area_name = paste0(Smallest_Island_group, collapse = " + "),
           n_cost = n()) %>% 
    ungroup() %>% 
    distinct(cost_area_name, cost_area_id, n_cost, .keep_all = T) %>%
    mutate(dup = duplicated(cost_area_name, fromLast = T) + duplicated(cost_area_name, fromLast = F)) %>%  
    mutate(cost_area_name = ifelse(dup >= 1, paste0(cost_area_name, "_", cost_area_id), cost_area_name))  %>% 
  left_join(polygons_df_distinct, 
            relationship = "many-to-many",
            by = join_by(Smallest_Island_group)) %>% 
  mutate(cost_area_name = ifelse(n_cost > 2, paste0(Marck_group), cost_area_name)) %>% 
  distinct(cost_area_name, cost_area_id) %>% 
  mutate(cost_area_name = ifelse(cost_area_name == "North Island (NZ) + South Island (NZ)", "Aotearoa", cost_area_name)) %>% 
  mutate(cost_area_name = ifelse(cost_area_name == "North Marquesas + South Marquesas", "Marquesas", cost_area_name)) %>% 
  full_join(SBZR_df, by = "cost_area_id") %>% 
  rename(SBZR_group = cost_area_name)

read_csv("data/RO_polygons_grouped_with_languages.csv", show_col_types = F) %>%
  dplyr::select(-SBZR_group, -cost_area_id) %>% 
  left_join(SBZR_df_names, by = join_by(Unique_ID)) %>% 
#  rename(`AREA (sq km)` = `AREA..sq.km.`) %>% 
#  rename(`COASTLINE (km) (perimeter)` = `COASTLINE..km...perimeter.`) %>% 
  distinct() %>%
  write_csv("data/RO_polygons_grouped_with_languages.csv")

#read_tsv("data/oceania_subregions.tsv") %>% 
#  full_join(SBZR_df_names) %>% 
#  write_tsv("data/oceania_subregions.tsv")





  
#read_csv("data/Remote_Oceania_Political_complex_and_more/RO_polygons_grouped_with_languages.csv") %>%
#  filter(!is.na(glottocodes)) %>%
#  group_by(Smallest_Island_group, glottocodes, Medium_only_merged_for_shared_language, Marck_group) %>% 
#  summarise(`Longitude (mean)` = mean(Longitude), 
#            `Latitude (mean)` = mean(Latitude)) %>% 
#  write_tsv("output/sheets/grouping_hierarchy_for_latex.tsv")
  
