source("01_requirements.R")

if(!file.exists("output/processed_data/isolation_RO_geo_dist_isolation_marck_group.tsv")){

polygon_geo <- read_csv("data/RO_polygons_grouped_with_languages.csv", show_col_types = F) %>% 
  filter(!is.na(Inhabited)) %>% 
  filter(Unique_ID != "11356") %>% #removing the huge islands of new guinea and new zealand, since the centroid is in the middle it doesn't make sense for calculating distances 
  filter(Unique_ID != "4444") %>% 
  filter(Unique_ID != "9102") %>% 
  filter(Unique_ID != "9524")  

polygon_geo_long_lat_matrix_smallest_islands_df <- polygon_geo %>% 
  filter(!is.na(Smallest_Island_group)) %>% 
  group_by(Smallest_Island_group) %>% 
  top_n(n = 100, wt = -`COASTLINE (km) (perimeter)`) %>% 
  ungroup()
   
polygon_geo_long_lat_matrix_smallest_islands <- polygon_geo_long_lat_matrix_smallest_islands_df %>% 
   dplyr::select(Longitude, Latitude) %>% 
   as.matrix() 

polygon_geo_most_df <- polygon_geo %>%
  filter(!is.na(Smallest_Island_group)) %>% 
  group_by(Smallest_Island_group) %>% 
  top_n(n = 100, wt = -`COASTLINE (km) (perimeter)`) %>% 
  ungroup()

polygon_geo_long_lat_matrix_all <- polygon_geo_most_df %>%
   dplyr::select(Longitude, Latitude) %>%
   as.matrix()

atDist <- rdist.earth(polygon_geo_long_lat_matrix_smallest_islands, polygon_geo_long_lat_matrix_all, miles = F)

rownames(atDist) <- polygon_geo_long_lat_matrix_smallest_islands_df$Unique_ID

colnames(atDist) <- polygon_geo_most_df$Unique_ID

atDist_melted <- atDist %>% 
  reshape2::melt()

atDist_melted %>% 
  write_rds("output/processed_data/isolation_RO_geo_dist_atDist_melted.rds")

dist_list_left_side <- polygon_geo %>% 
   dplyr::select(Unique_ID_left = Unique_ID, coastline_left= `COASTLINE (km) (perimeter)`, Smallest_Island_group_left = Smallest_Island_group, Medium_only_merged_for_shared_language_left = Medium_only_merged_for_shared_language, Marck_group_left = Marck_group)
 
dist_list_right_side <- polygon_geo  %>% 
   dplyr::select(Unique_ID_right = Unique_ID, coastline_right= `COASTLINE (km) (perimeter)`, Smallest_Island_group_right = Smallest_Island_group, Medium_only_merged_for_shared_language_right = Medium_only_merged_for_shared_language, Marck_group_right = Marck_group)
 
atDist_melted_sided <-  atDist_melted %>% 
   rename(Unique_ID_left = Var1, Unique_ID_right = Var2, dist = value) %>% 
   mutate(Unique_ID_left = as.character(Unique_ID_left)) %>% 
   mutate(Unique_ID_right = as.character(Unique_ID_right)) %>% 
   left_join(dist_list_left_side, by = "Unique_ID_left") %>% 
   left_join(dist_list_right_side, by = "Unique_ID_right") %>% 
  filter(Smallest_Island_group_left != Smallest_Island_group_right)

atDist_melted_sided %>% 
    write_rds("output/processed_data/isolation_RO_geo_dist_atDist_melted_sided.rds")

####Medium_group

atDist_melted_sided_summaried_medium <- atDist_melted_sided %>% 
  filter(Medium_only_merged_for_shared_language_right != Medium_only_merged_for_shared_language_left) %>% 
  group_by(Unique_ID_left) %>% 
  top_n(n= 1, wt = -dist)

####Distance from larger island groups

largest_island_per_group <- polygon_geo %>% 
  filter(!is.na(Inhabited)) %>% 
  group_by(Medium_only_merged_for_shared_language) %>% 
  top_n(1,  wt = `COASTLINE (km) (perimeter)`) %>% 
  ungroup() %>% 
  dplyr::select(Unique_ID_right = Unique_ID, Largest_in_group = Medium_only_merged_for_shared_language, coastline_right = `COASTLINE (km) (perimeter)`) 

#this gives us the mean distance from all the landmasses within an island group to the largest landmasses of the closest island gosup, weighted such that larger islands matter more. I.e, the distance from puka-puka to the largest islands of marquesas or napuka etc

atDist_df_w_island_largest_island_of_group_closest <- atDist_melted_sided %>%
  filter(Medium_only_merged_for_shared_language_right != Medium_only_merged_for_shared_language_left) %>% 
  group_by(Medium_only_merged_for_shared_language_left) %>% 
  top_n(1, wt = coastline_left) %>% 
  inner_join(largest_island_per_group, by = c("Unique_ID_right", "coastline_right")) %>% 
  group_by(Medium_only_merged_for_shared_language_left) %>% 
  top_n(1, wt = -dist)

atDist_df_w_island_largest_island_of_group_closest %>% 
  filter(!str_detect(Marck_group_left, "Non Remote")) %>% 
  dplyr::select(Medium_only_merged_for_shared_language_left, Medium_only_merged_for_shared_language_right, dist) %>% 
  write_tsv("output/processed_data/isolation_RO_geo_dist_isolation_medium_island.tsv")

#Marck_group
atDist_melted_sided_summaried_medium <- atDist_melted_sided %>% 
  filter(Marck_group_right != Marck_group_left) %>% 
  group_by(Unique_ID_left) %>% 
  top_n(n= 1, wt = -dist)

####Distance from larger island groups

largest_island_per_group <- polygon_geo %>% 
  filter(!is.na(Inhabited)) %>% 
  group_by(Marck_group) %>% 
  top_n(1,  wt = `COASTLINE (km) (perimeter)`) %>% 
  ungroup() %>% 
  dplyr::select(Unique_ID_right = Unique_ID, Marck_group_landmass = Marck_group, `COASTLINE (km) (perimeter)`) 
#this gives us the mean distance from all the landmasses within an island group to the largest landmasses of the closest island gosup, weighted such that larger islands matter more. I.e, the distance from puka-puka to the largest islands of marquesas or napuka etc

atDist_df_w_island_largest_island_of_group_closest <- atDist_melted_sided %>%
  filter(Marck_group_right != Marck_group_left) %>% 
  group_by(Marck_group_left) %>% 
  top_n(1, wt = -coastline_left) %>% 
  inner_join(largest_island_per_group, by = "Unique_ID_right") %>% 
  group_by(Marck_group_left) %>% 
  top_n(1, wt = -dist)

atDist_df_w_island_largest_island_of_group_closest %>% 
  filter(!str_detect(Marck_group_left, "Non Remote")) %>% 
  dplyr::select(Marck_group_left, Marck_group_right, dist) %>% 
  write_tsv("output/processed_data/isolation_RO_geo_dist_isolation_marck_group.tsv")
}
