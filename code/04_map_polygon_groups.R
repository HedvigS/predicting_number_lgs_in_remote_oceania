source("01_requirements.R")

#this script makes map that show which islands are included in which grouping. There are two groupings: "SBZR" and "medium" (Medium_only_merged_for_shared_language). 

#reading in which polygons are associated with which grouping, in table format with centroids of land masses from Global Self-consistent, Hierarchical, High-resolution Geography Database.
Polygon_lgs_glottocodes_sep <- read_csv("data/RO_polygons_grouped_with_languages.csv", show_col_types = F) %>% 
  dplyr::select(SBZR_group, Medium_only_merged_for_shared_language, Longitude, Latitude) %>% 
  mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude))

#worldmaps
#rendering a worldmap that is pacific centered
world <- map_data('world', wrap=c(-25,335), ylim=c(-56,80), margin=T)

lakes <- map_data("lakes", wrap=c(-25,335), col="white", border="gray", ylim=c(-55,65), margin=T)

#Basemap
basemap <- ggplot(Polygon_lgs_glottocodes_sep) +
  geom_polygon(data=world, aes(x=long, 
                               y=lat,group=group),
               colour="gray87", 
               fill="gray87", linewidth = 0.5) +
  geom_polygon(data=lakes, aes(x=long, 
                               y=lat,group=group),
               colour="gray87", 
               fill="white", linewidth = 0.3)  + 
  theme(panel.grid.major = element_blank(), #all of these lines are just removing default things like grid lines, axises etc
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.line = element_blank(), 
        panel.border = element_blank(), 
        axis.text.x = element_blank(),  
        axis.text.y = element_blank(),  
        axis.ticks = element_blank(),
        legend.position = "None", plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  coord_map(projection = "vandergrinten")


##groupings per SBZR_group
SBZR_groups <- read_tsv("output/processed_data/RO_aggregate_SBZR_group.tsv", show_col_types = F) %>% 
  dplyr::select(SBZR_group, color, lg_count) %>% 
  full_join(Polygon_lgs_glottocodes_sep, by = "SBZR_group") %>% 
  filter(!is.na(lg_count)) %>% 
  filter(!is.na(SBZR_group))

color_vec_SBZR <- unique(SBZR_groups$color)

#ggalt::geom_encircle cannot handle groups with 1 or 3 members. So for those island groups, we're adding rows with points that are super-near
groups_two_few_points <- SBZR_groups %>% 
  group_by(SBZR_group) %>% 
  summarise(n = n()) %>% 
  filter(n < 3)

SBZR_groups_too_few_points_df<- SBZR_groups %>% 
  filter(SBZR_group %in% groups_two_few_points$SBZR_group)

SBZR_groups_too_few_points_df_minus <- SBZR_groups %>% 
  mutate(Longitude = Longitude - 0.05, 
         Latitude = Latitude - 0.05)

SBZR_groups_too_few_points_df_plus <- SBZR_groups %>% 
  mutate(Longitude = Longitude + 0.05, 
         Latitude = Latitude + 0.05)

SBZR_groups_for_encircle_plotting_df <- SBZR_groups %>% 
  rbind(SBZR_groups_too_few_points_df_minus) %>% 
  rbind(SBZR_groups_too_few_points_df_plus)

#label df with lg_count
SBZR_groups_for_encircle_plotting_df_labels <- SBZR_groups_for_encircle_plotting_df %>% 
  group_by(SBZR_group, lg_count) %>% 
  summarise(mean_lat = mean(Latitude, na.rm = T),
            mean_long = mean(Longitude, na.rm = T),
            .groups = "drop")

basemap + 
  geom_encircle(data = SBZR_groups_for_encircle_plotting_df, mapping = aes(x=Longitude, y=Latitude, color = SBZR_group), size = 3,
                expand = 0.005, 
                s_shape = 1)  +
  geom_label(data = SBZR_groups_for_encircle_plotting_df_labels, mapping =aes(x = mean_long, 
                                                                               y = mean_lat,
                                                                               label = lg_count, 
                                                                               size = 0.01, 
                                                                               color = SBZR_group),
             label.padding = unit(0.2, "lines"))+
  scale_color_manual(values = color_vec_SBZR) +
  xlim(c(90, 252)) +
  ylim(c(-48, 23))+
  theme( panel.background = element_rect(fill = "white"))

ggsave("output/plots/maps/polygon_SBZR_group_map.png", width = 15, height = 8)
ggsave("../latex/polygon_SBZR_group_map.png", width = 15, height = 8)


#per medium group

if(!file.exists("output/plots/maps/polygon_medium_group_map_vanuatu_mh_inset.png")){

medium_groups <- read_tsv("output/processed_data/RO_aggregate_medium_island.tsv", show_col_types = F) %>% 
  dplyr::select(Medium_only_merged_for_shared_language, color, lg_count) %>% 
full_join(Polygon_lgs_glottocodes_sep, by = "Medium_only_merged_for_shared_language") %>% 
  filter(!is.na(lg_count)) %>% 
  filter(!is.na(Medium_only_merged_for_shared_language))

color_vec_medium <- unique(medium_groups$color)

#ggalt::geom_encircle cannot handle groups with 1 or 3 members. So for those island groups, we're adding rows with points that are super-near
groups_two_few_points <- medium_groups %>% 
  group_by(Medium_only_merged_for_shared_language) %>% 
  summarise(n = n()) %>%
  filter(n < 3)

medium_groups_too_few_points_df <- medium_groups  %>% 
  filter(Medium_only_merged_for_shared_language %in% groups_two_few_points$Medium_only_merged_for_shared_language)

medium_groups_too_few_points_df_minus <- medium_groups_too_few_points_df %>% 
  mutate(Longitude = Longitude - 0.05, 
         Latitude = Latitude - 0.05)

medium_groups_too_few_points_df_plus <- medium_groups_too_few_points_df %>% 
  mutate(Longitude = Longitude + 0.05, 
         Latitude = Latitude + 0.05)

medium_groups_for_encircle_plotting_df <- medium_groups %>% 
  rbind(medium_groups_too_few_points_df_minus) %>% 
  rbind(medium_groups_too_few_points_df_plus)

#label df with lg_count
medium_groups_for_encircle_plotting_df_labels <- medium_groups_for_encircle_plotting_df %>% 
  filter(lg_count >1) %>% 
  group_by(Medium_only_merged_for_shared_language, lg_count) %>% 
  summarise(mean_lat = mean(Latitude, na.rm = T),
            mean_long = mean(Longitude, na.rm = T),
            .groups = "drop")

medium_groups_for_encircle_plotting_df_labels_vanuatu <- 
  medium_groups_for_encircle_plotting_df_labels %>% 
  filter(between(mean_long, 164, 172)) %>% 
  filter(between(mean_lat, -22, -9)) 

medium_groups_for_encircle_plotting_df_labels_without_vanuatu <- medium_groups_for_encircle_plotting_df_labels  %>% 
  anti_join(medium_groups_for_encircle_plotting_df_labels_vanuatu, by = c("Medium_only_merged_for_shared_language", "lg_count", "mean_lat", "mean_long"))

medium_map <- basemap + 
  geom_encircle(data = medium_groups_for_encircle_plotting_df, mapping = aes(x=Longitude, y=Latitude, color = Medium_only_merged_for_shared_language), 
                size = 2,
                expand = 0.0005, 
                s_shape = 1
)  +
  ggrepel::geom_label_repel(data = medium_groups_for_encircle_plotting_df_labels_without_vanuatu, mapping =aes(x = mean_long,  y = mean_lat,                                     label = lg_count,                                      size = 0.01, 
                                                                               color = Medium_only_merged_for_shared_language),
             label.padding = unit(0.2, "lines"), 
             max.overlaps = 40) +
  scale_color_manual(values = color_vec_medium) +
  geom_segment(x = 162.5, y = -24, yend = -9, xend = 162.5, color = "#808080") +
  geom_segment(x = 172, y = -24, yend = -9, xend = 172, color = "#808080") +
  geom_segment(x = 162.5, y = -9, yend = -9, xend = 172, color = "#808080") +
  geom_segment(x = 162.5, y = -24, yend = -24, xend = 172, color = "#808080")+
  xlim(c(90, 252)) +
  ylim(c(-48, 23)) +
theme( panel.background = element_rect(fill = "white"))

ggsave(plot = 
         medium_map, filename = "output/plots/maps/polygon_medium_group_map.png", width = 15, height = 10)

medium_groups_for_encircle_plotting_df_only_vanuatu <-  medium_groups_for_encircle_plotting_df %>% 
  dplyr::filter(dplyr::between(Longitude, 164, 175)) %>% 
  dplyr::filter(dplyr::between(Latitude, -24, -9)) 

  


#Only vanuatu

vanuatu_inset <- basemap +   
  geom_rect(xmin = 162.5, xmax = 172, ymin = -24, ymax = -9, fill = "white") +
  geom_polygon(data=world, aes(x=long, 
                               y=lat,group=group),
               colour="gray87", 
               fill="gray87", linewidth = 0.5) +
  geom_polygon(data=lakes, aes(x=long, 
                               y=lat,group=group),
               colour="gray87", 
               fill="white", linewidth = 0.3)  + 
  geom_encircle(data = medium_groups_for_encircle_plotting_df_only_vanuatu, mapping = aes(x=Longitude, y=Latitude, color = Medium_only_merged_for_shared_language), 
                size = 2,
                expand = 0.0005, 
                s_shape = 1
  )  +
  scale_color_manual(values = color_vec_medium) +
  ylim(c(-24, -9)) +
  xlim(c(162.5, 172)) +
  geom_segment(x = 162.5, y = -24, yend = -9, xend = 162.5, color = "#808080") +
  geom_segment(x = 172, y = -24, yend = -9, xend = 172, color = "#808080") +
  geom_segment(x = 162.5, y = -9, yend = -9, xend = 172, color = "#808080") +
  geom_segment(x = 162.5, y = -24, yend = -24, xend = 172, color = "#808080") +
  ggrepel::geom_label_repel(data = medium_groups_for_encircle_plotting_df_labels_vanuatu , mapping =aes(x = mean_long,  y = mean_lat,                                     label = lg_count,                                      size = 0.01, 
                                                                                                        color = Medium_only_merged_for_shared_language),
                            label.padding = unit(0.2, "lines"), 
                            max.time = 2,
                            max.iter = 20000,
                            force = 2,
                            max.overlaps = 40)  +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  theme(panel.background = element_rect(fill='transparent'),
          plot.background = element_rect(fill='transparent', color=NA)) 

  
ggsave(plot = vanuatu_inset, filename = "output/plots/maps/polygon_medium_group_map_vanuatu_only.png", width = 7, height = 6)

mi <- medium_map +
  geom_segment(x = 162.5, y = -9, yend = -4.5, xend = 147, color = "#808080") + #upper diagonal line
  geom_segment(x = 162.5, y = -24, yend = -54, xend = 145, color = "#808080") + #lower diagonal line
  patchwork::inset_element(vanuatu_inset, align_to = "plot",
                                      right = 0.55, 
                                      bottom = 0.0, 
                                      left = 0, 
                                      top = 0.65)


ggsave(plot = mi, filename = "output/plots/maps/polygon_medium_group_map_vanuatu_mh_inset.png", width = 15, height = 10)
ggsave(plot = mi, filename = "../latex/polygon_medium_group_map_vanuatu_mh_inset.png", width = 15, height = 10)


}
