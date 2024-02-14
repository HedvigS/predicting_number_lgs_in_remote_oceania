source("01_requirements.R")

#read in
glottolog <- read_tsv("output/processed_data/glottolog_language_table_wide_df.tsv", show_col_types = F)  %>% 
  mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude)) 

polygon_grouping_hierachy <- read_tsv("output/processed_data/subregions.tsv", show_col_types = F)
  
#shifting the longlat of the dataframe to match the pacific centered map
All_polygons <- read_csv("data/RO_polygons_grouped_with_languages.csv", show_col_types = F) %>% 
  mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude)) %>% 
  mutate(area = as.numeric(`AREA (sq km)`)) %>% 
  arrange(area) %>% 
  full_join(polygon_grouping_hierachy, by = c("SBZR_group", "glottocodes", "Smallest_Island_group",  "Medium_only_merged_for_shared_language"), relationship = "many-to-many") %>% 
  filter(!is.na(glottocodes)) %>% 
  mutate(glottocodes = str_split(glottocodes, ",")) %>%
  unnest(glottocodes) %>% 
  mutate(glottocode = trimws(glottocodes)) %>% 
  distinct() 

#Basemap

#worldmaps
#rendering a worldmap that is pacific centered
world <- map_data('world', wrap=c(-25,335), ylim=c(-56,80), margin=T)

lakes <- map_data("lakes", wrap=c(-25,335), col="white", border="gray", ylim=c(-55,65), margin=T)

basemap <- ggplot(All_polygons) +
  geom_polygon(data=world, aes(x=long, 
                               y=lat,group=group),
               colour="gray87", 
               fill="gray87", linewidth = 0.5) +
  geom_polygon(data=lakes, aes(x=long, 
                               y=lat,group=group),
               colour="gray87", 
               fill="white", linewidth = 0.3)  + 
  theme(legend.position="none",
        panel.grid.major = element_blank(), #all of these lines are just removing default things like grid lines, axises etc
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.line = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_blank(),  
        axis.text.y = element_blank(),  
        axis.ticks = element_blank())   +
  coord_map(projection = "vandergrinten") +
#  coord_map(projection = "vandergrinten", xlim = c(130, 255), ylim = c(-56, 27)) +
  scale_x_continuous(expand=c(0,0), limits = c(110, 255)) +
  scale_y_continuous(expand=c(0,0), limits = c(-48, 23)) 

Map_plot_all_polygons <- basemap + 
  geom_point(aes(x=Longitude, y=Latitude), color = All_polygons$smallest_island_color, size = 0.5) 

plot(Map_plot_all_polygons)
ggsave("output/plots/maps/Map_RO_Smallest.png", width = 5, height = 3)

##SBZR grouping
Map_plot_SBZR <- basemap + 
  geom_point(aes(x=Longitude, y=Latitude), color = All_polygons$SBZR_group_color, size = 0.5) #+
#  scale_color_manual(values = color_vector_smallest)

plot(Map_plot_SBZR)
ggsave("output/plots/maps/Map_RO_SBZR.png", width = 5, height = 4)
ggsave("../latex/polygon_SBZR_group_map.png", width = 5, height = 3)

#pol_complex
col_vector_3 <- c("#91bfdb", "#ffffbf", "#fc8d59", "#d7191c")

pol_complex_data <-  read_tsv("data/Remote_oceania_pol_complex.tsv", na = "", show_col_types = F) %>%
  dplyr::select(Language_level_ID = glottocode, `Political complexity (EA033)`) %>% 
  mutate(glottocode = ifelse(Language_level_ID == "fiji1243", "fiji1243,kada1285,sout2864,nort2843", Language_level_ID)) %>% 
  mutate(glottocode = ifelse(Language_level_ID == "aust1304",  "aust1304,raiv1237,tubu1240,ruru1237,rima1237", glottocode)) %>% 
  mutate(glottocode = ifelse(Language_level_ID == "maor1246", "maor1246,mori1267", glottocode)) %>% 
  mutate(glottocode = str_split(glottocode, ",")) %>% 
  unnest(cols = c(glottocode)) %>% 
  dplyr::select(glottocode , `Political complexity (EA033)`) %>% 
  filter(!is.na(`Political complexity (EA033)`)) %>% 
  mutate(`Political complexity (EA033)` = as.character(`Political complexity (EA033)`)) %>% 
  inner_join(All_polygons, by = "glottocode", relationship = "many-to-many") %>% 
    group_by(`Political complexity (EA033)`, glottocode) %>% 
    summarise(Latitude = mean(Latitude, na.rm = T),
              Longitude = mean(Longitude, na.rm = T), .groups = "drop") 

trans <- scales::alpha("#91bfdb", 0.2)

basemap + 
  geom_jitter(data = pol_complex_data, aes(x=Longitude, y=Latitude, fill = `Political complexity (EA033)`),
              size = 3, 
              color = "darkblue",
              alpha = 0.8, shape = 21, stroke = 0.5, width = 1) +
  scale_fill_manual(values = col_vector_3) +
  theme(legend.position = c(0.2,0.42),
        legend.background  =  element_rect(fill = trans, color = trans), 
        legend.key = element_rect(fill = NA, color = NULL)) 
  
ggsave("output/plots/maps/map_pol_complex.png", width = 9, height = 5)
ggsave("../latex/map_pol_complex.png", width = 9, height = 5)

#dates
dates <- read_tsv("data/island_group_settlement_date.tsv", show_col_types = F) %>% 
  rename(settlement_date_grouping_finer = "Time depth settlement group", `Settlement date oldest date` = `Oldest date`) %>% 
  dplyr::select(Smallest_Island_group, settlement_date_grouping_finer, `Settlement date oldest date`) %>% 
  full_join(All_polygons, by = "Smallest_Island_group"  ) %>% 
  filter(!is.na(Smallest_Island_group)) %>% 
  filter(!is.na(settlement_date_grouping_finer)) %>% 
  group_by(Smallest_Island_group) %>% 
  summarise(Longitude = mean(Longitude), 
            Latitude = mean(Latitude),
            settlement_date_grouping_finer = min(settlement_date_grouping_finer),
            .groups = "drop") %>% 
  arrange(settlement_date_grouping_finer) 

dates_summarised_for_SM <- read_tsv("data/island_group_settlement_date.tsv", show_col_types = F)%>%
  rename(settlement_date_grouping_finer = "Time depth settlement group", `Settlement date oldest date` = `Oldest date`) %>% 
  group_by(`Name in source`) %>% 
  summarise(`Date ranges` = unique(paste0(`Date ranges`, collapse = ", ")),
            Source = unique(paste0(Source, collapse = ", ")),
            settlement_date_grouping_finer = unique(paste0(settlement_date_grouping_finer, collapse = ", ")),
            Meta_source = unique(paste0(Meta_sourcecollapse = ", ")),
            `Based on inference from neighbouring island?` = first(`Based on inference from neighbouring island?`),
            Island_groups = paste0(Smallest_Island_group, collapse = ", "))
  
#dates_labels <- dates %>% 
#  group_by(SBZR_group, settlement_date_grouping_finer) %>% 
#  summarise(Longitude = mean(Longitude), 
#            Latitude = mean(Latitude), 
#            .groups = "drop") 


trans <- scales::alpha("#35B779FF", 0.2)

basemap + 
  geom_jitter(data = dates, aes(x=Longitude, y=Latitude, colour = settlement_date_grouping_finer), 
             size = 3.5, alpha = 0.7) +
#  geom_label(data = dates_labels, aes(x= Longitude, 
       #                               y= Latitude, 
      #                                label = `settlement_date_grouping_finer`, 
     #                                 fill = settlement_date_grouping_finer),
    #                              color = "white",
   #                               size = 3, 
  #                                label.padding = unit(0.15, "lines"), 
 #            position = position_jitter(seed = 4, height =0.7, width = 0.7)) +
  scale_fill_viridis() +
  scale_color_viridis(breaks = c(1, 6 , 12)) +
  theme(legend.position = c(0.2,0.42), 
        legend.background  =  element_rect(fill = trans, color = trans)) +
  labs(color = "Settlement wave")

ggsave("output/plots/maps/Map_RO_dates.png", width = 9, height = 5)
ggsave("../latex/Map_RO_dates.png", width = 9, height = 5)