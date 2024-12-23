source("01_requirements.R")

Polygons <- read_csv("data/RO_polygons_grouped_with_languages.csv", show_col_types = F) %>% 
  group_by(Smallest_Island_group) %>% 
  mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude)) %>% 
  summarise(Longitude = mean(Longitude), 
            Latitude = mean(Latitude)) 

NANO_contact <- read_csv("data/Smallest_Island_group_NANO_contact_manual_edit.tsv", show_col_types = F) %>% 
  dplyr::select(-country_ids) %>% 
  mutate(NANO_contact = as.character(NANO_contact)) %>% 
  mutate(NANO_contact = ifelse(NANO_contact == "0", "None", NANO_contact)) %>% 
  mutate(NANO_contact = ifelse(NANO_contact == "0.5", "Medium", NANO_contact)) %>% 
  mutate(NANO_contact = ifelse(NANO_contact == "1", "High", NANO_contact)) %>% 
  mutate(Smallest_Island_group = str_split(Smallest_Island_group, pattern = ", ")) %>% 
  unnest(Smallest_Island_group) %>% 
  distinct()

joined <- left_join(NANO_contact, Polygons, by = join_by(Smallest_Island_group))

#worldmaps
#rendering a worldmap that is pacific centered
world <- map_data('world', wrap=c(-25,335), ylim=c(-56,80), margin=T)

lakes <- map_data("lakes", wrap=c(-25,335), col="white", border="gray", ylim=c(-55,65), margin=T)

basemap <- ggplot(joined) +
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
        legend.title = element_blank(),
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_blank(),  
        axis.text.y = element_blank(),  
        axis.ticks = element_blank())   +
  coord_map(projection = "vandergrinten") +
  #  coord_map(projection = "vandergrinten", xlim = c(130, 255), ylim = c(-56, 27)) +
  scale_x_continuous(expand=c(0,0), limits = c(110, 255)) +
  scale_y_continuous(expand=c(0,0), limits = c(-48, 23)) 



basemap + 
  geom_jitter(mapping = aes(x = Longitude, y = Latitude, fill = NANO_contact), shape = 21, stroke = 0.7, alpha = 0.9,color = "black") +
  scale_fill_manual(values = c( "#66d48a","#9636bf", "#6598eb"), na.value = "white")

ggsave("output/plots/maps/NANO_contact_map.png", width = 9, height = 8)
ggsave("../latex/NANO_contact_map.png", width = 9, height = 8)
