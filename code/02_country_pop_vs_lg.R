source("01_requirements.R")

if(!file.exists("output/processed_data/glottolog_language_table_wide_df.tsv")){
source("02_get_glottolog_language_table.R")}

lg_count_countries_df  <- read_tsv("output/processed_data/glottolog_language_table_wide_df.tsv", show_col_types = F) %>% 
  filter(level == "language") %>% 
  mutate(Countries = str_split(Countries, ";")) %>% 
  unnest(Countries) %>% 
  group_by(Countries) %>% 
  summarise("Number of languages" = n()) %>% 
  filter(!is.na(`Countries`)) %>% 
  rename("ISO2_code" = Countries) %>% 
  filter(!is.na(`Number of languages`))
  
#The excel sheets used here is downlaoded from the link below on 2021-08-26
#https://population.un.org/wpp/Download/Standard/CSV/

UN_pop_DF <- read_csv("data/United_Nations_population/WPP2022_Demographic_Indicators_Medium.csv", show_col_types = F) %>% 
  dplyr::select(ISO2_code, Time, TPopulation1July, Location) %>%
  filter(!is.na(ISO2_code)) %>% 
  filter(Time == 1950) %>% 
  filter(!is.na(TPopulation1July)) %>% 
  mutate("Population 1950" = TPopulation1July*1000) #it's in thousands

Oceanic_countries <- read_tsv("data/Oceanic_country_codes.tsv", show_col_types = F)

combined <- UN_pop_DF  %>% 
  inner_join(lg_count_countries_df, by = "ISO2_code") %>% 
  left_join(Oceanic_countries, by = join_by(ISO2_code, Location)) %>% 
  mutate("Population 1950 log10" = log10(`Population 1950`)) %>% 
  mutate("Number of languages log10" = log10(`Number of languages`)) %>% 
  mutate(Remote_Oceania = ifelse(is.na(Remote_Oceania), "no", Remote_Oceania))

combined_labels <- combined %>% 
  filter(Remote_Oceania == "yes")

combined %>%
  ggplot(mapping = aes(x = `Population 1950` , y = `Number of languages`)) +
  geom_point(aes(x = `Population 1950` , y = `Number of languages`, color = Remote_Oceania, shape = Remote_Oceania), size = 3 ) +
  theme_classic() +
  ggpubr::stat_cor(method = "pearson", p.digits = 2, color = "darkblue",
                   label.y.npc="top", label.x.npc = "left", alpha = 0.8, size = 8) +
  geom_smooth(method='lm', formula = 'y ~ x') +
  theme(text = element_text(size = 20),
        legend.position = "None") +
  scale_color_manual(values=c("darkolivegreen2","blue4"))

ggsave("output/plots/number_of_languags_vs_pop_1950.png", width = 7, height = 7)
ggsave("../latex/number_of_languags_vs_pop_1950.png", width = 7, height = 7)

combined %>%
  ggplot(aes(x = `Population 1950 log10` , y = `Number of languages log10`)) +
  geom_point( aes(x = `Population 1950 log10` , y = `Number of languages log10`, color = Remote_Oceania, shape = Remote_Oceania), size = 3) +
  
  theme_classic() +
  ggpubr::stat_cor(method = "pearson", p.digits = 2, 
                   color = "darkblue",
                   label.y.npc="top", label.x.npc = "left", 
                   alpha = 0.8, size = 8) +
  geom_smooth(method='lm', formula = 'y ~ x')+
  theme(text = element_text(size = 20), 
        legend.position = "None")+
  scale_color_manual(values=c("darkolivegreen2","blue4")) +
  geom_label_repel(data = combined_labels, mapping = aes(x = `Population 1950 log10` , y = `Number of languages log10`, label = Location), color = "blue4",
                   #force = 1.6, 
                   alpha = 0.6,
                   nudge_x = 0.4, nudge_y = -0.1,
                   max.overlaps = 15) 
  
ggsave("output/plots/number_of_languags_vs_pop_1950_log10.png", width = 7, height = 7)
ggsave("../latex/number_of_languags_vs_pop_1950_log10.png", width = 7, height = 7)


###COUNTRY VS CARRYING CAPCITY
COUNTRY_NAMES <- read_tsv("data/country_names.tsv", show_col_types = F)


Island_group_summarised_COUNTRY <- read_tsv("output/processed_data/RO_Hedvig_aggregate_country_group_scaled.tsv", show_col_types = F) %>% 
  inner_join(COUNTRY_NAMES, by ="COUNTRY NAME") %>% 
  left_join(combined, by = join_by(ISO2_code)) %>% 
  mutate(`Carrying_capactiy_PC1:Shoreline` = abs(1-Carrying_capactiy_PC1) * Shoreline) %>% 
  mutate(`Carrying_capactiy_PC2:Shoreline` = Carrying_capactiy_PC2 * Shoreline)

Island_group_summarised_COUNTRY %>% 
dplyr::select(lg_count, Carrying_capactiy_PC1, Carrying_capactiy_PC2,`Population 1950` ,`Population 1950 log10`, Shoreline, `Carrying_capactiy_PC1:Shoreline`, `Carrying_capactiy_PC2:Shoreline`) %>% 
  pairs.panels(method = "pearson", # correlation method
               hist.col = "#a3afd1",# "#a9d1a3","",""),
               density = TRUE,  # show density plots
               ellipses = F, # show correlation ellipses
               cex.labels= 0.7,
               #           smoother= T,
               cor=T,
               lm=T,
               ci = T, 
               cex.cor = 1,stars = T)


