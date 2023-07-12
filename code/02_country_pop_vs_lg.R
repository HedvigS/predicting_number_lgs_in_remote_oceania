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
  dplyr::select(ISO2_code, Time, TPopulation1July) %>%
  filter(!is.na(ISO2_code)) %>% 
  filter(Time == 1950) %>% 
  filter(!is.na(TPopulation1July)) %>% 
  mutate("Population 1950" = TPopulation1July*1000) #it's in thousands

combined <- UN_pop_DF  %>% 
  inner_join(lg_count_countries_df, by = "ISO2_code") %>% 
  mutate("Population 1950 log10" = log10(`Population 1950`)) %>% 
  mutate("Number of languages log10" = log10(`Number of languages`))

combined %>%
  ggplot(aes(x = `Population 1950` , y = `Number of languages`)) +
  geom_point( ) +
  theme_classic() +
  ggpubr::stat_cor(method = "pearson", p.digits = 2, geom = "label", color = "blue",
                   label.y.npc="top", label.x.npc = "left", alpha = 0.8) +
  geom_smooth(method='lm', formula = 'y ~ x')

ggsave("output/plots/number_of_languags_vs_pop_1950.png", width = 7, height = 7)

combined %>%
  ggplot(aes(x = `Population 1950 log10` , y = `Number of languages log10`)) +
  geom_point( ) +
  theme_classic() +
  ggpubr::stat_cor(method = "pearson", p.digits = 2, geom = "label", color = "blue",
                   label.y.npc="top", label.x.npc = "left", alpha = 0.8) +
  geom_smooth(method='lm', formula = 'y ~ x')

ggsave("output/plots/number_of_languags_vs_pop_1950_log10.png", width = 7, height = 7)
