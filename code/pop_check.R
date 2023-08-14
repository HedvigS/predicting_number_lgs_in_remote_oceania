lgs <- read_csv(file = "data/RO_polygons_grouped_with_languages.csv") %>% 
  filter(!is.na(glottocodes)) %>% 
  filter(glottocodes != "")  %>% 
  mutate(glottocodes = str_split(glottocodes, ",")) %>%
  unnest(glottocodes) %>% 
  mutate(Glottocode = trimws(glottocodes)) %>% 
  distinct(Glottocode, Smallest_Island_group, SBZR_group) 




elcat_values <- read_csv("https://raw.githubusercontent.com/cldf-datasets/elcat/main/cldf/values.csv", show_col_types = F) %>% 
  filter(Parameter_ID == "speaker_number") %>% 
  filter(preferred == "yes") %>% 
  dplyr::select(Language_ID, Parameter_ID, Value)

elcat_lgs <- read_csv("https://raw.githubusercontent.com/cldf-datasets/elcat/main/cldf/languages.csv", show_col_types = F) %>% 
  dplyr::select(Language_ID = ID, Glottocode, Name)

elcat_code <- read_csv("https://raw.githubusercontent.com/cldf-datasets/elcat/main/cldf/codes.csv", show_col_types = F) %>% 
  filter(Parameter_ID == "speaker_number") %>% 
  dplyr::select(ID, Description) %>% 
  separate(ID, into = c("Parameter_ID", "Value"), sep = "-") %>%  
  dplyr::select(Value, Description)

elcat <- full_join(elcat_values, elcat_lgs, relationship = "many-to-many", by = "Language_ID") %>% 
  full_join(elcat_code, relationship = "many-to-many", by = "Value") %>% 
  dplyr::select(Glottocode, Name, Value, Speakers_elcat = Description) %>% 
  distinct()

combined <- lgs %>% 
  left_join(elcat, relationship = "many-to-many") %>% 
  group_by(SBZR_group, Value) %>% 
  mutate(n_value = n()) %>% 
  group_by(SBZR_group) %>% 
  mutate(n_island = n()) %>% 
  distinct(SBZR_group, Value, n_value, n_island) %>% 
  ungroup() %>% 
  mutate(percent = n_value / n_island) %>% 
  filter(is.na(Value))
  