#This script takes the values and languages tables from a cldf-release and combines then and transforms them to a wide data format from a long. It does not take into account the parameter or code tables.
source("01_requirements.R")
options(timeout=300)

if(dir.exists(paths = "data/dplace-cldf")){
  cat(paste0("D-PLACE already downloaded, skipping fetching it anew."))
}else{
  

  #Script was written by Hedvig Skirgård
  #This script downloads datasets from zenodo that have been released via github, exemplfiying with grambank as the zenodo URL, but you can insert any valid Zenodo download URL.
  
  
  #if you need to download something like grambank-analysed which has inside of it other datasets, look at this script: https://github.com/OlenaShcherbakova/Sociodemographic_factors_complexity/blob/main/get_external_data.R
  

  #setting up a tempfile path where we can put the zipped files before unzipped to a specific location
  filepath <- file.path(tempfile())
  
  ##grambank: downloading, zipping and moving
  Zenodo_url <- c("https://zenodo.org/record/5554412/files/D-PLACE/dplace-cldf-v2.2.1.zip")
  exdir <- "data/dplace-cldf"
  
  utils::download.file(file.path(Zenodo_url), destfile = filepath)
  utils::unzip(zipfile = filepath, exdir = exdir)
  
  #Zenodo locations contain a dir with the name of the repos and the commit in the release. This is not convenient for later scripts, so we move the contents up one level
  
  old_fn <- list.files(exdir, full.names = T)
  old_fn_files <- list.files(old_fn, full.names = T, recursive = T)
  
x <-  file.copy(from = old_fn_files,to = exdir, recursive = T, overwrite = T)
  
  #remove old dir and all of its contents
  unlink(old_fn, recursive = T)
  
}

dplace_language_IDs <- read_csv("data/dplace-cldf/languages.csv", show_col_types = F) %>% 
  dplyr::select(ID, Glottocode)

#data table with information on the language-levlled parent
#
glottolog_fn <- "output/processed_data/glottolog_language_table_wide_df.tsv"
if(!file.exists(glottolog_fn)){
  source("02_get_glottolog_language_table.R")}

glottolog_cldf <- read_tsv(file = glottolog_fn, show_col_types = F) %>% 
  dplyr::select(Glottocode, Language_level_ID)

read_csv("data/dplace-cldf/values.csv", show_col_types = F, col_types = cols(.default = "c")) %>% 
  dplyr::select(ID = Language_ID, Parameter_ID, Value) %>% 
  left_join(dplace_language_IDs, by = "ID") %>% 
  left_join(glottolog_cldf, by = "Glottocode") %>% 
  filter(Parameter_ID == "MonthlyMeanNetPrimaryProduction" |
           Parameter_ID == "NetPrimaryProductionPredictability") %>% 
  mutate(Value = as.numeric(Value)) %>% 
  group_by(Language_level_ID, Parameter_ID) %>% #there are more than one entry per language, so we take the mean
  summarise(n = n(), 
            mean_Value = mean(Value), .groups = "drop") %>% 
  rename(Language_ID = Language_level_ID) %>% 
  reshape2::dcast(Language_ID ~ Parameter_ID, value.var = "mean_Value") %>% 
  write_tsv("output/processed_data/dplace_NPP.tsv", na = "")
 
  