#This script takes the values and languages tables from a cldf-release and combines then and transforms them to a wide data format from a long. It does not take into account the parameter or code tables.
source("01_requirements.R")
options(timeout=300)

if(dir.exists(paths = "output/processed_data/glottolog_language_table_wide_df.tsv")){
  cat(paste0("glottolog-cldf already downloaded, skipping fetching it anew."))
  }else{

#fetching glottolog from zenodo
  exdir <- "data/glottolog-cldf"

if(!dir.exists(exdir)){  

glottolog_fn <- c("https://zenodo.org/records/10804582/files/glottolog/glottolog-cldf-v5.0.zip")

SH.misc::get_zenodo_dir(url = glottolog_fn, exdir = exdir, drop_dir_level = T)
}

#finding the filenames for the two tables we are intersted in, the language and value tables. The specific filenames can vary, so instead of identifying them via the filename we should check which of the tables conform to particular CLDF-standards and then take the filenames for the tables that conform to those standards fromt the meta-datajson.

glottolog_cldf_fn <- paste0(exdir, "/cldf/")

glottolog_cldf_json <- jsonlite::read_json(paste0(glottolog_cldf_fn, "cldf-metadata.json"))

#finding the fileanme for the relevant tables by checking which of the tables entered into the json meta data file conforms to a given cldf-standard and pulling the filename from there

index <- 0

#going over each table in the json and checking which one conforms and saving the index of that one to a separate variable. First: "values"
for (table in glottolog_cldf_json$tables ) {
  
#table <- glottolog_cldf_json$tables[[1]]
    index <- index +1
  
  if("dc:conformsTo" %in% names(table) & !is.null(table$`dc:conformsTo`)) { #not every table in a cldf dataset has this attribute, or it can be set to "null". So we gotta check that this is even a thing in this table before we proceed
    if(table$`dc:conformsTo` == "http://cldf.clld.org/v1.0/terms.rdf#ValueTable") {index_ValueTable  <- index}
  }}

#using the index we derived above, pulling out the filename for that table
values_fn_name <- glottolog_cldf_json$tables[index_ValueTable][[1]]$url #not sure why this has the name "url" when it is just the filename but that is the way
values_csv_url <- paste0(glottolog_cldf_fn, values_fn_name) #creating the URL path

#doing the same thing for the second table we are interested in, "languages"

index <- 0

for (table in glottolog_cldf_json$tables ) {
  
  index <- index +1
  
  if("dc:conformsTo" %in% names(table) & !is.null(table$`dc:conformsTo`)) { #not every table in a cldf dataset has this attribute, or it can be set to "null". So we gotta check that this is even a thing in this table before we proceed
    if(table$`dc:conformsTo` == "http://cldf.clld.org/v1.0/terms.rdf#LanguageTable") {index_LangaugeTable <- index}
  }}

#using the index we derived above, pulling out the filename for that table
language_fn_name <- glottolog_cldf_json$tables[index_LangaugeTable][[1]]$url
languages_csv_url <- paste0(glottolog_cldf_fn, language_fn_name) #creating the URL path

#reading in data and making it wide
values <- readr::read_csv(values_csv_url, na = c("","<NA>"), col_types = cols()) %>% 
  reshape2::dcast(Language_ID ~ Parameter_ID, value.var = "Value") #making long data wide

languages <- readr::read_csv(languages_csv_url, na = c("","<NA>"), col_types = cols()) %>% 
  rename(Language_level_ID = Language_ID) %>% 
  rename(Language_ID = ID)

#The languages-table from glottolog-cldf contains a paramter called "Language_ID" which is NOT the same as the parameter "Language_ID" in the values tables. This parameter is in fact the language leveled parent of a dialect. In order to avoid confusion, let's rename the parameter in the languages tables to the more transparent "Language_level_ID". This set-up first test if this is indeed a problem (i.e. if this is glottolog-cldf) and only does the renaming then.

glottolog_language_table_wide_df <- dplyr::full_join(values,languages, by = "Language_ID") %>% 
  mutate(Language_level_ID = ifelse(level == "language", Language_ID, Language_level_ID))

write_tsv(glottolog_language_table_wide_df, "output/processed_data/glottolog_language_table_wide_df.tsv")

cat("glottolog-cldf table created.\n")

  }
