source("01_requirements.R")
source("fun_def_SPLOM.R")

#SBZR groups

data <- read_tsv("output/processed_data/RO_aggregate_SBZR_group_scaled.tsv", show_col_types = F)

colnames(data) <- str_replace_all(colnames(data), "_", "\n")

data_for_splom <- data %>%   
  dplyr::select("lg\ncount","EA033",  
                "Time depth" = "Settlement\ndate\ngrouping\nfiner", 
                 "Shoreline",
                "environ\nPC1", 
                "environ\nPC2",
                "Latitude\nabs\nmean","Annual\ntemperature\nmean", "Temperature\nseasonality\nmean", "Annual\nprecipitation\nmean", "Precipitation\nseasonality\nmean", "NPP\nterra\nmean", 
                "NPP\naqua\nmean" 
                )
                
fn <- "SPLOM_SBZR_all_variables.png"
                
png(filename = paste0("output/plots/", fn), width = 10, height = 10, units = "in", res = 300)

data_for_splom %>% 
  SPLOM_hedders()

x <- dev.off()

png(filename = paste0("../latex/", fn), width = 10, height = 10, units = "in", res = 300)

data_for_splom %>% 
  SPLOM_hedders()


x <- dev.off()

#medium groups
data <- read_tsv("output/processed_data/RO_aggregate_medium_group_scaled.tsv", show_col_types = F)

colnames(data) <- str_replace_all(colnames(data), "_", "\n")

data_for_splom <- data %>%   
  dplyr::select("lg\ncount","EA033",  
                "Time depth" = "Settlement\ndate\ngrouping\nfiner", 
                "Shoreline",
                "environ\nPC1", 
                "environ\nPC2",
                "environ\nPC3",
                "Latitude\nabs\nmean","Annual\ntemperature\nmean", "Temperature\nseasonality\nmean", "Annual\nprecipitation\nmean", "Precipitation\nseasonality\nmean", "NPP\nterra\nmean", 
                "NPP\naqua\nmean" 
  )

fn <- "SPLOM_medium_all_variables.png"

png(filename = paste0("output/plots/", fn), width = 10, height = 10, units = "in", res = 300)

data_for_splom %>% 
  SPLOM_hedders()

x <- dev.off()


png(filename = paste0("../latex/", fn), width = 10, height = 10, units = "in", res = 300)

data_for_splom %>% 
  SPLOM_hedders()


x <- dev.off()



#country
COUNTRY_NAMES <- read_tsv("data/country_names.tsv", show_col_types = F)

UN_pop <- read_tsv("output/processed_data/pop_un_country_data.tsv", show_col_types = F) %>% 
  dplyr::select(ISO2_code, "Population 1950", "Population 1950 log10" , "Number of languages", "Number of languages log10")

data <- read_tsv("output/processed_data/RO_aggregate_country_group_scaled.tsv", show_col_types = F) %>% 
  inner_join(COUNTRY_NAMES, by ="COUNTRY NAME") %>% 
  left_join(UN_pop, by = join_by(ISO2_code)) 

colnames(data) <- str_replace_all(colnames(data), "_", "\n")
colnames(data) <- str_replace_all(colnames(data), " ", "\n")

data_for_splom <- data %>%   
  dplyr::select("Number of\nlanguages\n(country)" = "Number\nof\nlanguages",
                "Number of\nlanguages (log10)\n(country)" = "Number\nof\nlanguages\nlog10",
                "Population 1950\n(country)" = "Population\n1950",
                "Population 1950\n(log10)\n(country)"= "Population\n1950\nlog10",
                "lg count\n(island groups)" = "lg\ncount",
                "EA033\n(island groups)" = "EA033", 
                "Time depth\n(island groups)" = "Settlement\ndate\ngrouping\nfiner", 
                "Shoreline\n(island groups)" = "Shoreline", 
                "environ PC1\n(island groups)" = "environ\nPC1",
                "environ PC2\n(island groups)" = "environ\nPC2"
  ) 

fn <- "SPLOM_country_all_variables.png"

png(filename = paste0("output/plots/", fn), width = 10, height = 10, units = "in", res = 300)

data_for_splom %>% 
  SPLOM_hedders()

x <- dev.off()

png(filename = paste0("../latex/", fn), width = 10, height = 10, units = "in", res = 300)

data_for_splom %>% 
  SPLOM_hedders()


x <- dev.off()