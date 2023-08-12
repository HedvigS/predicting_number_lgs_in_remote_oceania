#SBZR groups

data <- read_tsv("output/processed_data/RO_Hedvig_aggregate_SBZR_group_scaled.tsv", show_col_types = F)

colnames(data) <- str_replace_all(colnames(data), "_", "\n")

png(filename = "output/plots/SLOM_SBZR_all_variables.png", width = 10, height = 10, units = "in", res = 300)
data %>%   
  dplyr::select("lg\ncount","EA033",  
                "Carrying\ncapactiy\nPC1", 
                "Carrying\ncapactiy\nPC2",
                "Settlement\ndate\ngrouping\nfiner", "Area\nland" , "Shoreline", "ratio\ncoastline\nto\narea", 
                "Latitude\nabs\nmean","Annual\ntemperature\nmean", "Temperature\nseasonality\nmean", "Annual\nprecipitation\nmean", "Precipitation\nseasonality\nmean", "NPP\nterra\nmean", 
                #"NPP\nterra\nvar", 
                "NPP\naqua\nmean" 
                #"NPP\naqua\nvar"
  ) %>% 
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
x <- dev.off()


#medium groups
data <- read_tsv("output/processed_data/RO_Hedvig_aggregate_medium_group_scaled.tsv", show_col_types = F)

colnames(data) <- str_replace_all(colnames(data), "_", "\n")

png(filename = "output/plots/SLOM_medium_all_variables.png", width = 10, height = 10, units = "in", res = 300)
data %>%   
  dplyr::select("lg\ncount","EA033",  "Settlement\ndate\ngrouping\nfiner", "Area\nland" , "Shoreline", "ratio\ncoastline\nto\narea", 
                "Latitude\nabs\nmean","Annual\ntemperature\nmean", "Temperature\nseasonality\nmean", "Annual\nprecipitation\nmean", "Precipitation\nseasonality\nmean", "NPP\nterra\nmean",
                #"NPP\nterra\nvar", 
                "NPP\naqua\nmean" 
                #"NPP\naqua\nvar"
  ) %>% 
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
x <- dev.off()



#country
data <- read_tsv("output/processed_data/RO_Hedvig_aggregate_country_group_scaled.tsv", show_col_types = F)

colnames(data) <- str_replace_all(colnames(data), "_", "\n")

png(filename = "output/plots/SLOM_medium_all_variables.png", width = 10, height = 10, units = "in", res = 300)
data %>%   
  dplyr::select("lg\ncount","EA033",  "Settlement\ndate\ngrouping\nfiner", "Area\nland" , "Shoreline", "ratio\ncoastline\nto\narea", 
                "Latitude\nabs\nmean","Annual\ntemperature\nmean", "Temperature\nseasonality\nmean", "Annual\nprecipitation\nmean", "Precipitation\nseasonality\nmean", "NPP\nterra\nmean",
                #"NPP\nterra\nvar", 
                "NPP\naqua\nmean" 
                #"NPP\naqua\nvar"
  ) %>% 
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
x <- dev.off()



#country
data <- read_tsv("output/processed_data/RO_Hedvig_aggregate_country_group_scaled.tsv", show_col_types = F)

colnames(data) <- str_replace_all(colnames(data), "_", "\n")

colnames(data)

png(filename = "output/plots/SLOM_country_all_variables.png", width = 10, height = 10, units = "in", res = 300)
data %>%   
  dplyr::select("lg\ncount","EA033",  
                "Settlement\ndate\ngrouping\nfiner", 
                "Area\nland" , "Shoreline", "ratio\ncoastline\nto\narea", 
                "Latitude\nabs\nmean",
                "Annual\ntemperature\nmean", 
                "Temperature\nseasonality\nmean", 
                "Annual\nprecipitation\nmean", 
                "Precipitation\nseasonality\nmean", 
                "NPP\nterra\nmean",
                "NPP\naqua\nmean",
                "Carrying\ncapactiy\nPC1",
                "Carrying\ncapactiy\nPC2"
  ) %>% 
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
x <- dev.off()
