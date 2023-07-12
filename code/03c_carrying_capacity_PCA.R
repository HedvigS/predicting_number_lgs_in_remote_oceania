source("01_requirements.R")

OUTPUTDIR <- "output/processed_data/pca"
if(!dir.exists(OUTPUTDIR)){dir.create(OUTPUTDIR)}

data <- read_tsv("output/processed_data/RO_Hedvig_aggregate_marck_group_scaled.tsv", show_col_types = F)  %>% 
  column_to_rownames("group") %>% 
  dplyr::select(Area_land, Shoreline, Area_water) %>% 
#  dplyr::select(Annual_precipitation_mean, Precipitation_seasonality_mean, Annual_temperature_mean, Temperature_seasonality_mean, Latitude_abs_mean) %>% 
  mutate_all(as.numeric) %>% 
  as.matrix() 

data_pca <- prcomp(data, scale. = TRUE)

#testing to evaluate the optimal number of components
ev <- eigen(cor(data)) # get eigenEstimates
ap <- nFactors::parallel(
  subject=nrow(data),
  var=ncol(data),
  rep=100,
  cent=0.05)
nS <- nFactors::nScree(x=ev$values, aparallel=ap$eigen$qevpea)

optimal_components <- nS$Components$nparallel

plotnScree(nS)

png(filename = file.path(OUTPUTDIR, "PCA_nscree_plot.png"))
plotnScree(nS)
x <- dev.off()

prop_explained <- sum(summary(data_pca)$importance[2, 1:optimal_components])

nScree_summary_string <- paste(
  "The optimal number of components is", optimal_components, "and they explain", prop_explained * 100, "% of the variation.\n"
)

cat(nScree_summary_string)

writeLines(
  nScree_summary_string,
  con = file.path(OUTPUTDIR, "PCA_nScree_summary.txt")
)