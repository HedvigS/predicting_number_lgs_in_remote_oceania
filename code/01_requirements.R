# Please run this script first to make sure you have all the necessary packages 
# installed for running the rest of the scripts in this R project

source("fun_def_h_load.R")

h_load(pkg = c(
  "tidyverse",
  "reshape2",
  "jsonlite",
  "modEvA",
  "MuMIn",
  "rsq",
  "viridis",
  "rlang",
  "readODS",
  "devtools",
  "MASS", 
  "colorspace",
  "wesanderson",
  "ggalt",
  "randomcoloR",
  "ggplot2",
  "readxl",
  "ggrepel",
  "psych",
  "ggthemes",
  "readxl",
  "broom", 
  "ggpubr",
  "data.table",
  "naniar", 
  "fields",
  "devtools",
  "xtable",
  "broom", 
  "nFactors",
  "sp", 
  "raster"), verbose = F, dependencies = T
)

unlist_entire_df <- function(data) {
  temp1 <- sapply(data, is.list)
  temp2 <- do.call(
    cbind, lapply(data[temp1], function(x) 
      data.frame(do.call(rbind, x), check.names=FALSE)))
  cbind(data[!temp1], temp2)
}

#quieting down tidyverse
options(tidyverse.quiet = TRUE)

#set-up some output dirs
dir <- "output"
if(!dir.exists(dir)){dir.create(dir)}

dir <- "output/processed_data"
if(!dir.exists(dir)){dir.create(dir)}

dir <- "output/plots"
if(!dir.exists(dir)){dir.create(dir)}

dir <- "output/plots/maps"
if(!dir.exists(dir)){dir.create(dir)}

dir <- "output/results"
if(!dir.exists(dir)){dir.create(dir)}
