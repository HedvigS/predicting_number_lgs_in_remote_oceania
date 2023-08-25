# Please run this script first to make sure you have all the necessary packages 
# installed for running the rest of the scripts in this R project

set.seed(1988)

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
  "RColorBrewer",
  "ggplot2",
  "readxl",
  "ggrepel",
  "psych",
  "ggthemes",
  "readxl",
  "broom", 
  "ggpubr",
  "lemon",
  "data.table",
  "naniar", 
  "fields",
  "scales",
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


getmode <- function(v, na.rm = T) {
if(na.rm == T){
    v <- v[which(!is.na(v))] }
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

distinctive_plot_colors <- c("#FFB6C1",
            "#faf591",
            "#a6f7c9", 
            "#FFDAB9",
            "#87CEEB",
            "#C8A2C8",
            "#ebb0df",
            "#16c7c7",
            "#edae8e",
            "#CCCCFF",
            "#7DDAD9",
            "#957DAD"

)
