# Please run this script first to make sure you have all the necessary packages 
# installed for running the rest of the scripts in this R project

set.seed(1988)

#if you don't want to use the groundhog package manner of loading and installing packages, set groundhog to anything but "yes"
groundhog <- "no"
#if groundhog is set to "yes", then the code will expect R version 4.3.1. If you use another version of R, please set the argument tolerate.R.version of groundhog.ibrary to the version youa re using, (e.g. tolerate.R.version='4.4.1').

#packages
pkgs = c(
#  "tidyverse",
  "dplyr",
  "terra",
  "maps",
  "coda",
  "igraph",
  "stringr",
  "purrr",
  "readr",
  "reader",
  
  "tidyr",
  "tibble",
  "forcats",
  "magrittr",
  "reshape2",
  "jsonlite",
#  "modEvA",
  "MuMIn",
  "rsq",
  "viridis",
  "rlang",
  "devtools",
  "MASS", 
  "colorspace",
#  "wesanderson",
  "ggalt",
  "randomcoloR",
#  "RColorBrewer",
  "ggplot2",
  "readxl",
  "ggrepel",
  "psych",
  "ggthemes",
#  "readxl",
#  "broom", 
  "ggpubr",
  "lemon",
  "data.table",
  "naniar", 
"bib2df",
  "fields",
  "scales",
  "devtools",
  "xtable",
  "ape", 
  "loo",
  "adephylo", 
  "phytools",
  "MCMCglmm",
  "nFactors",
  "sp", 
"Matrix",
  "raster",
"ade4"
) 

pkgs <- unique(pkgs)

#groundhogr set-up

if(groundhog == "yes"){

groundhog_date = "2023-08-03"

if(!("groundhog"%in% rownames(installed.packages()))){
    remotes::install_version("groundhog", version = "3.1.0")
}
library(groundhog)

groundhog_dir <- paste0("groundhog_libraries_", groundhog_date)

if(!dir.exists(groundhog_dir)){
  dir.create(groundhog_dir)
}

groundhog::set.groundhog.folder(groundhog_dir)

groundhog.library(pkgs, groundhog_date)

}else{
  source("fun_def_h_load.R")
  h_load(pkgs, ignore.deps = F)  
}

#funs
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

dir <- "output/results/drop_one_out/"
if(!dir.exists(dir)){dir.create(dir)}

v <- c(7, 1, 1, 7, 7, 1, 5)


 

distinctive_plot_colors <- c("#FFB6C1",
            "#fcf0b3",
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



gray_dup_to_remove <- c("Sisingga", 
                        "Carolinian",
                        "Futuna", 
                        "Aria",
                        "Madara",
                        "Maututu",
                        "Chuukese",
                        "NakanaiBileki_D",
                        "LwepeSantaCruz",
                        "Buma",
                        "NehanHape",
                        "Woleai",
                        "Marshallese", 
                        "FutunaWest", #mystery language with no entries
                        "Baliledo"#can't get a glottocode match
)

