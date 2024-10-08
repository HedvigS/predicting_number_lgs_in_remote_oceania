# Run this script first to make sure you have all the necessary packages, directories and functions.

set.seed(1988)

#### PACKAGE INSTALLATION ####
R_version_numbers <- paste0(R.version$major, ".", R.version$minor)

if(R_version_numbers != "4.4.1"){
  message("These scripts were written using R version 4.4.1, but you are using ", R_version_numbers, ". This needn't be a problem, but it's worth noting in case the results differ.\n")
    }

#instead of using the R-packages for package mangement groundhog or pacman, I have recorded precisely which version is used for each package in a tsv-file and will be looping over each of them with a for-loop applying a function which installs the specific version. If there is another version present, the requested version will be installed. 

pkgs <- read.delim("data/packages.tsv", sep = "\t")

installed_pkgs <- installed.packages()

#the following functions may be masked by installing packages, i.e. it's necessary to specify the package when calling the function (package::function) to make sure you get the right function.
mask.ok_vec <- c("det", "expand", "pack", "unpack", "backsolve", "forwardsolve", "grid.draw", "grid.draw.absoluteGrob", "grobHeight.absoluteGrob", "robWidth.absoluteGrob", "robX.absoluteGrob",   "obY.absoluteGrob")

#the package remotes is necessary in order to use the function install_version
if(! "remotes" %in% rownames(installed_pkgs)){
install.packages("remotes", )
  }

for(i in 1:nrow(pkgs)){

#  i <- 14
  pkg <- pkgs[i,1]
  version <- pkgs[i,2]

  
  if(!pkg %in% rownames(installed_pkgs)){
    cat(paste0("Installing/loading packages. I'm on ", pkg,  " which is ", i , " of ", nrow(pkgs), ".\n"))
    cat(paste0("Package not installed, installing now.\n"))
    
    remotes::install_version(pkg, version = version, dependencies = "Depends", repos = "http://cran.us.r-project.org", upgrade = "never", quiet = T, force = F)
  }

  if(pkg %in% rownames(installed_pkgs) ){
        if(  installed_pkgs[pkg, "Version"] != version){
    
          cat(paste0("Installing/loading packages. I'm on ", pkg,  " which is ", i , " of ", nrow(pkgs), ".\n"))
    cat(paste0("Package installed, but not the right version. Installing requested version now.\n"))

    remotes::install_version(pkg, version = version, dependencies = "Depends", repos = "http://cran.us.r-project.org", upgrade = "never", quiet = T, force = F)
    
    }}
    
  library(pkg, character.only = T, warn.conflicts = F, quietly = T, verbose = F, attach.required = T, mask.ok = mask.ok_vec)
}

#### R packages not on cran 

# cmdstanr
if(!("cmdstanr" %in% rownames(installed_pkgs))){
  remotes::install_version(package = "cmdstanr", repos = "https://mc-stan.org/r-packages/", version = "0.8.0")
  cmdstanr::install_cmdstan()
}
library(cmdstanr)

if(!("rgrambank" %in% rownames(installed_pkgs))){
remotes::install_github("HedvigS/rgrambank", ref = "94b3cb2caae4744e0f574b3dd8b5d3c8af40d1d2")
  }
library(rgrambank)

if(!("SH.misc" %in% rownames(installed_pkgs))){
  remotes::install_github("HedvigS/SH.misc", ref = "fbadb86a474d955672101faad5165c87b2f2ef6d")
  }
library(SH.misc)

#small functions
unlist_entire_df <- function(data) {
  temp1 <- sapply(data, is.list)
  temp2 <- do.call(
    cbind, lapply(data[temp1], function(x) 
      data.frame(do.call(rbind, x), check.names=FALSE)))
  cbind(data[!temp1], temp2)
}

#quieting down tidyverse
options(tidyverse.quiet = TRUE)

#set-up output dirs if they don't exist already
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