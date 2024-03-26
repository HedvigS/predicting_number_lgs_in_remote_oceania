library(remotes)
install_github("SimonGreenhill/rcldf", dependencies = TRUE, ref = "v1.0.0")
library(rcldf)
library(readr)
library(stringr)

install.packages("R.cache")
library(R.cache)
clearCache()

#glottolog5 
cldf_object <- rcldf::cldf( "https://zenodo.org/records/10804582/files/glottolog/glottolog-cldf-v5.0.zip")


cldf_datasets <- read.delim("https://raw.githubusercontent.com/cldf-datasets/clld_meta/4d544e6e27ec79fa2a572827981454115e15fef1/cldf/contributions.csv", sep = ",")


cldf_datasets %>% 
  filter(
    str_detect("Name", "Grambank")) %>% View()

