source("01_requirements.R")
source("01_requirements_brms.R")

if(!("SH.misc"%in% rownames(installed.packages()))){
  remotes::install_github("HedvigS/SH.misc", ref = "3ad2758d63792e1e9216119b4b5bad269a3bf944")}
library(SH.misc)

fns <- list.files(path = ".", all.files = T, full.names = T, recursive = T, pattern = ".R$")

SH.misc::credit_packages(fns = fns, output_dir = "../latex/", print_bibTeX = T, print_tex_citation_string = T, print_LaTeX_table = T, print_tsv = T)



