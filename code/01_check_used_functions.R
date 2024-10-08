source("01_requirements.R")

fns <- list.files(path = ".", all.files = T, full.names = T, recursive = T, pattern = ".R$")

SH.misc::credit_packages(fns = fns, output_dir = "../latex/", extra_pkgs = NULL)
