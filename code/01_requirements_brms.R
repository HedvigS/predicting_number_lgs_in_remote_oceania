# Please run this script first to make sure you have all the necessary packages 
# installed for running the rest of the scripts in this R project

source("01_requirements.R")

options(CMDSTANR_NO_VER_CHECK=TRUE)

pkgs = c(
  "coda", 
  "rstan",
  "brms", 
  "mvtnorm",
  "devtools",
  "loo"#,
#  "dagitty",
#  "shape"
)

#if you don't want to use the groundhog package manner of loading and installing packages, set groundhog to anything but "yes"
groundhog <- "yes"

if(groundhog == "yes"){
  
groundhog::groundhog.library(pkgs, groundhog_date)
}else{
  source("fun_def_h_load.R")
  h_load(pkgs)  
}

if(!("rstan" %in% rownames(installed.packages()))){
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
}
library(rstan)

if(!("cmdstanr" %in% rownames(installed.packages()))){
  install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
cmdstanr::install_cmdstan()
}
library(cmdstanr)

#if(!("rethinking" %in% rownames(installed.packages()))){
  
#  devtools::install_github("rmcelreath/rethinking", upgrade = "never")
#}

#library("rethinking")

#If you just want to work through the first half of the course, without bothering with MCMC and Stan installs, you can install the 'slim' version of the rethinking package. Do this:
#devtools::install_github("rmcelreath/rethinking@slim", upgrade = "never")