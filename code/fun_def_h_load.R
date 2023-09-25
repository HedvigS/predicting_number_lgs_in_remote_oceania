
#function to check if a pkg is installed or not, if not it installs it and either way it's loaded.
#inspired by pacman::p_load()

h_load <- function(pkg, 
                   verbose = FALSE, 
                   groundhog_date = "2023-07-17",
                   ignore.deps = FALSE){


#groundhogr set-up
    if(!("groundhog" %in% rownames(installed.packages()))){
      
      install.packages("groundhog")
      library(groundhog)
  
        }else{  
      
          library(groundhog)
      
          }
  
    groundhog_dir <- paste0("groundhog_libraries_", groundhog_date)
  
  if(!dir.exists(groundhog_dir)){
    dir.create(groundhog_dir)
  }
  
  groundhog::set.groundhog.folder(groundhog_dir)
  
      for(p in pkg){
        
      if(is.null(version) & (!(p %in% rownames(installed.packages())))){ #if no version is specified, check if it's installed and if not then go and install it as normal
        
        groundhog::groundhog.library(p, date = groundhog_date, 
                                    ignore.deps = ignore.deps)
        
        if(verbose == T){
          cat(paste0("Installed ", p, ".\n"))}

      }
      
  if(verbose == T){
    library(p, character.only = T, quietly = F)
    cat(paste0("Loaded ", p, ", version ",packageVersion(p),".\n"))
  }else{
    suppressMessages(library(p, character.only = T, quietly = T, verbose = F, warn.conflicts = F))}
  
      }
}
