source("01_requirements.R")
source("01_requirements_brms.R")

pkgs <- c("NCmisc", "knitr", "bib2df", "tidyr")

groundhog.library(pkgs, groundhog_date)

r_fns <- list.files(path = ".", pattern = "*.R$", full.names = T, recursive = F)

df <- data.frame("packages" = as.character(),
                 "functions" = as.character(),
                 "scripts" = as.character())


for(fn in r_fns){

#  fn <- r_fns[19]
  
  cat(paste0("I'm on ", fn, ".\n"))
  
x <- NCmisc::list.functions.in.file(filename = fn) %>% 
  as.data.frame() %>% 
  rownames_to_column("packages") %>% 
  rename("functions" = 2) %>% 
  mutate(packages = str_replace_all(packages, "package:", "")) %>% 
  mutate(packages = str_replace_all(packages, "c\\(", "")) %>% 
  mutate(packages = str_replace_all(packages, "\\)", "")) %>% 
  mutate(packages = str_replace_all(packages, "\\\"", "")) %>% 
  mutate(packages = str_replace_all(packages, "character\\(0", "")) %>% 
  mutate(packages = str_split(packages, ",")) %>% 
  unnest(cols = "packages") %>% 
  unnest(cols = "functions") %>% 
  mutate(scripts = fn)

df <- full_join(x, df, by = c("packages", "functions", "scripts"))
}

used_packages <- df %>% 
  mutate(used = "TRUE")

# dealing with instances where a package wasn't found. in pipe above this was listed as "" but it should be a proper NA
used_packages <- naniar::replace_with_na(data = used_packages, replace= list(packages = ""))
used_packages$packages <- trimws(used_packages$packages)

#df with loaded packages
loaded_packages <- data.frame(packages = (.packages())) %>% 
  mutate(loaded = "TRUE")

joined_df <- full_join(used_packages, loaded_packages, by = "packages")

unused_but_loaded <- joined_df %>% 
  filter(is.na(used)) %>% 
  filter(!is.na(loaded)) 

cat("There are ", nrow(unused_but_loaded), "packages that it seems like you're not using, but that are loaded.\n They are: ", unused_but_loaded$packages, ".\n" )

most_used <- used_packages %>% 
  distinct(packages, functions) %>% 
  group_by(packages) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  filter(packages != ".GlobalEnv") %>% 
  filter(!is.na(packages))
  
cat("The top 5 packages from which you use the most different functions are:\n ")
most_used[1:5,]

cat("Keep in mind, this is not top-5 per times you use the package but the top-5 of pacakges from which you use the most functions.")

most_used$packages <- fct_reorder(most_used$packages, most_used$n)

most_used %>% 
  ggplot() +
  geom_bar(aes(x = packages, y = n, fill = n), stat = "identity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1), 
        legend.position = 0) 

ggsave("output/processed_data/used_packages.png", width = 10, height = 10)

script_with_most_functions <-  used_packages %>% 
  distinct(scripts, functions) %>% 
  group_by(scripts) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) 

cat("The top 5 scripst which use the most different functions:\n ")
script_with_most_functions [1:5,]

packages_in_most_scripts <-  used_packages %>% 
  distinct(scripts, packages) %>% 
  group_by(packages) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) 

cat("The top 5 packages that are used in the most scripts:\n ")
packages_in_most_scripts[1:5,]

#generating bibtex file of all the packages where you've used at least one funciton

output_fn <- "../latex/used_pkgs.bib"

#for unclear reasons, NCmisc::list.functions.in.file is not able to pick up on the following packages being in use so they are manually added.
used_but_not_detected_by_NCmisc <- c("brms", "fs", "patchwork", "cmdstanr", "MCMCglmm", "maps", "rstanarm",
                                     "StanHeaders", "mvtnorm", "Rcpp", "coda", "maps" ,"ade4",  "devtools",  "rlang", "nFactors")


pkgs_to_cite <- c(used_but_not_detected_by_NCmisc, as.character(most_used$packages)) %>% 
  unique() %>% sort()

knitr::write_bib(x = pkgs_to_cite, file = output_fn)

readLines(output_fn) %>% 
  str_replace_all("\\&", "\\\\&") %>%
  str_replace_all("\\\\\\\\&", "\\\\&") %>% #.[307:319]
    writeLines(output_fn)

cat(paste0("Wrote citations for packages you've used to", output_fn, ".\n There were ", length(!is.na(most_used$packages %>% unique()))
, " entries.\n" ))

#optional part, this generates a text string with the bibtex citation KEYS from earlier that you can then paste into LaTeX in order to cite all

bibdf <- suppressWarnings(bib2df(output_fn))

#solution to getting an "and" on the last line from SO
# https://stackoverflow.com/questions/42456452/changing-vector-to-string-special-separator-for-last-element
fPaste <- function(vec) sub(",\\s+([^,]+)$", " and \\1", toString(vec))

vec <- paste0("\\citet{", bibdf$BIBTEXKEY, "}")

fPaste(vec)   %>% 
  writeLines(con = "../latex/citation_keys.txt")


fn_out = "../latex/appendix_used_packages_table_versions.tex"
cap <- "Table of R-packages used in this study."
lbl <- "appendix_r_package_table"
align <- c("r","p{2.5cm}","p{2.5cm}") 


pkgs_to_cite_df <- installed.packages()[pkgs_to_cite, "Version"] %>% 
  as.data.frame() %>% 
  rownames_to_column("Package") %>% 
  rename("Version" = "." )
  
  pkgs_to_cite_df %>% 
    write_tsv("output/pkgs_versions_table.tsv")
  
  pkgs_to_cite_df %>% 
  xtable(caption = cap, label = lbl,
         align = align) %>% 
  xtable::print.xtable(file = fn_out, 
                       sanitize.colnames.function = function(x){x},
                       sanitize.text.function = function(x){x},
                       include.rownames = FALSE, math.style.negative = F,tabular.environment = "longtable",
                       booktabs = TRUE, floating = F) 

