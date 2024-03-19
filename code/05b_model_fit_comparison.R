source("01_requirements.R")
source("fun_def_combine_tsvs.R")
source("fun_def_SPLOM.R")

fns <- list.files("output/results/", pattern = ".*fit_loo.*", full.names = T)
df_fit_loo <- combine_tsvs(fns = fns) %>% 
  mutate(group = ifelse(str_detect(filename, "medium"), "medium", "SBZR")) %>% 
  mutate(control = str_extract(filename, "control_[:alpha:]*")) %>% 
  mutate(control = str_replace(string = control, pattern = "control_",replacement = "")) %>% 
  filter(fit_score == "looic") %>% 
  dplyr::select(control, group, LOO = Estimate)

fns <- list.files("output/results/", pattern = ".*fit_waic.*", full.names = T)
df_fit_waic <- combine_tsvs(fns = fns)  %>% 
  mutate(group = ifelse(str_detect(filename, "medium"), "medium", "SBZR")) %>% 
  mutate(group = ifelse(str_detect(filename, "medium"), "medium", "SBZR")) %>% 
  mutate(control = str_extract(filename, "control_[:alpha:]*")) %>% 
  mutate(control = str_replace(string = control, pattern = "control_",replacement = "")) %>% 
  filter(fit_score == "waic") %>% 
  dplyr::select(control, group, WAIC = Estimate)

fns <- list.files("output/results/", pattern = ".*fit_R2.*", full.names = T)
df_fit_R2 <- combine_tsvs(fns = fns)  %>% 
  mutate(group = ifelse(str_detect(filename, "medium"), "medium", "SBZR")) %>% 
  mutate(group = ifelse(str_detect(filename, "medium"), "medium", "SBZR")) %>% 
  mutate(control = str_extract(filename, "control_[:alpha:]*")) %>% 
  mutate(control = str_replace(string = control, pattern = "control_",replacement = "")) %>% 
  dplyr::select(control, group, R2 = Estimate)

fns <- list.files("output/results/", pattern = ".*diff_means.*", full.names = T)
fns <- fns[!str_detect(fns, "drop")]

df_fit_diff_means <- combine_tsvs(fns = fns)  %>% 
  mutate(group = ifelse(str_detect(filename, "medium"), "medium", "SBZR")) %>% 
  mutate(group = ifelse(str_detect(filename, "medium"), "medium", "SBZR")) %>% 
  mutate(control = str_extract(filename, "control_[:alpha:]*")) %>% 
  mutate(control = str_replace(string = control, pattern = "control_",replacement = "")) %>% 
  dplyr::select(control, group, `Absolute diff` = diff_poisson_abs)
  
joined <- df_fit_loo %>% 
  full_join(df_fit_waic,by = join_by(control, group)) %>% 
  full_join(df_fit_R2,by = join_by(control, group)) %>% 
  full_join(df_fit_diff_means, by = join_by(control, group))

joined_SBZR <- joined %>% 
  filter(group == "SBZR")

joined_medium <- joined %>% 
  filter(group == "medium")

png("output/plots/splom_model_fit_medium.png", width = 10, height = 10, res = 300, units = "cm")
joined_medium %>% 
  dplyr::select(-group, -control) %>% 
    SPLOM_hedders()
x <- dev.off()

png("output/plots/splom_model_fit_SBZR.png", width = 10, height = 10, res = 300, units = "cm")

joined_SBZR %>% 
  dplyr::select(-group, -control) %>% 
  SPLOM_hedders()

x <- dev.off()


####LaTeX tables
fn_out = "../latex/model_fit_score_table_SBZR.tex"
cap <- "Comparison of model fit scores of models with different controls for spatial and/or phylogenetic non-independence. Overnight-sailing island groups."
lbl <- "model_fit_score_table_SBZR"
align <- c("r","p{2cm}","p{2cm}","p{2cm}", "p{2cm}",  "p{2cm}") 

joined_SBZR %>%   
  dplyr::select(-group) %>% 
  xtable(caption = cap, label = lbl,
         digits = 3, 
         align = align) %>% 
  xtable::print.xtable(file = fn_out, 
                       sanitize.colnames.function = function(x){x},
                       sanitize.text.function = function(x){x},
                       include.rownames = FALSE, math.style.negative = F,
                       booktabs = TRUE, floating = F, tabular.environment = "longtable") 

fn_out = "../latex/model_fit_score_table_medium.tex"
cap <- "Comparison of model fit scores of models with different controls for spatial and/or phylogenetic non-independence. Shared language island groups."
lbl <- "model_fit_score_table_SBZR"
align <- c("r","p{2cm}","p{2cm}","p{2cm}", "p{2cm}",  "p{2cm}") 

joined_medium %>% 
  dplyr::select(-group) %>% 
  xtable(caption = cap, label = lbl,
         digits = 3, 
         align = align) %>% 
  xtable::print.xtable(file = fn_out, 
                       sanitize.colnames.function = function(x){x},
                       sanitize.text.function = function(x){x},
                       include.rownames = FALSE, math.style.negative = F,
                       booktabs = TRUE, floating = F, tabular.environment = "longtable") 



best_control_SBZR <- joined_SBZR[which.min(joined_SBZR$WAIC),][1,1] %>% as.character()
best_control_medium <- joined_medium[which.min(joined_medium$WAIC),][1,1]  %>% as.character()


cat(paste0("For the SBZR-island grouping, the best performing model (lowest WAIC) is the one with control for: ", best_control_SBZR, ".\n"))


cat(paste0("For the shared language-island grouping, the best performing model (lowest WAIC) is the one with control for: ", best_control_medium, ".\n"))


