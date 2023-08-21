source("01_requirements.R")

medium_effects <- read_tsv("output/results/brms_medium_full_effects_table.tsv", show_col_types = F) %>% 
  filter(variable == "Estimate"|
          variable == "Est.Error"|
           variable == "l-95% CI"|
           variable == "u-95% CI"|
           variable == "straddle_zero_95"|
           variable == "Rhat"|
           variable == "Bulk_ESS" |
           variable == "Tail_ESS") %>% 
  reshape2::dcast(term ~ variable, value.var = "value") %>% 
  mutate(term = str_replace_all(term, "Settlement_date_grouping_finer", "Time depth")) %>% 
  rename(`Does 95% interval straddle zero?`= straddle_zero_95) %>% 
  mutate(Estimate = as.numeric(Estimate)) %>% 
  mutate(Est.Error = as.numeric(Est.Error)) %>% 
  mutate(`l-95% CI` = as.numeric(`l-95% CI`)) %>% 
  mutate(`u-95% CI` = as.numeric(`u-95% CI`)) %>% 
  mutate(Rhat = as.numeric(Rhat)) %>% 
  mutate(Bulk_ESS = as.numeric(Bulk_ESS)) %>% 
  mutate(Tail_ESS = as.numeric(Tail_ESS)) %>% 
  mutate_if(.predicate = is.numeric, .funs = function(x){round(x, 3)}) %>% 
  dplyr::select(term, Estimate, `Est.Error`, `l-95% CI`, `u-95% CI`, `Does 95% interval straddle zero?`, Bulk_ESS, Tail_ESS)

colnames(medium_effects) <- str_replace_all(colnames(medium_effects), "\\_", " ")
colnames(medium_effects) <- str_replace_all(colnames(medium_effects), "\\%", "\\\\%")
colnames(medium_effects) <- str_replace_all(colnames(medium_effects), "\\:", "\\: ")


#xtable printing

cap <- "Table of BRMS model outcomes, shared-language island groups (all observations included)."
lbl <- "BRMS_effects_medium"
align <- c("r","p{5cm}","p{2cm}","p{2cm}", "p{2cm}", "p{2cm}", "p{2cm}", "p{2cm}", "p{2cm}") 

fn <- "../latex/BRMS_effects_medium.tex"

medium_effects %>% 
  xtable(caption = cap, label = lbl,
         digits = 3, 
         align = align) %>% 
  xtable::print.xtable(file = fn, sanitize.colnames.function = function(x){x},
                       include.rownames = FALSE, math.style.negative = F,
                       booktabs = TRUE, floating = F) 

