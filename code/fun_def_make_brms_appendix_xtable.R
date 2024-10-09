source("01_requirements.R")

make_brms_appendix_xtable <- function(fn_in,
                                     fn_out,
                                     lbl,
                                     cap
    ){

#fn_in = "output/results/brms_medium_full_effects_table.tsv"
#fn_out = "../latex/BRMS_effects_medium.tex"
#cap <- "Table of BRMS model outcomes, shared-language island groups (all observations included)."
#lbl <- "BRMS_effects_medium"

df <- read_tsv(fn_in, show_col_types = F) %>% 
  filter(variable == "Estimate"|
           variable == "Est.Error"|
           variable == "l-95% CI"|
           variable == "u-95% CI"|
           variable == "Rhat"|
           variable == "Bulk_ESS" |
           variable == "Tail_ESS") %>% 
  reshape2::dcast(term ~ variable, value.var = "value") %>% 
  mutate(term = str_replace_all(term,"moEA", "EA")) %>% 
  mutate(term = str_replace_all(term, "moSettlement_date_grouping_finer", "Time depth")) %>% 
  mutate(Estimate = as.numeric(Estimate)) %>% 
  mutate(Est.Error = as.numeric(Est.Error)) %>% 
  mutate(`l-95% CI` = as.numeric(`l-95% CI`)) %>% 
  mutate(`u-95% CI` = as.numeric(`u-95% CI`)) %>% 
  mutate(Rhat = as.numeric(Rhat)) %>% 
  mutate(Bulk_ESS = as.numeric(Bulk_ESS)) %>% 
  mutate(Tail_ESS = as.numeric(Tail_ESS)) %>% 
  mutate_if(.predicate = is.numeric, .funs = function(x){round(x, 3)}) %>% 
  dplyr::select(term, Estimate, `Est.Error`, `l-95% CI`, `u-95% CI`, Bulk_ESS, Tail_ESS, Rhat)

colnames(df) <- str_replace_all(colnames(df), "\\_", " ")
colnames(df) <- str_replace_all(colnames(df), "\\%", "\\\\%")
colnames(df) <- str_replace_all(colnames(df), "\\:", "\\: ")

align <- c("r","p{3cm}","p{1.35cm}","p{1.35cm}", "p{1.35cm}", "p{1.35cm}", "p{1.35cm}", "p{1.35cm}", "p{1.35cm}") 

df %>% 
  xtable(caption = cap, label = lbl,
         digits = 2, 
         align = align) %>% 
  xtable::print.xtable(file = fn_out, sanitize.colnames.function = function(x){x},
                       include.rownames = FALSE, math.style.negative = F,
                       booktabs = TRUE, floating = T) 

}