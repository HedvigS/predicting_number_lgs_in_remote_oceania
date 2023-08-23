source("01_requirements.R")
source("fun_def_make_brms_appendix_xtable.R")


#BRMS tables
make_brms_appendix_xtable(fn_in = "output/results/brms_medium_full_effects_table.tsv", 
                          fn_out = "../latex/BRMS_effects_medium.tex",
                          cap = "Table of BRMS model outcomes, shared-language island groups (all observations included).",
                          lbl = "BRMS_effects_medium"
 )

make_brms_appendix_xtable(fn_in = "output/results/brms_SBZR_full_effects_table.tsv", 
                          fn_out = "../latex/BRMS_effects_SBZR.tex",
                          cap = "Table of BRMS model outcomes, overnight-distance island groups (all observations included).",
                          lbl = "BRMS_effects_SBZR"
)

#pol complex table
subregions <- read_tsv( "output/processed_data/subregions.tsv", show_col_types = F) %>% 
  distinct(`Island group (overnight-sailing)` = SBZR_group, `Island group (shared language)` = Medium_only_merged_for_shared_language, Smallest_Island_group, glottocode = Glottocode_spec)

pol_complex <- readODS::read_ods("data/Remote_oceania_pol_complex_hedvig_code_latex.ods", sheet = 1) %>%
  dplyr::select(glottocodes = glottocode, `Political complexity (EA033)`, Reference) %>% 
  mutate(glottocodes = ifelse(glottocodes == "fiji1243", "fiji1243,kada1285,sout2864,nort2843", glottocodes)) %>% 
  mutate(glottocodes = ifelse(glottocodes == "aust1304",  "aust1304,raiv1237,tubu1240,ruru1237", glottocodes)) %>% 
  mutate(glottocodes = ifelse(glottocodes == "maor1246", "maor1246,mori1267", glottocodes)) %>% 
  mutate(glottocodes = str_split(glottocodes, ",")) %>% 
  unnest(cols = c(glottocodes)) %>% 
  rename(glottocode = glottocodes) %>% 
  left_join(subregions, by = "glottocode") %>% 
  filter(!is.na(`Island group (shared language)`)) %>% 
  group_by(`Island group (overnight-sailing)`, `Island group (shared language)`, `Political complexity (EA033)`) %>% 
  summarise(glottocodes = paste0(glottocode, collapse = ", "), 
            references = paste0(Reference, collapse = "%"), .groups = "drop") %>% 
  mutate(references = str_split(references, "%")) %>% 
  unnest(cols = c(references)) %>% 
  distinct() %>% 
  group_by(`Island group (overnight-sailing)`) %>% 
  mutate(references = paste0(references, collapse = "; "), .groups = "drop") %>% 
  distinct(`Island group (overnight-sailing)`, `Island group (shared language)`, `Political complexity (EA033)`, glottocodes, references)
  

fn_out = "../latex/appendix_pol_complex_xtable.tex"
cap <- "Table of political complexity values (EA033)."
lbl <- "appendix_pol_complex_xtable"
align <- c("r","p{1.8cm}","p{1.8cm}","p{1.8cm}", "p{2cm}", "p{7cm}") 

pol_complex %>% 
  arrange(`Island group (overnight-sailing)`) %>% 
xtable(caption = cap, label = lbl,
       digits = 3, 
       align = align) %>% 
  xtable::print.xtable(file = fn_out, sanitize.colnames.function = function(x){x},
                       include.rownames = FALSE, math.style.negative = F,tabular.environment = "longtable",
                       booktabs = TRUE, floating = F) 
  