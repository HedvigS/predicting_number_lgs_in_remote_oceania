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
# island groups
subregions <- read_tsv( "output/processed_data/subregions.tsv", show_col_types = F) %>% 
  distinct(`Island group (overnight-sailing)` = SBZR_group, `Island group (shared language)` = Medium_only_merged_for_shared_language, Smallest_Island_group, glottocode = Glottocode_spec)

# refs
pol_complex_refs <- read_tsv("data/Remote_oceania_pol_complex.tsv", na = "", show_col_types = F)  %>% 
  dplyr::select(`Political complexity (EA033)`, glottocode, citekey)

# rename("$$\\textbf{\\parbox{2cm}{\\raggedright Method}}$$" = "Method") %>% 
pol_complex_refs$citekey <- paste0("\\citet{",pol_complex_refs$citekey, "}")

#table itself
pol_complex <- readODS::read_ods("data/Remote_oceania_pol_complex_hedvig_code_latex.ods", sheet = 1) %>%
  left_join(pol_complex_refs, 
            relationship = "many-to-many",
            by = join_by(`Political complexity (EA033)`, glottocode)) %>% 
  dplyr::select(glottocodes = glottocode, `Political complexity (EA033)`, citekey) %>% 
  distinct() %>% 
  mutate(glottocodes = ifelse(glottocodes == "fiji1243", "fiji1243,kada1285,sout2864,nort2843", glottocodes)) %>% 
  mutate(glottocodes = ifelse(glottocodes == "aust1304",  "aust1304,raiv1237,tubu1240,ruru1237", glottocodes)) %>% 
  mutate(glottocodes = ifelse(glottocodes == "maor1246", "maor1246,mori1267", glottocodes)) %>% 
  mutate(glottocodes = str_split(glottocodes, ",")) %>% 
  unnest(cols = c(glottocodes)) %>% 
  rename(glottocode = glottocodes) %>% 
  left_join(subregions, by = "glottocode", relationship = "many-to-many") %>% 
  filter(!is.na(`Island group (shared language)`)) %>% 
  group_by(`Island group (overnight-sailing)`, `Island group (shared language)`, `Political complexity (EA033)`) %>% 
  summarise(glottocodes = paste0(unique(glottocode), collapse = ", "),
            References = paste0(unique(citekey), collapse = ", "), .groups = "drop") %>% 
  distinct(`Island group (overnight-sailing)`, `Island group (shared language)`, `Political complexity (EA033)`, glottocodes, References)
  
#write xtable
fn_out = "../latex/appendix_pol_complex_xtable.tex"
cap <- "Table of political complexity values (EA033)."
lbl <- "appendix_pol_complex_xtable"
align <- c("r","p{4.5cm}","p{2cm}","p{2cm}", "p{4cm}", "p{4cm}") 

pol_complex %>% 
  arrange(`Island group (overnight-sailing)`) %>% 
  write_tsv("output/processed_data/appendix_pol_complex.tsv")

pol_complex %>% 
  arrange(`Island group (overnight-sailing)`) %>% 
xtable(caption = cap, label = lbl,
       digits = 0, 
       align = align) %>% 
  xtable::print.xtable(file = fn_out, 
                       sanitize.colnames.function = function(x){x},
                       sanitize.text.function = function(x){x},
                       include.rownames = FALSE, math.style.negative = F,tabular.environment = "longtable",
                       booktabs = TRUE, floating = F) 
  

#model input

## SBZR
fn_out = "../latex/appendix_SBZR_group_table.tex"
cap <- "Table of input values to model, overnight-sailing island groups."
lbl <- "appendix_SBZR_table"
align <- c("r","p{4.5cm}","p{1.4cm}","p{1.4cm}", "p{1.4cm}", "p{1.4cm}",  "p{1.7cm}","p{1.7cm}") 

read_tsv("output/processed_data/RO_Hedvig_aggregate_SBZR_group_scaled.tsv", show_col_types = F) %>% 
  mutate(lg_count = as.character(lg_count)) %>% 
  dplyr::select(`Island group (overnight-sailing)` = SBZR_group,
                `Lang count` = lg_count,
                Shoreline, 
                `environ PC1` = environ_PC1,
                `environ PC2` = environ_PC2,
                `Political complexity (EA033)` = EA033, 
                `Time depth` = Settlement_date_grouping_finer) %>% 
  xtable(caption = cap, label = lbl,
         digits = 3, 
         align = align) %>% 
  xtable::print.xtable(file = fn_out, 
                       sanitize.colnames.function = function(x){x},
                       sanitize.text.function = function(x){x},
                       include.rownames = FALSE, math.style.negative = F,tabular.environment = "longtable",
                       booktabs = TRUE, floating = F) 


## medium
fn_out = "../latex/appendix_medium_group_table.tex"
cap <- "Table of input values to model, shared language island groups."
lbl <- "appendix_medium_table"
align <- c("r","p{4.5cm}","p{1.4cm}","p{1.4cm}", "p{1.4cm}", "p{1.4cm}",  "p{1.7cm}","p{1.7cm}") 

read_tsv("output/processed_data/RO_Hedvig_aggregate_medium_group_scaled.tsv", show_col_types = F) %>% 
  mutate(lg_count = as.character(lg_count)) %>% 
  dplyr::select(`Island group (shared language)` = Medium_only_merged_for_shared_language,
              `Lang count` = lg_count,
              Shoreline, 
              `environ PC1` = environ_PC1,
              `environ PC2` = environ_PC2,
              `environ PC3` = environ_PC3,
              `Political complexity (EA033)` = EA033, 
              `Time depth` = Settlement_date_grouping_finer) %>% 
  xtable(caption = cap, label = lbl,
         digits = 3, 
         align = align) %>% 
  xtable::print.xtable(file = fn_out, 
                       sanitize.colnames.function = function(x){x},
                       sanitize.text.function = function(x){x},
                       include.rownames = FALSE, math.style.negative = F,tabular.environment = "longtable",
                       booktabs = TRUE, floating = F) 



##

dates <- read_tsv("data/island_group_settlement_date.tsv", show_col_types = F)  %>% 
  inner_join(subregions, by = "Smallest_Island_group") %>% 
  dplyr::select("Time depth settlement group",
                `Island group (finest)` = "Smallest_Island_group", 
              `Island group (overnight-sailing)`, 
              `Island group (shared language)`,
              `Oldest date`,
              `Name in source`,
              `Date ranges`,
              `Based on inference from neighbouring island?`,
              Source, Meta_source, 
              Comment
              )  %>% 
distinct() %>% 
  group_by( `Island group (finest)`, `Date ranges`) %>% 
  summarise(`Island group (overnight-sailing)` = paste0(unique(`Island group (overnight-sailing)`), collapse= ", "),
            `Island group (shared language)` = paste0(unique(`Island group (shared language)`), collapse= ", "),
            `Time depth settlement group` = paste0(unique(`Time depth settlement group`), collapse= ", "),
                        `Based on inference from neighbouring island?` = paste0(unique(`Based on inference from neighbouring island?`), collapse = ", "),
            `Date ranges` = paste0(unique(`Date ranges`), collapse= ", "),
            `Name in source` = paste0(unique(`Name in source`), collapse = ", "),
            `Source` = paste0(unique(`Source`), collapse = ", "),
            `Source (meta)` = paste0(unique(`Meta_source`), collapse = ", "),
            .groups = "drop"
  ) %>% 
  mutate(`Time depth settlement group` = as.numeric(`Time depth settlement group`)) %>% 
           arrange(`Time depth settlement group`) %>% 
  mutate_all(.funs = ~str_replace(.,"NA", ""))
  
dates %>% 
  write_tsv("output/processed_data/appendix_dates.tsv")

#write xtable
fn_out = "../latex/appendix_dates_xtable.tex"
cap <- "Table of settlement time depth."
lbl <- "appendix_dates_xtable"
align <- c("r","p{2cm}","p{2cm}","p{2cm}", "p{2cm}", "p{2cm}", "p{2cm}", "p{2cm}", "p{2cm}", "p{2cm}") 


dates %>% 
xtable(caption = cap, label = lbl,
       digits = 0, 
       align = align) %>% 
  xtable::print.xtable(file = fn_out, 
                       sanitize.colnames.function = function(x){x},
                       sanitize.text.function = function(x){x},
                       include.rownames = FALSE, math.style.negative = F,tabular.environment = "longtable",
                       booktabs = TRUE, floating = F) 

