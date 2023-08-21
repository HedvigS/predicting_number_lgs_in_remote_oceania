source("01_requirements.R")
source("fun_def_make_brms_appendix_xtable.R")

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
