source("01_requirements.R")

compare_effects <- function(fn, simple = F, interaction = F){

  df <- read_tsv(fn, show_col_types = F)
  
  df$bsp_moEA033 <- abs(df$bsp_moEA033)
  df$`bsp_moEA033:Shoreline` <- abs(df$`bsp_moEA033:Shoreline`)
  df$bsp_moSettlement_date_grouping_finer <- abs(df$bsp_moSettlement_date_grouping_finer) 
  df$`bsp_moSettlement_date_grouping_finer:Shoreline` <- abs(df$`bsp_moSettlement_date_grouping_finer:Shoreline`)

simple_comparison <- mean(df$bsp_moEA033 >  df$bsp_moSettlement_date_grouping_finer )
interaction_comparison <- mean(df$`bsp_moEA033:Shoreline` > df$`bsp_moSettlement_date_grouping_finer:Shoreline`)

if(simple == T & interaction == F){
 output <- list(simple_comparison = simple_comparison)}

if(simple == F & interaction == T){
  output <-list(interaction_comparison = interaction_comparison)}

if(simple == T & interaction == T){
  output <-  list(simple_comparison = simple_comparison, interaction_comparison = interaction_comparison)}
output
}


compare_effects(fn = "output/results/brms_medium_control_none_full_chains.tsv", simple = T, interaction = T) #SI
compare_effects(fn =  "output/results/brms_medium_control_phylo_full_chains.tsv", simple = F, interaction = T) #I
compare_effects(fn =  "output/results/brms_medium_control_spatial_full_chains.tsv", simple = T) #S
compare_effects(fn =   "output/results/brms_SBZR_control_none_full_chains.tsv", interaction = T)  #I
compare_effects(fn =   "output/results/brms_SBZR_control_spatial_full_chains.tsv", interaction = T)  #I
