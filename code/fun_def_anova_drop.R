source("01_requirements.R")


fun_anova_pick_drop <- function(model) {

   anova_df <- anova(model, test = "Chisq") %>% 
    rownames_to_column("variable") %>% 
    mutate(interactions = str_count(variable, ":") + 1) %>% 
    arrange(-interactions, -`Pr(>Chi)`) %>% 
    mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig"))
  
   to_drop_df <- anova_df %>% 
     slice_max(order_by = `Pr(>Chi)`) 
   
#if there's more than one to kick out, kick out the one that is an interaction
if(nrow(to_drop_df) != 1 & filter(to_drop_df, interactions == 2) %>% nrow()){
  
  to_drop_var <-   filter(to_drop_df, interactions == 2) %>% 
    .$variable
}

#if there aren't interactions, randomly pick one of the same valued ones
if(nrow(to_drop_df) != 1 & (filter(to_drop_df, interactions == 2) %>% nrow() < 1)){
  
  to_drop_var <-   to_drop_df %>% 
    sample_n(size = 1) %>% 
    .$variable
}

to_drop_var
}

   