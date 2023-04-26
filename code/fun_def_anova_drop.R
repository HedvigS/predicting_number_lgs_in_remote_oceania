source("01_requirements.R")

#We are fitting the model by pruning the factors in a stepwise fashion with an ANOVA Chi-square test. The term that is the least significant in the ANOVA will be dropped, a new model rendered and another ANOVA until all the terms left are significant in the ANOVA. Significant here means passuing the p-value < 0.05 treshold. The term with the highest p value is dropped each time. If the term that has the highest p value also participates in an interaction that is significant, it is not dropped, the term with the second highest p value is dropped instead (if it does not also participate in an interaction that is significant, and so on).

fun_anova_pick_drop <- function(model) {
#model <- full_model

anova_df <-  anova(model, test = "Chisq") %>% 
    rownames_to_column("variable") %>% 
    mutate(interactions = str_count(variable, ":") + 1) %>% 
    arrange(-interactions, -`Pr(>Chi)`) %>% 
    mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig"))

n_non_sig <- anova_df %>% 
  filter(sig == "Non Sig") %>%  nrow()
 
if(n_non_sig == 0){
  cat(paste0("All variables meet the p-value < 0.05 treshold in the anova, no more pruning necessary.\n"))
  
  to_drop_var <- NULL
  }else{
 
    warning("There are variables which don't meet the p-value < 0.05 treshold in the anova, pruning necessary.\n")

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
  }

sig_vec <- anova_df %>% 
  filter(sig == "Sig") %>% 
  dplyr::select(variable) %>% 
  as.matrix() %>% 
  as.vector() %>% 
  paste(collapse = ",")

if(to_drop_var %in% sig_vec){
  warning("The variable with the highest p-value is involved in an interaction!!! Manual thinking needed. Sorry!")
  to_drop_var <- NULL
}

to_drop_var 
}

   