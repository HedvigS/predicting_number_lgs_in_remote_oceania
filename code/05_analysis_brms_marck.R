source("01_requirements.R")
source("01_requirements_brms.R")

data <- read_tsv("output/processed_data/RO_Hedvig_aggregate_marck_group_scaled.tsv", show_col_types = F) 


#inspiried by
#https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/counting-and-classification.html#binomial-regression

output <-  brm(data = data, 
               family = negbinomial,
      lg_count  ~ Carrying_capactiy_PC1 + 
        Carrying_capactiy_PC2 +
                  EA033 +
                  Isolation + 
                  Shoreline * Settlement_date_grouping_finer,
      prior = c(prior(normal(0, 100), class = Intercept), prior(normal(0, 1), class = b)),
      iter = 3000, 
      warmup = 1000, 
      chains = 4, 
      save_pars = save_pars(all = T),
      cores = 4,
      seed = 10,
backend="cmdstanr") 

summary(output)

predict_df <- data.frame(predicted =  predict(output),
                         group = data$Marck_group) %>% 
  mutate(min = predicted.Estimate - predicted.Est.Error) %>% 
  mutate(max = predicted.Estimate + predicted.Est.Error)


