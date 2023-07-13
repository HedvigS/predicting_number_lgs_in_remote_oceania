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
      prior = c(prior(normal(1, 150), class = Intercept), prior(normal(0, 1), class = b)),
      iter = 3000, 
      warmup = 1000, 
      chains = 4, 
      save_pars = save_pars(all = T),
      cores = 4,
      seed = 10,
backend="cmdstanr") 

#predict plot
predict_df <- data.frame(predicted =  predict(output),
                         group = data$Marck_group, 
                         lg_count = data$lg_count) %>% 
  mutate(min = predicted.Estimate - predicted.Est.Error) %>% 
  mutate(max = predicted.Estimate + predicted.Est.Error)

predict_df$group <- fct_reorder(predict_df$group, predict_df$lg_count)

predict_df %>% 
  ggplot() +
  geom_errorbar(aes(y = group, xmin = min, xmax = max), color = "#6DCD59FF") +
  geom_point(aes(y = group, x = predicted.Estimate), color = "#1F9E89FF", shape = 17) +
  geom_point(aes(y = group, x = lg_count), color = "#440154FF", shape = 16, alpha = 0.9) +
  ggthemes::theme_fivethirtyeight() +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125)) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"))
  
ggsave(filename = "output/plots/brms_predict_marck.png", height = 8, width = 6)  




### model output

summary(output)

output$

