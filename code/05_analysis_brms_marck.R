source("01_requirements.R")
source("01_requirements_brms.R")

data <- read_tsv("output/processed_data/RO_Hedvig_aggregate_marck_group_scaled.tsv", show_col_types = F) 
data$Settlement_date_grouping_finer <- abs(data$Settlement_date_grouping_finer - 2 ) + 1


#inspiried by
#https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/counting-and-classification.html#binomial-regression

output_poission <-  brm(data = data, 
               family = poisson,
      lg_count  ~ Carrying_capactiy_PC1 + 
        Carrying_capactiy_PC2 +
                  EA033 +
                  Isolation + 
                  Shoreline * Settlement_date_grouping_finer,
      iter = 3000, 
      warmup = 1000, 
      chains = 4, 
      save_pars = save_pars(all = T),
      cores = 4,
      seed = 10,
backend="cmdstanr") 

#predict plot
predict_df <- data.frame(predicted_poission =  predict(output_poission),
                         group = data$Marck_group, 
                         lg_count = data$lg_count) %>% 
  mutate(min_poission = predicted_poission.Estimate - predicted_poission.Est.Error) %>% 
  mutate(max_poission = predicted_poission.Estimate + predicted_poission.Est.Error)

predict_df$group <- fct_reorder(predict_df$group, predict_df$lg_count)

predict_df %>% 
  ggplot() +
  geom_errorbar(aes(y = group, xmin = min_poission, xmax = max_poission), color = "#6DCD59FF") +
  geom_point(aes(y = group, x = predicted_poission.Estimate), color = "#1F9E89FF", shape = 17) +
#  geom_errorbar(aes(y = group, xmin = min_negbin, xmax = max_negbin), color = "#FDE725FF") +
#  geom_point(aes(y = group, x = predicted_negbin.Estimate), color = "#DCE318FF", shape = 15) +
  geom_point(aes(y = group, x = lg_count), color = "#440154FF", shape = 16, alpha = 0.9) +
  ggthemes::theme_fivethirtyeight() +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125)) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"))

predict_df <- predict_df %>% 
  mutate(diff_poission = abs(lg_count - predicted_poission.Estimate)) %>% 
  mutate(diff_negbin = abs(lg_count - predicted_negbin.Estimate))

predict_df$diff_poission %>% mean()
predict_df$diff_negbin %>% mean()

  
ggsave(filename = "output/plots/brms_predict_marck.png", height = 8, width = 6)  

bayesfactor_output <- bayes_factor(x1 = output_poission, x2 = output)


waic(output)

waic(output_poission)






predict_df$group <- fct_reorder2(predict_df$group,  desc(predict_df$predicted.Estimate),desc(predict_df$lg_count))

predict_df %>% 
  ggplot() +
  geom_point(aes(group, predicted.Estimate), color = "red") +
  geom_point(aes(group, lg_count), color = "blue") 
  




rstan::plot(output)

### model output

summary(output)

chain_1 <- output$fit@sim$samples[[1]] %>% as.data.frame()  %>% mutate(chain = "1")
chain_2 <- output$fit@sim$samples[[2]] %>% as.data.frame()  %>% mutate(chain = "2")
chain_3 <- output$fit@sim$samples[[3]] %>% as.data.frame()  %>% mutate(chain = "3")
chain_4 <- output$fit@sim$samples[[4]] %>% as.data.frame()  %>% mutate(chain = "4")

chain_joined <- full_join(chain_1, chain_2) %>% full_join(chain_3) %>% full_join(chain_4)

library("ggridges")

chain_joined %>% 
  ggplot() +
geom_histogram(aes(x = b_Shoreline), bins = 900)

getAnywhere(stan_hist)
