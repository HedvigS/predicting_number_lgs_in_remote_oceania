source("01_requirements.R")
source("01_requirements_brms.R")

data <- read_tsv("output/processed_data/RO_Hedvig_aggregate_marck_group_scaled.tsv", show_col_types = F) %>%  
  mutate("Carrying_capactiy_PC1_Shoreline" = Carrying_capactiy_PC1*Shoreline*EA033) %>% 
  mutate("Carrying_capactiy_PC2_Shoreline" = Carrying_capactiy_PC2*Shoreline*EA033)

  #inspiried by
#https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/counting-and-classification.html#binomial-regression

output_poission <-  brm(data = data, 
               family = poisson,
      lg_count  ~ Carrying_capactiy_PC1 + 
                  Carrying_capactiy_PC2 +
                  EA033 + 
                  Shoreline +
                  Settlement_date_grouping_finer,
      iter = 30000, 
      warmup = 1000, 
      chains = 4, 
      save_pars = save_pars(all = T),
      cores = 4,
      seed = 10,
backend="cmdstanr") 

summary(output_poission)

plot(output_poission)

brms::posterior_predict(output_poission, ndraws = 2000) %>% 
  as.data.frame() %>% 
  .[23,20]

output_poission$


#predict plot
predict_df <- data.frame(predicted_poission =  predict(output_poission),
                         group = data$Marck_group, 
                         lg_count = data$lg_count) %>% 
  mutate(min_poission = predicted_poission.Estimate - predicted_poission.Est.Error) %>% 
  mutate(max_poission = predicted_poission.Estimate + predicted_poission.Est.Error)

predict_df$predicted_poission.Estimate[1]

predict_df$group <- fct_reorder(predict_df$group, predict_df$lg_count)

predict_df %>% 
  ggplot() +
  geom_errorbar(aes(y = group, xmin = min_poission, xmax = max_poission), color = "#6DCD59FF") +
  geom_point(aes(y = group, x = predicted_poission.Estimate), color = "#1F9E89FF", shape = 17) +
#  geom_errorbar(aes(y = group, xmin = min_negbin, xmax = max_negbin), color = "#FDE725FF") +
#  geom_point(aes(y = group, x = predicted_negbin.Estimate), color = "#DCE318FF", shape = 15) +
  geom_point(aes(y = group, x = lg_count), color = "black", fill = "#440154FF", 
             shape = 21, alpha = 0.6, stroke = 0.4, 
             size = 1) +
  ggthemes::theme_fivethirtyeight() +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125)) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"))

predict_df <- predict_df %>% 
  mutate(diff_poission = abs(lg_count - predicted_poission.Estimate)) 

predict_df$diff_poission %>% mean()

ggsave(filename = "output/plots/brms_predict_marck.png", height = 8, width = 6)  

#rstan::plot(output)

### model output

chain_1 <- output_poission$fit@sim$samples[[1]] %>% as.data.frame()  %>% mutate(chain = "1")
chain_2 <- output_poission$fit@sim$samples[[2]] %>% as.data.frame()  %>% mutate(chain = "2")
chain_3 <- output_poission$fit@sim$samples[[3]] %>% as.data.frame()  %>% mutate(chain = "3")
chain_4 <- output_poission$fit@sim$samples[[4]] %>% as.data.frame()  %>% mutate(chain = "4")

chain_joined <- full_join(chain_1, chain_2) %>% full_join(chain_3) %>% full_join(chain_4)

for(n in 1:58){

#  n <- 13

model_estimate <- predict_df[n,]$predicted_poission.Estimate

data_chopped <- data %>% 
    mutate_if(is.numeric, .funs = function(x){floor(x*100)/100})

manual_estimate <- exp((mean(data_chopped[n,]$Carrying_capactiy_PC1 * chain_joined$b_Carrying_capactiy_PC1)
 +
       mean(data_chopped[n,]$Carrying_capactiy_PC2 * chain_joined$b_Carrying_capactiy_PC2)
 +
       mean(data_chopped[n,]$Shoreline * chain_joined$b_Shoreline)
 +
       mean(data_chopped[n,]$EA033 * chain_joined$b_EA033)
 +
       mean(data_chopped[n,]$Settlement_date_grouping_finer * chain_joined$b_Settlement_date_grouping_finer)) +
   mean(chain_joined$b_Intercept)
)

print(manual_estimate-model_estimate)

}

