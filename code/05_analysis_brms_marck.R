source("01_requirements.R")
source("01_requirements_brms.R")

data <- read_tsv("output/processed_data/RO_Hedvig_aggregate_marck_group_scaled.tsv", show_col_types = F) %>%  
  mutate("Combined_CCPC1_Size_EA033" = Carrying_capactiy_PC1*Shoreline*EA033) %>% 
  mutate("Combined_CCPC2_Size_EA033" = Carrying_capactiy_PC2*Shoreline*EA033)

#inspired by
#https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/counting-and-classification.html#binomial-regression

#model specs
formula <- "lg_count  ~ Carrying_capactiy_PC1 + 
  Carrying_capactiy_PC2 +
  EA033 + 
  Shoreline +
  Settlement_date_grouping_finer"

iter = 30000
warmup = 1000
chains = 4
save_pars = save_pars(all = T)
cores = 4
seed = 10

############# ALL OBSERVATIONS ####################

output_poission <-  brm(data = data, 
               family = poisson,
               formula = formula,
      iter = iter, 
      warmup = warmup, 
      chains = chains, 
      silent = 2,
      save_pars = save_pars,
      cores = cores,
      seed = seed,
backend="cmdstanr") 

#summary(output_poission)

posterior_predict_df <- brms::posterior_predict(output_poission, cores = cores, ndraws = 10000) %>%
  as.data.frame() %>% 
  data.table::transpose() %>% 
  mutate(Marck_group = data$Marck_group) %>% 
  reshape2::melt(id.vars = "Marck_group") %>% 
  group_by(Marck_group) %>% 
  mutate(mean = mean(value), 
            sd = sd(value),
            min = min(value),
            max = max(value)) %>% 
  full_join(data, by = "Marck_group") %>% 
  mutate(diff_poission = abs(lg_count - mean)) 

#predict plot

posterior_predict_df$Marck_group <- fct_reorder(posterior_predict_df$Marck_group, posterior_predict_df$lg_count)


c("#440154FF" ,"#481769FF" ,"#472A7AFF" ,"#433D84FF" ,"#3D4E8AFF" ,"#355E8DFF", "#2E6D8EFF",
 "#297B8EFF", "#23898EFF" ,"#1F978BFF", "#21A585FF", "#2EB37CFF" ,"#46C06FFF" ,"#65CB5EFF",
 "#89D548FF" ,"#B0DD2FFF", "#D8E219FF" ,"#FDE725FF")

p <- posterior_predict_df %>% 
  ggplot() +
  geom_boxplot(mapping = aes(y = Marck_group, x = value), color = "#2EB37CFF", fill = "#65CB5EFF", alpha = 0.2) +
  geom_point(aes(y = Marck_group, x = lg_count),
             fill = "#355E8DFF", color = "#481769FF",
             shape =24, alpha = 0.2, stroke = 0.6, 
             size = 1.6) +
  ggthemes::theme_fivethirtyeight() +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125)) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"))

ggsave(plot = p, filename = "output/plots/brms_predict_marck.png", height = 8, width = 6)  

cat(paste0("The mean differences between the predicted and observed numbers of languages is ", posterior_predict_df$diff_poission %>% mean() %>% round(2), ".\n"))

### model output
chain_1 <- output_poission$fit@sim$samples[[1]] %>% as.data.frame()  %>% mutate(chain = "1")
chain_2 <- output_poission$fit@sim$samples[[2]] %>% as.data.frame()  %>% mutate(chain = "2")
chain_3 <- output_poission$fit@sim$samples[[3]] %>% as.data.frame()  %>% mutate(chain = "3")
chain_4 <- output_poission$fit@sim$samples[[4]] %>% as.data.frame()  %>% mutate(chain = "4")

chain_joined <- full_join(chain_1, chain_2, by = join_by(b_Intercept, b_Carrying_capactiy_PC1, b_Carrying_capactiy_PC2,
                                                         b_EA033, b_Shoreline, b_Settlement_date_grouping_finer, Intercept, lprior, lp__, chain)) %>% 
  full_join(chain_3, by = join_by(b_Intercept, b_Carrying_capactiy_PC1, b_Carrying_capactiy_PC2,
                                  b_EA033, b_Shoreline, b_Settlement_date_grouping_finer, Intercept, lprior, lp__, chain)) %>% 
  full_join(chain_4, by = join_by(b_Intercept, b_Carrying_capactiy_PC1, b_Carrying_capactiy_PC2,
                                  b_EA033, b_Shoreline, b_Settlement_date_grouping_finer, Intercept, lprior, lp__, chain))

chain_joined %>% 
  reshape2::melt(id.vars = "chain") %>% 
  filter(variable != "lprior") %>% 
  filter(variable != "lp__") %>% 
  filter(variable != "Intercept") %>% 
  ggplot(aes(x = value, fill = variable, 
             color = variable,
                     y = ..density..)) + 
  geom_density(  alpha = 0.8,
                 color = "darkgray",
                 linewidth = 0.8, adjust = 0.7
               ) +
  lemon::facet_rep_wrap(~variable, 
             #ncol = 3, 
#             scales = "free",
             repeat.tick.labels = c('bottom')) +
  geom_vline(aes(xintercept = 0), linetype="dashed", color = "darkgray", alpha = 0.7) +
  theme_light() +
  theme(legend.position = "none", 
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank()) +
  scale_color_manual(values = RColorBrewer::brewer.pal(name = "Set3", n = 9)) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(name = "Set3", n = 9))

########### KICKING OUT ONE OBSERVATION AT A TIME

obs <- data$Marck_group


for(ob in obs){
  
#  ob <- obs[1]

  cat(paste0("Dropping out ", ob, ".\n"))
  
data_spec <-   data %>% 
    filter(Marck_group != {{ob}})

output_spec <-  brm(data = data_spec, 
                    family = poisson,
                    formula = formula,
                    iter = iter, 
                    silent = 2,
                    refresh = 0,
                    warmup = warmup, 
                    chains = chains, 
                    save_pars = save_pars,
                    cores = cores,
                    seed = seed,
                    backend="cmdstanr") 

predict_df_spec <- data.frame(predicted_spec =  predict(output_spec),
                         group = data_spec$Marck_group, 
                         lg_count = data_spec$lg_count)  %>% 
  mutate(diff = abs(lg_count - predicted_spec.Estimate)) 

cat(paste0("The diff was ", mean(predict_df_spec$diff) %>% round(2)
, ".\n"))
  
}




######################################


#calculating the estimate value "manually" from the coef vs what predict() does
for(n in 1:58){

  n <- 13

model_estimate <- predict_df[n,]$predicted_poission.Estimate

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




