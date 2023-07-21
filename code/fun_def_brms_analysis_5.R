
fun_hedvig_brms_predicting <- function(data = NULL, 
                                       formula = NULL, 
                                       group = NULL,
                                       iter = 30000,
                                       warmup = 1000,
                                       chains = 4,
                                       cores = 4,
                                       seed = 10,
                                       ndraws = 10000
){
  
#  iter = 30000
#  warmup = 1000
#  chains = 4
#  cores = 4
#  seed = 10
#  ndraws = 10000
  
  
  
  ############# ALL OBSERVATIONS ####################
  
  output_poission <-  brm(data = data, 
                          family = poisson,
                          formula = formula,
                          iter = iter, 
                          warmup = warmup, 
                          chains = chains, 
                          silent = 2,
                          save_pars = save_pars(all = T),
                          cores = cores,
                          seed = seed,
                          backend="cmdstanr") 
  
  #summary(output_poission)
  
  posterior_predict_df <- brms::posterior_predict(output_poission, cores = cores, ndraws = ndraws) %>%
    as.data.frame() %>% 
    data.table::transpose() %>% 
    mutate(group = data$group) %>% 
    reshape2::melt(id.vars = "group") %>% 
    group_by(group) %>% 
    mutate(mean = mean(value), 
           sd = sd(value),
           min = min(value),
           max = max(value)) %>% 
    full_join(data, by = "group") %>% 
    mutate(diff_poission = abs(lg_count - mean)) 
  
  cat(paste0("The mean differences between the predicted and observed numbers of languages is ", posterior_predict_df$diff_poission %>% mean() %>% round(2), ".\n"))
  
  posterior_predict_df %>% 
    distinct(group, mean, sd, min, max, diff_poission, lg_count) %>% 
    write_tsv(file = paste0("output/results/brms_", group, "_predict_table.tsv"), na = "")
  
  #predict plot
  
  posterior_predict_df$group <- fct_reorder(posterior_predict_df$group, posterior_predict_df$lg_count)
  
#  c("#440154FF" ,"#481769FF" ,"#472A7AFF" ,"#433D84FF" ,"#3D4E8AFF" ,"#355E8DFF", "#2E6D8EFF",
#    "#297B8EFF", "#23898EFF" ,"#1F978BFF", "#21A585FF", "#2EB37CFF" ,"#46C06FFF" ,"#65CB5EFF",
#    "#89D548FF" ,"#B0DD2FFF", "#D8E219FF" ,"#FDE725FF")
  
  p <- posterior_predict_df %>% 
    ggplot() +
    geom_boxplot(mapping = aes(y = group, x = value), color = "#2EB37CFF", fill = "#65CB5EFF", alpha = 0.2) +
    geom_point(aes(y = group, x = lg_count),
               fill = "#355E8DFF", color = "#481769FF",
               shape =24, alpha = 0.2, stroke = 0.6, 
               size = 1.6) +
    ggthemes::theme_fivethirtyeight() +
    scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125)) +
    theme(panel.background = element_rect(fill = "white"), 
          plot.background = element_rect(fill = "white"))
  
  ggsave(plot = p, filename = paste0("output/plots/brms_predict_", group, ".png"), height = 8, width = 6)  
  ggsave(filename = paste0("../latex/brms_predict_", group, ".png"),  height = 8, width = 6) 
  
  
  ### model output
  chain_1 <- output_poission$fit@sim$samples[[1]] %>% as.data.frame()  %>% mutate(chain = "1")
  chain_2 <- output_poission$fit@sim$samples[[2]] %>% as.data.frame()  %>% mutate(chain = "2")
  chain_3 <- output_poission$fit@sim$samples[[3]] %>% as.data.frame()  %>% mutate(chain = "3")
  chain_4 <- output_poission$fit@sim$samples[[4]] %>% as.data.frame()  %>% mutate(chain = "4")
  
  chain_joined <- suppressMessages(full_join(chain_1, chain_2)) %>% 
    suppressMessages(full_join(chain_3)) %>%
    suppressMessages(full_join(chain_4))
  
  chain_joined %>% 
    write_tsv(file = paste0("output/results/brms_", group, "_full_chains.tsv"), na = "")
  
  chain_joined %>%
    reshape2::melt(id.vars = "chain") %>% 
    filter(variable != "lprior") %>% 
    filter(variable != "lp__") %>% 
    filter(variable != "Intercept") %>% 
    ggplot(aes(x = value, fill = variable, 
               color = variable,
               y = after_stat(density))) + 
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
          axis.text.y = element_blank()) #+
#    scale_color_manual(values = RColorBrewer::brewer.pal(name = "Set3", n = 10)) +
#    scale_fill_manual(values = RColorBrewer::brewer.pal(name = "Set3", n = 10))
  
  ggsave(filename = paste0("output/plots/brms_", group, "_group_full_effect_ridge_panels_plot.png"), height = 9, width = 10)
  ggsave(filename = paste0("../latex/brms_", group, "_group_full_effect_ridge_panels_plot.png"),  height = 9, width = 10) 
  
  
  ########### KICKING OUT ONE OBSERVATION AT A TIME
  
  
  df_all <- data.frame(
    "dropped_obs"     = as.character(),
    "b_Carrying_capactiy_PC1_max"         = as.numeric(),          
    "b_Carrying_capactiy_PC1_min"     = as.numeric(),            
    "b_Carrying_capactiy_PC1_mean"      = as.numeric(),             
    "b_Intercept_max"                                = as.numeric(),
    "b_Intercept_min"                                = as.numeric(),
    "b_Intercept_mean"                             = as.numeric(),
    "b_Carrying_capactiy_PC2_max"                    = as.numeric(),
    "b_Carrying_capactiy_PC2_min"                 = as.numeric(),
    "b_Carrying_capactiy_PC2_mean"                   = as.numeric(),
    "b_EA033_max"                            = as.numeric(),
    "b_EA033_min"                                    = as.numeric(),
    "b_EA033_mean"                          = as.numeric(),
    "b_Shoreline_max"                                = as.numeric(),
    "b_Shoreline_min"                     = as.numeric(),
    "b_Shoreline_mean"                               = as.numeric(),
    "b_Settlement_date_grouping_finer_max"      = as.numeric(),
    "b_Settlement_date_grouping_finer_min"           = as.numeric(),
    "b_Settlement_date_grouping_finer_mean"     = as.numeric(),
    "mean_Rhat"                                      = as.numeric(),
    "mean_Bulk_ESS"                         = as.numeric(),
    "mean_Tail_ESS"                         = as.numeric(),
    "diff_predicted_vs_observed"               = as.numeric(),
    "dropped_observation_prediction"                  = as.numeric(),
    "dropped_observation_prediction_diff"      = as.numeric(),
    "b_Carrying_capactiy_PC1_includes_zero"         = as.character(),
    "b_Carrying_capactiy_PC2_includes_zero"  = as.character(),
    "b_Intercept_includes_zero"                     = as.character(),
    "b_EA033_includes_zero"                  = as.character(),
    "b_Shoreline_includes_zero"                     = as.character(),
    "b_Settlement_date_grouping_finer_includes_zero"= as.character()
  )
  
  obs <- c(data$group, NA)
  
  for(obs in obs){
    
    #  obs <- obs[1]
    
    cat(paste0("Dropping out ", obs, ".\n"))
    
    if(is.na(obs)){
      data_spec <-   data 
    }else{data_spec <-   data %>% 
      filter(group != {{obs}})
    }
    output_spec <-  brm(data = data_spec, 
                        family = poisson,
                        formula = formula,
                        iter = iter, 
                        silent = 2,
                        refresh = 0,
                        save_pars = save_pars(all = T),
                        warmup = warmup, 
                        chains = chains, 
                        cores = cores,
                        seed = seed,
                        backend="cmdstanr") 
    
    ms <- summary(output_spec)
    
    #predicting number of lgs
    posterior_predict_df_spec <- brms::posterior_predict(output_spec, cores = cores, ndraws = 1000) %>%
      as.data.frame() %>% 
      data.table::transpose() %>% 
      mutate(group = data_spec$group) %>% 
      reshape2::melt(id.vars = "group") %>% 
      group_by(group) %>% 
      summarise(mean = mean(value), 
                sd = sd(value),
                min = min(value),
                max = max(value)) %>% 
      left_join(data, by = "group") %>% 
      mutate(diff = abs(lg_count - mean)) 
    
    diff <- mean(posterior_predict_df_spec$diff)
    
    cat(paste0("The diff was ", diff %>% round(2)
               , ".\n"))
    
    #coef
    chain_1 <- output_spec$fit@sim$samples[[1]] %>% as.data.frame()  %>% mutate(chain = "1")
    chain_2 <- output_spec$fit@sim$samples[[2]] %>% as.data.frame()  %>% mutate(chain = "2")
    chain_3 <- output_spec$fit@sim$samples[[3]] %>% as.data.frame()  %>% mutate(chain = "3")
    chain_4 <- output_spec$fit@sim$samples[[4]] %>% as.data.frame()  %>% mutate(chain = "4")
    
    chain_joined <- suppressMessages(full_join(chain_1, chain_2)) %>% 
      suppressMessages(full_join(chain_3)) %>%
      suppressMessages(full_join(chain_4))
    
    predict_new_data <- brms::posterior_predict(output_spec, newdata = data, ndraws = ndraws) %>% 
      as.data.frame() %>% 
      data.table::transpose() %>% 
      mutate(group = data$group) %>% 
      reshape2::melt(id.vars = "group") %>% 
      group_by(group) %>% 
      summarise(mean = mean(value), 
                sd = sd(value),
                min = min(value),
                max = max(value)) %>% 
      left_join(data, by = "group") %>% 
      mutate(diff = abs(lg_count - mean)) 
  
    if(!is.na(obs)){
      predict_new_data  <- predict_new_data %>% 
        filter(group == {{obs}})
      
    }
    
    #output_data_frame
    
    
    
    
    df_spec <- data.frame(dropped_obs = obs, 
                          b_Carrying_capactiy_PC1_max = chain_joined$b_Carrying_capactiy_PC1 %>% max(),
                          b_Carrying_capactiy_PC1_min = chain_joined$b_Carrying_capactiy_PC1 %>% min(),
                          b_Carrying_capactiy_PC1_mean = chain_joined$b_Carrying_capactiy_PC1 %>% mean(),
                          b_Intercept_max = chain_joined$b_Intercept %>% max(),
                          b_Intercept_min = chain_joined$b_Intercept %>% min(),
                          b_Intercept_mean = chain_joined$b_Intercept %>% mean(),
                          b_Carrying_capactiy_PC2_max = chain_joined$b_Carrying_capactiy_PC2 %>% max(),
                          b_Carrying_capactiy_PC2_min = chain_joined$b_Carrying_capactiy_PC2 %>% min(),
                          b_Carrying_capactiy_PC2_mean = chain_joined$b_Carrying_capactiy_PC2 %>% mean(),
                          b_EA033_max = chain_joined$b_EA033 %>% max(),
                          b_EA033_min = chain_joined$b_EA033 %>% min(),
                          b_EA033_mean = chain_joined$b_EA033 %>% mean(),
                          b_Shoreline_max = chain_joined$b_Shoreline %>% max(),
                          b_Shoreline_min = chain_joined$b_Shoreline %>% min(),
                          b_Shoreline_mean = chain_joined$b_Shoreline %>% mean(),
                          b_Settlement_date_grouping_finer_max = chain_joined$b_Settlement_date_grouping_finer %>% max(),
                          b_Settlement_date_grouping_finer_min = chain_joined$b_Settlement_date_grouping_finer %>% min(),
                          b_Settlement_date_grouping_finer_mean = chain_joined$b_Settlement_date_grouping_finer %>% mean(),
                          mean_Rhat =  ms$fixed$Rhat %>% mean(),
                          mean_Bulk_ESS = ms$fixed$Bulk_ESS %>% mean(),
                          mean_Tail_ESS = ms$fixed$Tail_ESS %>% mean(),
                          diff_predicted_vs_observed = diff,
                          dropped_observation_prediction = predict_new_data$mean,
                          dropped_observation_prediction_diff = predict_new_data$diff) %>% 
      mutate(
        b_Carrying_capactiy_PC1_includes_zero = ifelse(
          b_Carrying_capactiy_PC1_max > 0 & b_Carrying_capactiy_PC1_min < 0 |
            b_Carrying_capactiy_PC1_max < 0 & b_Carrying_capactiy_PC1_min > 0, 
          yes = "Yes", no = "No")) %>% 
      mutate(
        b_Carrying_capactiy_PC2_includes_zero = ifelse(
          b_Carrying_capactiy_PC2_max > 0 & b_Carrying_capactiy_PC2_min < 0 |
            b_Carrying_capactiy_PC2_max < 0 & b_Carrying_capactiy_PC2_min > 0, 
          yes = "Yes", no = "No")) %>% 
      mutate(
        b_Intercept_includes_zero = ifelse(
          b_Intercept_max > 0 & b_Intercept_min < 0 |
            b_Intercept_max < 0 & b_Intercept_min > 0, 
          yes = "Yes", no = "No")) %>% 
      mutate(
        b_EA033_includes_zero = ifelse(
          b_EA033_max > 0 & b_EA033_min < 0 |
            b_EA033_max < 0 & b_EA033_min > 0, 
          yes = "Yes", no = "No")) %>% 
      mutate(
        b_Shoreline_includes_zero = ifelse(
          b_Shoreline_max > 0 & b_Shoreline_min < 0 |
            b_Shoreline_max < 0 & b_Shoreline_min > 0, 
          yes = "Yes", no = "No")) %>% 
      mutate(
        b_Settlement_date_grouping_finer_includes_zero = ifelse(
          b_Settlement_date_grouping_finer_max > 0 & b_Settlement_date_grouping_finer_min < 0 |
            b_Settlement_date_grouping_finer_max < 0 & b_Settlement_date_grouping_finer_min > 0, 
          yes = "Yes", no = "No"))
    
    df_all <- full_join(df_all, df_spec, 
                        by = join_by(dropped_obs, b_Carrying_capactiy_PC1_max, b_Carrying_capactiy_PC1_min,
                                     b_Carrying_capactiy_PC1_mean, b_Intercept_max, b_Intercept_min, 
                                     b_Intercept_mean, b_Carrying_capactiy_PC2_max,
                                     b_Carrying_capactiy_PC2_min, b_Carrying_capactiy_PC2_mean, 
                                     b_EA033_max, b_EA033_min, b_EA033_mean,
                                     b_Shoreline_max, b_Shoreline_min, b_Shoreline_mean, 
                                     b_Settlement_date_grouping_finer_max,
                                     b_Settlement_date_grouping_finer_min, 
                                     b_Settlement_date_grouping_finer_mean, mean_Rhat, mean_Bulk_ESS,   
                                     mean_Tail_ESS, diff_predicted_vs_observed, 
                                     dropped_observation_prediction, dropped_observation_prediction_diff,   
                                     b_Carrying_capactiy_PC1_includes_zero, 
                                     b_Carrying_capactiy_PC2_includes_zero, b_Intercept_includes_zero,   
                                     b_EA033_includes_zero, b_Shoreline_includes_zero,
                                     b_Settlement_date_grouping_finer_includes_zero))
    
  }
  
  #df_all <- read_tsv(paste0("output/results/brms_", group, "_group_drop_one_out.tsv"))
  
  #the run with no dropped generates 58 rows, let's cut that down to one
  df_all <- df_all %>% 
    mutate(dropped_obs = ifelse(is.na(dropped_obs), "NONE", dropped_obs)) %>% 
    group_by(dropped_obs) %>% 
    mutate(dropped_observation_prediction_mean = mean(dropped_observation_prediction, na.rm = T),
           dropped_observation_prediction_diff_mean = mean(dropped_observation_prediction_diff, na.rm = T)) %>% 
    distinct(dropped_obs, dropped_observation_prediction_diff_mean, .keep_all = T) 
  
  df_all %>%          
    write_tsv(file = paste0("output/results/brms_", group, "_group_drop_one_out.tsv"), na = "")
  
  
  df_all$dropped_obs <- fct_reorder(df_all$dropped_obs, df_all$diff_predicted_vs_observed)
  
  df_all %>% 
    ggplot() +
    geom_bar(aes(x = dropped_obs, y = diff_predicted_vs_observed, fill = diff_predicted_vs_observed), stat = "identity") +
    theme_fivethirtyeight() +
    theme(axis.text.x =  element_text(angle = 70, hjust = 1) , 
          legend.position = "none") +
    scale_fill_viridis(direction = -1) #+
#    ylim(c(0,3.4))
  
  ggsave(filename = paste0("output/plots/brms_", group, "_dropped_out_plot_diff.png"), width = 9, height = 9)
  ggsave(filename = paste0("../latex/brms_", group, "_dropped_out_plot_diff.png"), width = 9, height = 9)
  
  
  df_all %>% 
    filter(diff_predicted_vs_observed < 2.5) %>% 
    #  column_to_rownames("dropped_obs") %>% 
    data.table::transpose(make.names = "dropped_obs", keep.names = "variable") %>% 
    write_tsv(file = paste0("output/results/brms_", group, "_dropped_effects.tsv"), na = "")
  
  ######################################
  
  # 
  # #calculating the estimate value "manually" from the coef vs what predict() does
  # for(n in 1:58){
  # 
  #   n <- 13
  # 
  # model_estimate <- predict_df[n,]$predicted_poission.Estimate
  # 
  # manual_estimate <- exp((mean(data_chopped[n,]$Carrying_capactiy_PC1 * chain_joined$b_Carrying_capactiy_PC1)
  #  +
  #        mean(data_chopped[n,]$Carrying_capactiy_PC2 * chain_joined$b_Carrying_capactiy_PC2)
  #  +
  #        mean(data_chopped[n,]$Shoreline * chain_joined$b_Shoreline)
  #  +
  #        mean(data_chopped[n,]$EA033 * chain_joined$b_EA033)
  #  +
  #        mean(data_chopped[n,]$Settlement_date_grouping_finer * chain_joined$b_Settlement_date_grouping_finer)) +
  #    mean(chain_joined$b_Intercept)
  # )
  # 
  # print(manual_estimate-model_estimate)
  # 
  # }
  # 
  # 
  
}

