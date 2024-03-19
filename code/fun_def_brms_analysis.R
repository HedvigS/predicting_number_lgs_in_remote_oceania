
fun_brms_predicting <- function(data = NULL, 
                                data2 = NULL,
                                       formula = NULL, 
                                      control = NULL,
                                       group = NULL,
                                       drop_one_out = TRUE,
                                       iter = 30000,
                                       warmup = 1000,
                                       chains = 4,
                                       cores = 4,
                                       seed = 10,
                                       ndraws = 10000
){

  #sanity checks of arguments
  if(length(control) == 0 ){
    stop("The argument control is NULL.")    
    
  }
  
  if(!(control %in% c("phylo", "none", "spatial","spatialphylo"))     ){
stop("The argument control is not one of the recognised strings.")    
  }
  
  if(control == "phylo" & 
     !str_detect(as.character(formula)[3], pattern = "phylo_vcv")){
    stop("The control argument is set to phylo but the formula does not include phylo_vcv.")
    }
  
  if(control == "spatial" & 
     !str_detect(as.character(formula)[3], pattern = "spatial_vcv")){
    stop("The control argument is set to spatial but the formula does not include spatial_vcv.")
  }
  
  
  if(control == "spatialphylo" & 
     !str_detect(as.character(formula)[3], pattern = "spatial_vcv") &
     !str_detect(as.character(formula)[3], pattern = "phylo_vcv")){
    stop("The control argument is set to spatialphylo but the formula does not include spatial_vcv and phylo_vcv.")
  }
  
#  iter = 30000
#  warmup = 1000
#  chains = 4
#  cores = 4
#  seed = 10
#  ndraws = 10000
  
  ############# ALL OBSERVATIONS ####################
  
  output_poisson <-  brms::brm(data = data, 
                               data2 = data2,
                               family = poisson,
                          formula = formula,
                          iter = iter, 
                          silent = 2,
                          refresh = 0,
                          warmup = warmup, 
                          chains = chains, 
                          cores = cores,
                          seed = seed,
                          control = list(adapt_delta = 0.9),
                          backend="cmdstanr") 
  
  waic <- loo::waic(output_poisson)
  loo <- loo::loo(output_poisson)
  bayes_r2 <- bayes_R2(output_poisson, probs = c(0, 0.025, 0.975, 1)) 

  waic$estimates %>% 
    as.data.frame() %>% 
    rownames_to_column("fit_score") %>%
    mutate(control = control) %>% 
    mutate(group = group) %>% 
    write_tsv(file = paste0("output/results/brms_", group, "_control_", control,"_model_fit_waic.tsv"), na = "")
  
  loo$estimates %>% 
    as.data.frame() %>% 
    rownames_to_column("fit_score") %>% 
    mutate(control = control) %>% 
    mutate(group = group) %>% 
    write_tsv(file = paste0("output/results/brms_", group, "_control_", control,"_model_fit_loo.tsv"), na = "")
  
  bayes_r2 %>%
    as.data.frame() %>% 
    rownames_to_column("fit_score") %>% 
    mutate(control = control) %>% 
    mutate(group = group) %>% 
    write_tsv(file = paste0("output/results/brms_", group, "_control_", control,"_model_fit_R2.tsv"), na = "")
  
ms_full <- summary(output_poisson)
  
  posterior_predict_df <- brms::posterior_predict(output_poisson, cores = cores, ndraws = ndraws) %>%
    as.data.frame() %>% 
    data.table::transpose() %>%
    mutate(group = data$group) %>% 
    reshape2::melt(id.vars = "group") %>% 
    group_by(group) %>% 
    mutate(mean = mean(value), 
              sd = sd(value),
              min = min(value),
              max = max(value)) %>% 
    left_join(data, by = "group") %>% 
    mutate(diff_poisson = lg_count - mean) %>%  
    mutate(diff_poisson_abs = abs(diff_poisson)) 
  
  cat(paste0("The mean absolute difference between the predicted and observed numbers of languages is ", posterior_predict_df$diff_poisson_abs %>% mean() %>% round(2), ".\n"))
  
  data.frame(
    diff_poisson_abs  =  posterior_predict_df$diff_poisson_abs %>% mean(), 
    diff_poisson = posterior_predict_df$diff_poisson %>% mean()
  ) %>% 
    mutate(control = control) %>% 
    mutate(group = group) %>% 
  write_tsv(file = paste0("output/results/brms_", group, "_control_", control,"_diff_means.tsv"), na = "")
  
  posterior_predict_df %>% 
    distinct(group, mean, sd, min, max, diff_poisson, diff_poisson_abs, lg_count) %>% 
    mutate(control = control) %>% 
    mutate(group = group) %>% 
    write_tsv(file = paste0("output/results/brms_", group, "_control_", control,"_predict_table.tsv"), na = "")
  
  #predict plot
  
  posterior_predict_df$group <- fct_reorder(posterior_predict_df$group, posterior_predict_df$lg_count)
  
  p <- posterior_predict_df %>% 
    ggplot() +
#    geom_density_ridges(aes(x = lg_count, y = group, fill = group), quantile_lines = T, quantile_fun = mean, jittered_points = TRUE, point_size = 2, point_shape = 21  ,  position = position_points_jitter(height = 0)) +
    geom_boxplot(mapping = aes(y = group, x = value), color = "#2EB37CFF", fill = "#65CB5EFF", alpha = 0.2) +
    geom_point(aes(y = group, x = mean),
               fill = "#f5ea25", color = "#481769FF",
               shape =21, alpha = 0.4, stroke = 0.6, 
               size = 1.6) +
    geom_point(aes(y = group, x = lg_count),
               fill = "#355E8DFF", color = "#481769FF",
               shape =24, alpha = 0.4, stroke = 0.6, 
               size = 1.6) +
    ggthemes::theme_fivethirtyeight() +
    scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150, 175)) +
    theme(legend.position = "None",
      panel.background = element_rect(fill = "white"), 
          plot.background = element_rect(fill = "white"))
  
  ggsave(plot = p, filename = paste0("output/plots/brms_predict_", group, "_control_", control, ".png"), height = 8, width = 6)  
  ggsave(plot = p,filename = paste0("../latex/brms_predict_", group, "_control_", control, ".png"),  height = 8, width = 6) 
  
  ### model output
  chain_1 <- output_poisson$fit@sim$samples[[1]] %>% as.data.frame()  %>% mutate(chain = "1")
  chain_2 <- output_poisson$fit@sim$samples[[2]] %>% as.data.frame()  %>% mutate(chain = "2")
  chain_3 <- output_poisson$fit@sim$samples[[3]] %>% as.data.frame()  %>% mutate(chain = "3")
  chain_4 <- output_poisson$fit@sim$samples[[4]] %>% as.data.frame()  %>% mutate(chain = "4")
  
  chain_joined <- suppressMessages(full_join(chain_1, chain_2)) %>% 
    suppressMessages(full_join(chain_3)) %>%
    suppressMessages(full_join(chain_4))
  
  chain_joined %>% 
    write_tsv(file = paste0("output/results/brms_", group, "_control_", control, "_full_chains.tsv"), na = "")
  
colnames(chain_joined) <- str_replace_all(colnames(chain_joined), "b_", "")  
colnames(chain_joined) <- str_replace_all(colnames(chain_joined), "bsp_", "")  

#making a straddle df
ms_df_long <- ms_full$fixed %>% 
  rownames_to_column("term") %>% 
  mutate(straddle_zero_95 = ifelse(`l-95% CI` < 0 & `u-95% CI` < 0|
                                     `l-95% CI`  > 0 & `u-95% CI`  > 0  , "no", "yes")) %>% 
  reshape2::melt(id.vars = "term") 

ms_df_long_straddle <- ms_df_long %>% 
  filter(variable == "straddle_zero_95") %>% 
  dplyr::select(variable = term, straddle_zero_95 = value)

p <-  chain_joined %>% 
    reshape2::melt(id.vars = "chain") %>%  
    left_join(ms_df_long_straddle, by = "variable" ) %>% 
    filter(variable != "lprior") %>% 
    filter(variable != "lp__") %>% 
  filter(!str_detect(variable, "ntercept")) %>%
  filter(!str_detect(variable, "r_group")) %>%
  filter(!str_detect(variable, "simo_mo")) %>%
    filter(variable != "Intercept") %>% 
    mutate(variable = str_replace_all(variable,"Settlement_date_grouping_finer", "Time depth")) %>% 
    mutate(variable = str_replace_all(variable,"bsp_mo", "")) %>% 
    mutate(variable = str_replace_all(variable,"moEA", "EA")) %>% 
    mutate(variable = str_replace_all(variable,"moTime", "Time")) %>% 
                ggplot(aes(x = value, fill = variable, 
               color = variable,
               y = after_stat(density))) + 
    scale_color_manual(values = distinctive_plot_colors) +
    scale_fill_manual(values = distinctive_plot_colors) +
    geom_density(mapping = aes(alpha = as.factor(straddle_zero_95), 
                               linetype = as.factor(straddle_zero_95)),
                   color = "darkgray",
                   linewidth = 0.8, adjust = 0.7
    ) +
  suppressWarnings(  scale_alpha_discrete(range = c(1, 0.1)) )+
    lemon::facet_rep_wrap(~variable, 
                          #ncol = 3, 
                          #             scales = "free",
                          repeat.tick.labels = c('bottom')) +
    geom_vline(aes(xintercept = 0), linetype="dashed", color = "darkgray", alpha = 0.7) +
    theme_classic() +
    theme(legend.position = "none", 
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title = element_blank(),
          strip.text = element_text(size = 14))

    ggsave(plot = p, filename = paste0("output/plots/brms_", group, "_control_", control, "_group_full_effect_ridge_panels_plot.png"), height = 7, width = 7, dpi = 200)
  ggsave(plot = p, filename = paste0("../latex/brms_", group, "_control_", control, "_group_full_effect_ridge_panels_plot.png"), height = 7, width = 7, dpi = 200)
   
  ms_df <- ms_df_long %>% 
  unite(term, variable, sep = "§", col = "variable") %>% 
  data.table::transpose(make.names = "variable")

  chain_summarised <-      chain_joined %>% 
    reshape2::melt(id.vars = c("chain")) %>% 
    group_by(variable) %>% 
    summarise(mean = mean(value), 
              sd = sd(value),
              min = min(value),
              max = max(value)) %>% 
    mutate(straddle_zero = ifelse(max < 0 & min < 0|
                                    max > 0 & min > 0  , "no", "yes")) %>%
    rename(variable_1 = variable) %>% 
    reshape2::melt(id.vars = "variable_1") %>%
    ungroup() %>% 
    mutate(variable = paste0(`variable_1`, "§", `variable`)) %>% 
    dplyr::select(variable, value) %>% 
    data.table::transpose(make.names = "variable") %>% 
    cbind(ms_df) %>% 
    mutate(dropped_obs = "NONE_original_full")

chain_summarised  %>% 
  reshape2::melt(id.vars = "dropped_obs") %>% 
  separate(col = variable, into = c("term","variable"), sep = "§", remove = T) %>% 
  dplyr::select(-dropped_obs) %>% 
  write_tsv(file = paste0("output/results/brms_", group, "_control_", control, "_full_effects_table.tsv"))
  
  ########### KICKING OUT ONE OBSERVATION AT A TIME
  
if(drop_one_out == TRUE){
#empty df to bind to in the for-loop
  df_all <- data.frame(
    "dropped_obs"     = as.character()  )
  
  #add NA to the things to exclude, stands for excluding nothing so that it's all in a neat table.
  obs <- c("None", data$group)
  
  for(ob in obs){
    #  ob <- obs[6]
    
    dir_spec <- paste0("output/results/drop_one_out/", group, "_", control, "/")
    if(!dir.exists(dir_spec)){dir.create(dir_spec)}
    
    dir_spec <- paste0("output/results/drop_one_out/", group, "_", control, "/", str_replace_all(ob, " ", "_"), "/")
    if(!dir.exists(dir_spec)){dir.create(dir_spec)}

    
    cat(paste0("Dropping out ", ob, " with group: ", group, " and control: ", control, ".\n"))
    
fn <-     paste0(dir_spec,"diff_means.tsv")
if(file.exists(fn)){

  cat(paste0("already exists. moving on.\n"))
  
}else{
  
    if(ob == "None"){
      data_spec <-   data 
  
        }else{
      data_spec <-   data %>% 
      filter(group != {{ob}})
          }
    output_spec <-  brms::brm(data = data_spec, 
                              data2 = data2, 
                        family = poisson,
                        formula = formula,
                        iter = iter, 
                        silent = 2,
                        refresh = 0,
                        warmup = warmup, 
                        chains = chains, 
                        cores = cores,
                        seed = seed,
                        control = list(adapt_delta = 0.9),
                        backend="cmdstanr") 
    
    waic <- loo::waic(output_spec)
    loo <- loo::loo(output_spec)
    bayes_r2 <- bayes_R2(output_spec, probs = c(0, 0.025, 0.975, 1)) 
    
    waic$estimates %>% 
      as.data.frame() %>% 
      rownames_to_column("fit_score") %>% 
      mutate(island_group_dropped = ob) %>% 
      mutate(control = control) %>% 
      mutate(group = group) %>% 
      write_tsv(file = paste0(dir_spec,"model_fit_waic.tsv"), na = "")
    
    loo$estimates %>% 
      as.data.frame() %>% 
      rownames_to_column("fit_score") %>% 
      mutate(island_group_dropped = ob) %>% 
      mutate(control = control) %>% 
      mutate(group = group) %>% 
      write_tsv(file = paste0(dir_spec, "model_fit_loo.tsv"), na = "")
    
    bayes_r2 %>%
      as.data.frame() %>% 
      rownames_to_column("fit_score") %>% 
      mutate(island_group_dropped = ob) %>% 
      mutate(control = control) %>% 
      mutate(group = group) %>% 
      write_tsv(file = paste0(dir_spec, "model_fit_R2.tsv"), na = "")
    
    
    ms <- summary(output_spec)
    
ms_df <-  ms$fixed %>% 
  rownames_to_column("term") %>% 
  mutate(straddle_zero_95 = ifelse(`l-95% CI` < 0 & `u-95% CI` < 0|
                                     `l-95% CI`  > 0 & `u-95% CI`  > 0  , "no", "yes")) %>%
  reshape2::melt(id.vars = "term") %>% 
  unite(term, variable, sep = "§", col = "variable") %>% 
  data.table::transpose(make.names = "variable") %>% 
  mutate(dropped_obs = ob)

ms_df %>% 
write_tsv(file = paste0(dir_spec, "ms_df.tsv"), na = "")

  #predicting number of lgs
    posterior_predict_df_spec <- brms::posterior_predict(output_spec, cores = cores, ndraws = ndraws) %>%
      as.data.frame() %>% 
      data.table::transpose() %>% 
      mutate(group = data_spec$group) %>% 
      reshape2::melt(id.vars = "group") %>%
      group_by(group) %>% 
      mutate(mean = mean(value), 
                sd = sd(value),
                min = min(value),
                max = max(value)) %>% 
      left_join(data, by = "group") %>% 
      mutate(diff = lg_count-mean) %>%  
      mutate(diff_abs = abs(diff)) %>% 
      mutate(predicted = mean)
    
    diff <- mean(posterior_predict_df_spec$diff)
    diff_abs <- mean(posterior_predict_df_spec$diff_abs)
    predicted <- mean(posterior_predict_df_spec$predicted)
    
    
    data.frame(
      diff_poisson_abs  =  diff_abs, 
      diff_poisson = diff
    ) %>% 
      mutate(island_group_dropped = ob) %>% 
      mutate(control = control) %>% 
      mutate(group = group) %>% 
      write_tsv(file = paste0(dir_spec, "diff_means.tsv"), na = "")
    
    cat(paste0("The diff was ", diff_abs %>% round(2)
               , ".\n"))
    
    #coef
    chain_1 <- output_spec$fit@sim$samples[[1]] %>% as.data.frame()  %>% mutate(chain = "1")
    chain_2 <- output_spec$fit@sim$samples[[2]] %>% as.data.frame()  %>% mutate(chain = "2")
    chain_3 <- output_spec$fit@sim$samples[[3]] %>% as.data.frame()  %>% mutate(chain = "3")
    chain_4 <- output_spec$fit@sim$samples[[4]] %>% as.data.frame()  %>% mutate(chain = "4")
    
    chain_joined <- suppressMessages(full_join(chain_1, chain_2)) %>% 
      suppressMessages(full_join(chain_3)) %>%
      suppressMessages(full_join(chain_4))
    
    chain_summarised <-      chain_joined %>% 
      dplyr::select(-c("lp__", "lprior")) %>% 
      reshape2::melt(id.vars = c("chain")) %>% 
      group_by(variable) %>% 
      summarise(mean = mean(value), 
                sd = sd(value),
                min = min(value),
                max = max(value)) %>% 
      mutate(straddle_zero = ifelse(max < 0 & min < 0|
                                      max > 0 & min > 0  , "no", "yes")) %>% 
      rename(variable_1 = variable) %>% 
      reshape2::melt(id.vars = "variable_1") %>%
      ungroup() %>% 
      mutate(variable = paste0(`variable_1`, "_", `variable`)) %>% 
      dplyr::select(variable, value) %>% 
      data.table::transpose(make.names = "variable") %>% 
      mutate(dropped_obs = ob) 
  
      #output_data_frame
    
    df_spec <- data.frame(dropped_obs = ob, 
                          mean_Rhat =  ms$fixed$Rhat %>% mean(),
                          mean_Bulk_ESS = ms$fixed$Bulk_ESS %>% mean(),
                          mean_Tail_ESS = ms$fixed$Tail_ESS %>% mean(),
                          diff_predicted_vs_observed = diff,
                          diff_predicted_vs_observed_abs = diff_abs) %>% 
      full_join(chain_summarised, by = "dropped_obs") %>% 
      full_join(ms_df, by = "dropped_obs")
    
    df_spec %>% 
      write_tsv(file = paste0(dir_spec,"df_spec.tsv"), na = "")
    
    df_all <- suppressMessages(full_join(df_all, df_spec))
    
    #the run with no dropped generates 58 rows, let's cut that down to one
    df_all <- df_all %>% 
      group_by(dropped_obs) %>% 
      mutate(dropped_observation_prediction_diff_mean = mean(diff_predicted_vs_observed_abs, na.rm = T)) %>% 
      distinct(dropped_obs, dropped_observation_prediction_diff_mean, .keep_all = T) 
    
    df_all %>%          
      write_tsv(file = paste0("output/results/brms_", group, "_control_", control, "_group_drop_one_out.tsv"), na = "")
    
  } 
  }
  
}
}

