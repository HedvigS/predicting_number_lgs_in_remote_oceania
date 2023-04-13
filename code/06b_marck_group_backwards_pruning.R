source("01_requirements.R")

data <- read_tsv("output/processed_data/RO_Hedvig_aggregate_marck_group_scaled.tsv", show_col_types = F) 

##full model
full_model <- glm.nb(data = data, lg_count  ~  Annual_precipitation_mean * Precipitation_seasonality_mean +
                       Annual_temperature_mean * Temperature_seasonality_mean +
                        Latitude_abs_mean  +
                       EA033 +  Isolation + 
                       Shoreline * Settlement_date_grouping_finer +
                       Area_land * Settlement_date_grouping_finer  +
                      ratio_coastline_to_area* Settlement_date_grouping_finer                 ,  
                     control = list(maxit = 30000, epsilon = 1e-7, trace = 3))

broomed_full_model <-  broom::tidy(full_model) %>% 
  mutate(estimate = round(as.numeric(estimate), digits = 7)) %>% 
  mutate(p.value = round(p.value, digit = 5)) %>% 
  mutate(statistic_abs = abs(statistic)) %>% 
  arrange(-statistic_abs) %>% 
  dplyr::select(term, estimate, p.value, statistic = statistic_abs) %>% 
  mutate(estimate_abs = abs(estimate)) %>% 
  mutate(sig = ifelse(`p.value` <0.05, "Sig", "Non Sig")) 
  
anova_full_model <- anova(full_model, test = "Chisq") %>% 
  rownames_to_column("variable") %>% 
  mutate(interactions = str_count(variable, ":") + 1) %>% 
  arrange(-interactions, -`Pr(>Chi)`) %>% 
  mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig"))

write_tsv(x = broomed_full_model, file = "output/results/marck_full_model_broomed.tsv")
write_tsv(x = anova_full_model, file = "output/results/marck_full_model_anova.tsv")

##backwards pruning
#variable_highest_p_value <- anova_full_model[1,1]

to_drop_df <- anova_full_model %>% 
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




sig_vars <- anova_full_model %>% 
  filter(sig == "Sig") %>% 
  dplyr::select(variable) %>% 
  as.matrix() %>% 
  as.vector() %>% 
  paste(collapse = ",")

if(variable_highest_p_value %in% sig_vars){
  cat("The variable with the highest p-value is involved in an interaction!!!")
}

model_1 <- update(full_model,  paste(". ~ . -", variable_highest_p_value))

anova_model_1 <- anova(model_1, test = "Chisq") %>% 
  rownames_to_column("variable") %>% 
  mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
  mutate(interactions = str_count(variable, ":") + 1) %>% 
  arrange(-interactions, -`Pr(>Chi)`) %>% 
  mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>% 
  dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())

#View(anova_model_1)
variable_highest_p_value <- anova_model_1[1,1]

sig_vars <- anova_model_1 %>% 
  filter(sig == "Sig") %>% 
  dplyr::select(variable) %>% 
  as.matrix() %>% 
  as.vector() %>% 
  paste(collapse = ",")

if(variable_highest_p_value %in% sig_vars){
  cat("The variable with the highest p-value is involved in an interaction!!!")
}

model_2 <- update(model_1,  paste(". ~ . -", variable_highest_p_value))

anova_model_2 <- anova(model_2, test = "Chisq") %>% 
  rownames_to_column("variable") %>% 
  mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
  mutate(interactions = str_count(variable, ":") + 1) %>% 
  arrange(-interactions, -`Pr(>Chi)`) %>% 
  mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>% 
  dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())

#View(anova_model_2)
variable_highest_p_value <- anova_model_2[1,1]

model_3 <- update(model_2,paste(". ~ . -", variable_highest_p_value))

anova_model_3 <- anova(model_3,test = "Chisq") %>% 
  rownames_to_column("variable") %>% 
  mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
  mutate(interactions = str_count(variable, ":") + 1) %>% 
  arrange(-interactions, -`Pr(>Chi)`) %>% 
  mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>% 
  dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())

#View(anova_model_3)
variable_highest_p_value <- anova_model_3[1,1]

model_4 <- update(model_3,paste(". ~ . -", variable_highest_p_value))

anova_model_4 <- anova(model_4, test = "Chisq") %>% 
  rownames_to_column("variable") %>% 
  mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
  mutate(interactions = str_count(variable, ":") + 1) %>% 
  arrange(-interactions, -`Pr(>Chi)`) %>% 
  mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>% 
  dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())

#View(anova_model_4)
variable_highest_p_value <-  anova_model_4[1,1]
 
 model_5 <- update(model_4, paste(". ~ . -", variable_highest_p_value))
              
anova_model_5 <- anova(model_5, test = "Chisq") %>%
  rownames_to_column("variable") %>%
  mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
  mutate(interactions = str_count(variable, ":") + 1) %>%
  arrange(-interactions, -`Pr(>Chi)`) %>%
  mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>%
  dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())

#View(anova_model_5)
variable_highest_p_value <-  anova_model_5[1,1]

model_6 <- update(model_5, paste(". ~ . -", variable_highest_p_value))

anova_model_6 <- anova(model_6, test = "Chisq") %>%
  rownames_to_column("variable") %>%
  mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
  mutate(interactions = str_count(variable, ":") + 1) %>%
  arrange(-interactions, -`Pr(>Chi)`) %>%
  mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>%
  dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())

#View(anova_model_6)
variable_highest_p_value <-  anova_model_6[1,1]

model_7 <- update(model_6, paste(". ~ . -", variable_highest_p_value))

anova_model_7 <- anova(model_7, test = "Chisq") %>%
  rownames_to_column("variable") %>%
  mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
  mutate(interactions = str_count(variable, ":") + 1) %>%
  arrange(-interactions, -`Pr(>Chi)`) %>%
  mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>%
  dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())

#View(anova_model_7)
variable_highest_p_value <-  anova_model_7[1,1]

model_8 <- update(model_7, paste(". ~ . -", variable_highest_p_value))

anova_model_8 <- anova(model_8, test = "Chisq") %>%
  rownames_to_column("variable") %>%
  mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
  mutate(interactions = str_count(variable, ":") + 1) %>%
  arrange(-interactions, -`Pr(>Chi)`) %>%
  mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>%
  dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())

#View(anova_model_8)
variable_highest_p_value <-  anova_model_8[1,1]

 model_9 <- update(model_8, paste(". ~ . -", variable_highest_p_value))

  anova_model_9 <- anova(model_9, test = "Chisq") %>%
  rownames_to_column("variable") %>%
   mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
   mutate(interactions = str_count(variable, ":") + 1) %>%
  arrange(-interactions, -`Pr(>Chi)`) %>%
   mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>%
  dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())
 
# View(anova_model_9)
  variable_highest_p_value <-    anova_model_9[1,1]
# 
 model_10 <- update(model_9, paste(". ~ . -", variable_highest_p_value))

anova_model_10 <- anova(model_10, test = "Chisq") %>%
 rownames_to_column("variable") %>%
  mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
  mutate(interactions = str_count(variable, ":") + 1) %>%
 arrange(-interactions, -`Pr(>Chi)`) %>%
  mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>%
  dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())

#View(anova_model_10)
variable_highest_p_value <-    anova_model_10[1,1]

model_11 <- update(model_10, paste(". ~ . -", variable_highest_p_value))

anova_model_11 <- anova(model_11, test = "Chisq") %>%
 rownames_to_column("variable") %>%
 mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
 mutate(interactions = str_count(variable, ":") + 1) %>%
 arrange(-interactions, -`Pr(>Chi)`) %>%
 mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>%
 dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())
 
variable_highest_p_value <-    anova_model_11[1,1]

model_12 <- update(model_11, paste(". ~ . -", variable_highest_p_value))

anova_model_12 <- anova(model_12, test = "Chisq") %>% 
  rownames_to_column("variable") %>% 
  mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
  mutate(interactions = str_count(variable, ":") + 1) %>% 
  arrange(-interactions, -`Pr(>Chi)`) %>% 
  mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>% 
  dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())

variable_highest_p_value <-    anova_model_12[1,1]

model_13 <- update(model_12, paste(". ~ . -", variable_highest_p_value))

anova_model_13 <- anova(model_13, test = "Chisq") %>% 
  rownames_to_column("variable") %>% 
  mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
  mutate(interactions = str_count(variable, ":") + 1) %>% 
  arrange(-interactions, -`Pr(>Chi)`) %>% 
  mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>% 
  dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())

###STOP

final_model <- model_13

summary(final_model)

modEvA::RsqGLM(final_model)
rsq(final_model)
pseudo_r = (final_model$null.deviance - final_model$deviance) / final_model$null.deviance

xtable(final_model, digits = 2, caption = c("GLM model for predicting language counts in island groups joined for overnight sailing distances."), label = "table:GLM_model_marck") %>% 
  print() %>% 
  write_lines("output/sheets/glm_table_Marck_model.txt")


broomed_final_model <-  broom::tidy(final_model) %>% 
  mutate(estimate = round(as.numeric(estimate), digits = 7)) %>% 
  mutate(p.value = round(p.value, digit = 5)) %>% 
  mutate(std.error = round(std.error, digit = 5)) %>% 
  mutate(statistic_abs = abs(statistic)) %>% 
  arrange(-statistic_abs) %>% 
  dplyr::select(term, estimate, p.value, `z-value` = statistic_abs, std.error) %>% 
  mutate(sig = ifelse(p.value <0.05, "Sig", "Non Sig"))

write_tsv(broomed_final_model, "output/sheets/broomed_model_marck_group.tsv")

lgs <- final_model$model %>% 
  left_join(data)

predict_data_with_interaction <- predict(final_model, type = "response", se.fit = T) %>% 
  as.data.frame() %>% 
  cbind(lgs) %>% 
  dplyr::rename(Predicted_lg_count = "fit") %>%   
  mutate(diff = ((abs(lg_count - Predicted_lg_count))))

#predict_data_with_interaction %>% 
#  dplyr::select(group, EA033, lg_count, Predicted_lg_count, diff, Shoreline, Settlement_order_oldest) %>% View() 

mean_diff <- round(mean(predict_data_with_interaction$diff), digits = 3)

predict_data_with_interaction$group <- fct_reorder(predict_data_with_interaction$group, predict_data_with_interaction$lg_count)

marck_group_island_merged_with_interaction_plot <-  predict_data_with_interaction %>% 
  dplyr::select(-diff) %>%
  dplyr::select(group, `Observed language count` = lg_count, `Predicted language count` = Predicted_lg_count, everything()) %>%
  #  melt() %>% 
  #  left_join(predict_data_with_interaction_se_fit) %>% 
  ggplot() +
  geom_point(aes(y = group, x = `Predicted language count`), fill = "#00b3ca", size = 2, alpha = 0.8, shape = 21, stroke = 0.4) +
  geom_point(aes(y = group, x = `Observed language count`), fill = "#f2501c" , size = 2, alpha = 0.8, shape = 21, stroke = 0.4) +
  #  coord_flip() +
  geom_errorbarh(aes(xmin = `Predicted language count` - se.fit, xmax = `Predicted language count` + se.fit, y = group), alpha = 0.6) +
  theme_fivethirtyeight() +
  #  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.x = element_text(size = 8), 
        legend.title = element_blank()) +
  ggtitle(label = paste0("Prediction of number of languages per island group"), subtitle= paste0("(overnight distance groups) \nMean diff = ", mean_diff)) +
  scale_color_manual(values=wes_palette(n=2, name="Darjeeling1")) +
  theme(title = element_text(size = 12)) +
  theme(panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"), 
        plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"))


plot(marck_group_island_merged_with_interaction_plot)

ggsave("output/plots/Marck_group_model_prediction.png", limitsize = F, width = 9, height = 10)
ggsave("../../Hedvigs_academia/Hedvigs PhD thesis/tex/illustrations/plots_from_R/Marck_group_model_prediction.png", limitsize = F, width = 9, height = 10)



marck_group_island_merged_with_interaction_plot_log10 <-  predict_data_with_interaction %>% 
  dplyr::select(-diff) %>%
  dplyr::select(group, `Observed language count` = lg_count, `Predicted language count` = Predicted_lg_count, everything()) %>%
  #  melt() %>% 
  #  left_join(predict_data_with_interaction_se_fit) %>% 
  ggplot() +
  geom_point(aes(y = group, x = log10(`Predicted language count`)), fill = "#00b3ca", size = 2, alpha = 0.8, shape = 21, stroke = 0.4) +
  geom_point(aes(y = group, x = log10(`Observed language count`)), fill = "#f2501c" , size = 2, alpha = 0.8, shape = 21, stroke = 0.4) +
  #  coord_flip() +
  geom_errorbarh(aes(xmin = log10(`Predicted language count` - se.fit), xmax = log10(`Predicted language count` + se.fit), y = group), alpha = 0.6) +
  theme_fivethirtyeight() +
  #  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.x = element_text(size = 8), 
        legend.title = element_blank()) +
  ggtitle(label = paste0("Prediction of number of languages per island group"), subtitle= paste0("(overnight distance groups) \nlog 10")) +
  scale_color_manual(values=wes_palette(n=2, name="Darjeeling1")) +
  theme(title = element_text(size = 12)) +
  theme(panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"), 
        plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"))



plot(marck_group_island_merged_with_interaction_plot_log10)

ggsave("output/plots/Marck_group_model_prediction_log10.png", limitsize = F, width = 9, height = 10)
ggsave("../../Hedvigs_academia/Hedvigs PhD thesis/tex/illustrations/plots_from_R/Marck_group_model_prediction_log10.png", limitsize = F, width = 9, height = 10)


