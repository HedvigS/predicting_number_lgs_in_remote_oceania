source("01_requirements.R")

data <- read_tsv("output/processed_data/RO_Hedvig_aggregate_marck_group.tsv", show_col_types = F) %>% 
  dplyr::filter(!is.na(Marck_group)) %>% 
  dplyr::select(group = Marck_group, everything()) %>% 
  dplyr::mutate(Latitude_abs_mean = abs(mean_lat)) %>% 
  filter(!is.na(mean_pol_complex))   %>% 
  rename(Annual_precipitation_mean = mean_CCSM_piControl_1760_bio12) %>% 
  rename(Precipitation_seasonality_mean = mean_CCSM_piControl_1760_bio15) %>% 
  rename(Annual_temperature_mean = mean_CCSM_piControl_1760_bio1) %>% 
  rename(Temperature_seasonality_mean = mean_CCSM_piControl_1760_bio4) %>% 
  rename(EA033 = mean_pol_complex) %>% 
  rename(Area_incl_water = sum_water_area) %>% 
  rename(Area_land = sum_area) %>% 
  rename(Shoreline = sum_shoreline) %>% 
  rename(Settlement_date_grouping_finer = settlement_date_grouping_finer) %>% 
  rename(Net_Primary_Production_mean = mean_NPP) %>% 
  rename(Net_Primary_Production_Predictability_mean = mean_NetPrimaryProductionPredictability) %>% 
  rename(Isolation = dist) %>% 
  mutate(group = str_replace_all(group, "Kanaky", "New Caledonia (incl loyalties)")) %>% 
  mutate(group = str_replace_all(group, "and", "+")) %>% 
  arrange(-lg_count) %>% 
  dplyr::select(group, lg_count, Latitude_abs_mean, Annual_precipitation_mean, Precipitation_seasonality_mean, Annual_temperature_mean, Temperature_seasonality_mean, EA033, Area_land, Shoreline, Isolation, Settlement_date_grouping_finer, ratio_coastline_to_area)

#log10 size variables
data$Area_land <- log10(data$Area_land)
data$Shoreline <- log10(data$Shoreline)
data$ratio_coastline_to_area <- data$Shoreline / data$Area_land
data$Isolation <- log10(data$Isolation)

#data$lg_count <- log10(data$lg_count)


#normalise by standard deviation

 data$Area_land <- scale(x = data$Area_land)[,1]
 data$Shoreline <- scale(x = data$Shoreline)[,1]
 data$ratio_coastline_to_area <- scale(x = data$ratio_coastline_to_area)[,1]
 data$Isolation <- scale(x = data$Isolation)[,1]
 data$Annual_precipitation_mean <- scale(x = data$Annual_precipitation_mean)[,1]
 data$Precipitation_seasonality_mean <- scale(x = data$Precipitation_seasonality_mean)[,1]
 data$Annual_temperature_mean <- scale(x = data$Annual_temperature_mean)[,1]
 data$Temperature_seasonality_mean <- scale(x = data$Temperature_seasonality_mean)[,1]
 data$Settlement_date_grouping_finer <- scale(x = data$Settlement_date_grouping_finer)[,1]
 data$EA033 <- scale(x = data$EA033)[,1]
 data$Latitude_abs_mean <- scale(x = data$Latitude_abs_mean)[,1]
 
 png(filename = "output/plots/SLOM_marck_all_variables.png", width = 15, height = 15, units = "in", res = 300)
data %>%   
   dplyr::select(lg_count,EA033,  Settlement_date_grouping_finer, Area_land ,Shoreline, ratio_coastline_to_area, Isolation, Latitude_abs_mean,Annual_temperature_mean, Temperature_seasonality_mean, Annual_precipitation_mean, Precipitation_seasonality_mean) %>% 
 pairs.panels(method = "pearson", # correlation method
              hist.col = "#a3afd1",# "#a9d1a3","",""),
              density = TRUE,  # show density plots
              ellipses = F, # show correlation ellipses
              cex.labels= 1,
              #           smoother= T,
              cor=T,
              lm=T,
              ci = T, 
              cex.cor = 2,stars = T)
 x <- dev.off()
 
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
  mutate(estimate_abs = abs(estimate))

anova_full_model <- anova(full_model, test = "Chisq") %>% 
  rownames_to_column("variable") %>% 
  mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
  mutate(interactions = str_count(variable, ":") + 1) %>% 
  arrange(-interactions, -`Pr(>Chi)`) %>% 
  mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>% 
  dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())

variable_highest_p_value <- anova_full_model[1,1]

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


