source("requirements.R")

data <- read_tsv("output/sheets/RO_Hedvig_aggregate_medium_island.tsv") %>% 
  dplyr::filter(!is.na(Medium_only_merged_for_shared_language)) %>% 
  dplyr::select(group = Medium_only_merged_for_shared_language, everything()) %>% 
  dplyr::mutate(Latitude_abs_mean = abs(mean_lat)) %>% 
  filter(!is.na(mean_pol_complex))   %>% 
  rename(Annual_precipitation_mean = mean_CCSM_piControl_1760_bio12) %>% 
  rename(Precipitation_seasonality_mean = mean_CCSM_piControl_1760_bio15) %>% 
  rename(Annual_temperature_mean = mean_CCSM_piControl_1760_bio1) %>% 
  rename(Temperature_seasonality_mean = mean_CCSM_piControl_1760_bio4) %>% 
  rename(EA033 = mean_pol_complex) %>% 
  rename(Area_land = sum_area) %>% 
  rename(Shoreline = sum_shoreline) %>% 
  rename(Settlement_order_oldest = settlement_date_grouping_finer) %>% 
  rename(Isolation = dist) %>% 
  dplyr::select(group, lg_count, Latitude_abs_mean, Annual_precipitation_mean, Precipitation_seasonality_mean, Annual_temperature_mean, Temperature_seasonality_mean, EA033, Area_land, Shoreline, Isolation, Settlement_order_oldest, ratio_coastline_to_area)
#exclude least significant in anova, taking interactions first
  
#log10 size variables
data$Area_land <- log10(data$Area_land)
data$Shoreline <- log10(data$Shoreline)
data$ratio_coastline_to_area <- data$Shoreline / data$Area_land
data$Isolation <- log10(data$Isolation)

#normalise by standard deviation

data$Area_land <- scale(x = data$Area_land)
data$Shoreline <- scale(x = data$Shoreline)
data$ratio_coastline_to_area <- scale(x = data$ratio_coastline_to_area)
data$Isolation <- scale(x = data$Isolation)
data$Annual_precipitation_mean <- scale(x = data$Annual_precipitation_mean)
data$Precipitation_seasonality_mean <- scale(x = data$Precipitation_seasonality_mean)
data$Annual_temperature_mean <- scale(x = data$Annual_temperature_mean)
data$Temperature_seasonality_mean <- scale(x = data$Temperature_seasonality_mean)
data$Settlement_order_oldest <- scale(x = data$Settlement_order_oldest)
data$EA033 <- scale(x = data$EA033)
data$Latitude_abs_mean <- scale(x = data$Latitude_abs_mean)

data %>%
  dplyr::select(group, lg_count,EA033,  Settlement_order_oldest, Area_land ,Shoreline, ratio_coastline_to_area, Isolation, Latitude_abs_mean,Annual_temperature_mean, Temperature_seasonality_mean, Annual_precipitation_mean, Precipitation_seasonality_mean) %>% 
  write_tsv("output/sheets/RO_Hedvig_aggregate_medium_island_pruned.tsv")



##full model
full_model <- glm.nb(data = data, lg_count  ~   Latitude_abs_mean + 
                       Annual_precipitation_mean * Precipitation_seasonality_mean +
                       Annual_temperature_mean * Temperature_seasonality_mean +
                       EA033 + 
                       Isolation + 
                       #                       Net_Primary_Production_mean +
                       #                       Net_Primary_Production_Predictability_mean +
                       #                       Area_incl_water * Settlement_order_oldest +
                       Shoreline * Settlement_order_oldest +
                       Area_land * Settlement_order_oldest +
                     ratio_coastline_to_area* Settlement_order_oldest,  
                     control = list(maxit = 2500, epsilon = 1e-7, trace = F))# %>% 
#  stepAIC()


broomed_full_model <-  broom::tidy(full_model) %>% 
  mutate(estimate = round(as.numeric(estimate), digits = 7)) %>% 
  mutate(p.value = round(p.value, digit = 5)) %>% 
  mutate(statistic_abs = abs(statistic)) %>% 
  arrange(-statistic_abs) %>% 
  dplyr::select(term, estimate, p.value, statistic = statistic_abs) %>% 
  mutate(sig = ifelse(`p.value` <0.05, "Sig", "Non Sig")) 
  

anova_full_model <- anova(full_model, test = "Chisq") %>% 
  rownames_to_column("variable") %>% 
   mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
  mutate(interactions = str_count(variable, ":") + 1) %>% 
  arrange(-interactions, -`Pr(>Chi)`) %>% 
  mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>% 
  dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())

#View(anova_full_model)

anova_full_model[1,1]

model_1 <- update(full_model,  .~. -	Annual_precipitation_mean:Precipitation_seasonality_mean)

anova_model_1 <- anova(model_1, test = "Chisq") %>% 
  rownames_to_column("variable") %>% 
  mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
  mutate(interactions = str_count(variable, ":") + 1) %>% 
  arrange(-interactions, -`Pr(>Chi)`) %>% 
  mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>% 
  dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())

#View(anova_model_1)
anova_model_1[1,1]

model_2 <- update(model_1,  .~. -	Settlement_order_oldest:Area_land)

anova_model_2 <- anova(model_2, test = "Chisq") %>% 
  rownames_to_column("variable") %>% 
  mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
  mutate(interactions = str_count(variable, ":") + 1) %>% 
  arrange(-interactions, -`Pr(>Chi)`) %>% 
  mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>% 
  dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())

#View(anova_model_2)
anova_model_2[1,1]

model_3 <- update(model_2,  .~. -	Settlement_order_oldest:ratio_coastline_to_area)

anova_model_3 <- anova(model_3,test = "Chisq") %>% 
  rownames_to_column("variable") %>% 
  mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
  mutate(interactions = str_count(variable, ":") + 1) %>% 
  arrange(-interactions, -`Pr(>Chi)`) %>% 
  mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>% 
  dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())

#View(anova_model_3)
anova_model_3[1,1]

model_4 <- update(model_3,  .~. -	ratio_coastline_to_area)

anova_model_4 <- anova(model_4, test = "Chisq") %>% 
  rownames_to_column("variable") %>% 
  mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
  mutate(interactions = str_count(variable, ":") + 1) %>% 
  arrange(-interactions, -`Pr(>Chi)`) %>% 
  mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>% 
  dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())

#View(anova_model_4)
anova_model_4[1,1]

# model_5 <- update(model_4,  .~. -	Annual_temperature_mean)
# 
# anova_model_5 <- anova(model_5, test = "Chisq") %>%
#   rownames_to_column("variable") %>%
#   mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
#   mutate(interactions = str_count(variable, ":") + 1) %>%
#   arrange(-interactions, -`Pr(>Chi)`) %>%
#   mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>%
#   dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())
# 
# anova_model_5[1,1]
# 
# model_6 <- update(model_5,  .~. -Isolation)
# 
# anova_model_6 <- anova(model_6, test = "Chisq") %>%
#   rownames_to_column("variable") %>%
#   mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
#   mutate(interactions = str_count(variable, ":") + 1) %>%
#   arrange(-interactions, -`Pr(>Chi)`) %>%
#   mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>%
#   dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())
# 
# View(anova_model_6)
# anova_model_6[1,1]
# 
# 
# model_7 <- update(model_6,  .~. -Latitude_abs_mean)
# 
#  anova_model_7 <- anova(model_7, test = "Chisq") %>%
#    rownames_to_column("variable") %>%
#    mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
#    mutate(interactions = str_count(variable, ":") + 1) %>%
#    arrange(-interactions, -`Pr(>Chi)`) %>%
#    mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>%
#    dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())

# 
# # 
# # 
# 
# model_7 <- update(model_6,  .~. -EA033)
# 
# anova_model_7 <- anova(model_7, test = "Chisq") %>%
#   rownames_to_column("variable") %>%
#   mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
#   mutate(interactions = str_count(variable, ":") + 1) %>%
#   arrange(-interactions, -`Pr(>Chi)`) %>%
#   mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>%
#   dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())
# 
# #View(anova_model_7)
# 
# model_8 <- update(model_7,  .~. -	Area_land)

# anova_model_8 <- anova(model_8, test = "Chisq") %>% 
#   rownames_to_column("variable") %>% 
#   mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
#   mutate(interactions = str_count(variable, ":") + 1) %>% 
#   arrange(-interactions, -`Pr(>Chi)`) %>% 
#   mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>% 
#   dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())
# 
# #View(anova_model_8)
# 
# model_9 <- update(model_8,  .~. -Area_incl_water)
# 
# anova_model_9 <- anova(model_9, test = "Chisq") %>% 
#   rownames_to_column("variable") %>% 
#   mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
#   mutate(interactions = str_count(variable, ":") + 1) %>% 
#   arrange(-interactions, -`Pr(>Chi)`) %>% 
#   mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>% 
#   dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())
# 
# #View(anova_model_9)
# 
# model_10 <- update(model_9,  .~. -Settlement_order_oldest)
# 
# anova_model_10 <- anova(model_10, test = "Chisq") %>% 
#   rownames_to_column("variable") %>% 
#   mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
#   mutate(interactions = str_count(variable, ":") + 1) %>% 
#   arrange(-interactions, -`Pr(>Chi)`) %>% 
#   mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>% 
#   dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())
# 
# #View(anova_model_10)
# 
# model_11 <- update(model_10,  .~. -Net_Primary_Production_Predictability_mean)
# 
# anova_model_11 <- anova(model_11, test = "Chisq") %>% 
#   rownames_to_column("variable") %>% 
#   mutate(`Pr(>Chi)`= round(`Pr(>Chi)`, digits = 7)) %>%
#   mutate(interactions = str_count(variable, ":") + 1) %>% 
#   arrange(-interactions, -`Pr(>Chi)`) %>% 
#   mutate(sig = ifelse(`Pr(>Chi)` <0.05, "Sig", "Non Sig")) %>% 
#   dplyr::select(variable, sig, interactions, `Pr(>Chi)`, everything())
# 
# #View(anova_model_11)


###STOP

final_model <- model_4

summary(final_model)
modeva_pseudo_rs<- modEvA::RsqGLM(final_model)

modeva_pseudo_rs$McFadden
rsq(final_model)
pseudo_r = (final_model$null.deviance - final_model$deviance) / final_model$null.deviance

xtable(final_model, digits = 2, caption = c("GLM model for predicting language counts in island groups joined for shared language."), label = c("table:GLM_model_medium")) %>% 
  print() %>% 
  write_lines("output/sheets/glm_table_medium_model.txt")

broomed_final_model <-  broom::tidy(final_model) %>% 
  mutate(estimate = round(as.numeric(estimate), digits = 15)) %>% 
  mutate(p.value = round(p.value, digit = 15)) %>% 
  mutate(std.error = round(std.error, digit = 15)) %>% 
  mutate(statistic_abs = abs(statistic)) %>% 
  arrange(-statistic_abs) %>% 
  dplyr::select(term, estimate, p.value, `z-value` = statistic_abs, std.error) %>% 
  mutate(sig = ifelse(p.value <0.05, "Sig", "Non Sig"))

write_tsv(broomed_final_model, "output/sheets/broomed_model_medium_group.tsv")

lgs <- final_model$model %>% 
  left_join(data) 

predict_data_with_interaction <- predict(final_model, type = "response", se.fit = T) %>% 
   as.data.frame() %>% 
   cbind(lgs) %>% 
   dplyr::rename(Predicted_lg_count = "fit") %>%   
   mutate(diff = ((abs(lg_count - Predicted_lg_count))))

mean_diff <- round(mean(predict_data_with_interaction$diff), digits = 3)
 
predict_data_with_interaction$group <- fct_reorder(predict_data_with_interaction$group, abs(predict_data_with_interaction$lg_count))
 
predict_data_with_interaction %>% 
  dplyr::select(group, EA033, lg_count, Predicted_lg_count, diff, Shoreline, Settlement_order_oldest) %>% 
#  filter(EA033 == 1) %>% 
#  filter(Settlement_order_oldest >= 10) %>% 
  arrange(Shoreline) %>% 
  ggplot() +
  geom_point(aes(x=log10(Shoreline), y = lg_count)) +
  geom_label(mapping = aes(label = group, x=log10(Shoreline), y = lg_count))

predict_data_with_interaction_se_fit <- predict_data_with_interaction %>% 
  dplyr::select(se.fit, group)

medium_group_island_merged_with_interaction_plot  <-  predict_data_with_interaction %>% 
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
  theme(panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"), 
        plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF")) +
  ggtitle(label = paste0("Prediction of number of languages per island group"), subtitle= paste0("(Shared language groups). \nMean diff = ", mean_diff)) +
  scale_color_manual(values=wes_palette(n=2, name="Darjeeling1")) +
  theme(title = element_text(size = 12)) 
  
plot(medium_group_island_merged_with_interaction_plot)

ggsave("output/plots/medium_group_model_prediction.png", limitsize = F, width = 9, height = 10)
ggsave("../../Hedvigs_academia/Hedvigs PhD thesis/tex/illustrations/plots_from_R/medium_group_model_prediction.png", limitsize = F, width = 9, height = 10)




medium_group_island_merged_with_interaction_plot_log10  <-  predict_data_with_interaction %>% 
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
  theme(panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"), 
        plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF")) +
  ggtitle(label = paste0("Prediction of number of languages per island group"), subtitle= paste0("(Shared language groups) \nlog 10")) +
  scale_color_manual(values=wes_palette(n=2, name="Darjeeling1")) +
  theme(title = element_text(size = 12)) 

plot(medium_group_island_merged_with_interaction_plot_log10)
ggsave("output/plots/medium_group_model_prediction_log10.png", limitsize = F, width = 9, height = 10)
ggsave("../../Hedvigs_academia/Hedvigs PhD thesis/tex/illustrations/plots_from_R/medium_group_model_prediction_log10.png", limitsize = F, width = 9, height = 10)


