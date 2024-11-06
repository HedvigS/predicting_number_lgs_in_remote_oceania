source("01_requirements.R")

df <- SBZR_none_summary <- readRDS("output/results/brms_SBZR_control_none_model_summary_full.rds")$mo %>% 
  rownames_to_column("term") %>% 
  mutate(model = "Overnight-sailing distances\nisland group ~ none")

df <- readRDS("output/results/brms_SBZR_control_spatial_model_summary_full.rds")$mo %>% 
  rownames_to_column("term") %>% 
  mutate(model = "Overnight-sailing distances\nisland group ~ spatial") %>% 
    full_join(df, by = join_by(term, Estimate, Est.Error, `l-95% CI`, `u-95% CI`, Rhat, Bulk_ESS, Tail_ESS, model))

df <- readRDS("output/results/brms_SBZR_control_phylo_model_summary_full.rds")$mo %>% 
  rownames_to_column("term") %>% 
  mutate(model = "Overnight-sailing distances\nisland group ~ phylo") %>% 
  full_join(df, by = join_by(term, Estimate, Est.Error, `l-95% CI`, `u-95% CI`, Rhat, Bulk_ESS, Tail_ESS, model))

df <- readRDS("output/results/brms_SBZR_control_spatialphylo_model_summary_full.rds")$mo %>% 
  rownames_to_column("term") %>% 
  mutate(model = "Overnight-sailing distances\nisland group ~ spatialphylo") %>% 
  full_join(df, by = join_by(term, Estimate, Est.Error, `l-95% CI`, `u-95% CI`, Rhat, Bulk_ESS, Tail_ESS, model))

df <- medium_none_summary <- readRDS("output/results/brms_medium_control_none_model_summary_full.rds")$mo %>% 
  rownames_to_column("term") %>% 
  mutate(model = "Shared language\nisland group ~ none")   %>% 
  full_join(df, by = join_by(term, Estimate, Est.Error, `l-95% CI`, `u-95% CI`, Rhat, Bulk_ESS, Tail_ESS, model))

df <- readRDS("output/results/brms_medium_control_spatial_model_summary_full.rds")$mo %>% 
  rownames_to_column("term") %>% 
  mutate(model = "Shared language\nisland group ~ spatial") %>% 
  full_join(df, by = join_by(term, Estimate, Est.Error, `l-95% CI`, `u-95% CI`, Rhat, Bulk_ESS, Tail_ESS, model))

df <- readRDS("output/results/brms_medium_control_phylo_model_summary_full.rds")$mo %>% 
  rownames_to_column("term") %>% 
  mutate(model = "Shared language\nisland group ~ phylo") %>% 
  full_join(df, by = join_by(term, Estimate, Est.Error, `l-95% CI`, `u-95% CI`, Rhat, Bulk_ESS, Tail_ESS, model))

df <- readRDS("output/results/brms_medium_control_spatialphylo_model_summary_full.rds")$mo %>% 
  rownames_to_column("term") %>% 
  mutate(model = "Shared language\nisland group ~ spatialphylo") %>% 
  full_join(df, by = join_by(term, Estimate, Est.Error, `l-95% CI`, `u-95% CI`, Rhat, Bulk_ESS, Tail_ESS, model))

df %>% 
  filter(term == "moEA0331[1]"|
           term == "moEA0331[2]"|
           term == "moEA0331[3]") %>% 
  ggplot() +
  geom_point(aes(x = term, y = Estimate, color = model, group = model)) +
  geom_line(aes(x = term, y = Estimate, color = model, group = model)) +
  theme(axis.text = element_text(angle = 40, hjust = 1))

ggsave("output/plots/simplex_effects_EA033.png", width = 6, height = 6)

df_time <- df %>% 
  filter(term == "moSettlement_date_grouping_finer1[1]"|
           term == "moSettlement_date_grouping_finer1[2]"|
           term == "moSettlement_date_grouping_finer1[3]"|
           term == "moSettlement_date_grouping_finer1[4]"|
           term == "moSettlement_date_grouping_finer1[5]"|
           term == "moSettlement_date_grouping_finer1[6]"|
           term == "moSettlement_date_grouping_finer1[7]"|
           term == "moSettlement_date_grouping_finer1[8]"|
           term == "moSettlement_date_grouping_finer1[9]"|
           term == "moSettlement_date_grouping_finer1[10]"
     ) 

df_time$term <- factor(df_time$term, levels = c("moSettlement_date_grouping_finer1[1]",
                                                "moSettlement_date_grouping_finer1[2]",
                                                "moSettlement_date_grouping_finer1[3]",
                                                "moSettlement_date_grouping_finer1[4]",
                                                "moSettlement_date_grouping_finer1[5]",
                                                "moSettlement_date_grouping_finer1[6]",
                                                "moSettlement_date_grouping_finer1[7]",
                                                "moSettlement_date_grouping_finer1[8]",
                                                "moSettlement_date_grouping_finer1[9]",
                                                "moSettlement_date_grouping_finer1[10]")

)                         

df_time %>% 
  ggplot() +
  geom_point(aes(x = term, y = Estimate, color = model, group = model)) +
  geom_line(aes(x = term, y = Estimate, color = model, group = model)) +
  theme(axis.text = element_text(angle = 40, hjust = 1))

ggsave("output/plots/simplex_effects_time.png", width = 6, height = 6)



#NEW PREDICTIONS

nd <- read_tsv("output/results/brms_medium_control_phylo_epreds.tsv", show_col_types = F) %>% 
  mutate(model = "Shared language\nisland group ~ phylo")

nd <- read_tsv("output/results/brms_medium_control_none_epreds.tsv", show_col_types = F) %>% 
  mutate(model = "Shared language\nisland group ~ none") %>% 
  full_join(nd, by = join_by(rowname, EA033, Settlement_date_grouping_finer, environ_PC1, environ_PC2, environ_PC3, Shoreline,
                             mean_posterior_predict_nd, model))

nd <- read_tsv("output/results/brms_medium_control_spatial_epreds.tsv", show_col_types = F) %>% 
  mutate(model = "Shared language\nisland group ~ spatial") %>% 
  full_join(nd, by = join_by(rowname, EA033, Settlement_date_grouping_finer, environ_PC1, environ_PC2,  environ_PC3,Shoreline,
                             mean_posterior_predict_nd, model))

nd <- read_tsv("output/results/brms_medium_control_spatialphylo_epreds.tsv", show_col_types = F) %>% 
  mutate(model = "Shared language\nisland group ~ spatialphylo") %>% 
  full_join(nd, by = join_by(rowname, EA033, Settlement_date_grouping_finer, environ_PC1, environ_PC2,  environ_PC3,Shoreline,
                             mean_posterior_predict_nd, model))

nd <- read_tsv("output/results/brms_SBZR_control_phylo_epreds.tsv", show_col_types = F) %>% 
  mutate(model = "Overnight-sailing distances\nisland group ~ phylo") %>% 
  full_join(nd, by = join_by(rowname, EA033, Settlement_date_grouping_finer, environ_PC1, environ_PC2, Shoreline,
                             mean_posterior_predict_nd, model))

nd <- read_tsv("output/results/brms_SBZR_control_none_epreds.tsv", show_col_types = F) %>% 
  mutate(model = "Overnight-sailing distances\nisland group ~ none") %>% 
  full_join(nd, by = join_by(rowname, EA033, Settlement_date_grouping_finer, environ_PC1, environ_PC2, Shoreline,
                             mean_posterior_predict_nd, model))

nd <- read_tsv("output/results/brms_SBZR_control_spatial_epreds.tsv", show_col_types = F) %>% 
  mutate(model = "Overnight-sailing distances\nisland group ~ spatial") %>% 
  full_join(nd, by = join_by(rowname, EA033, Settlement_date_grouping_finer, environ_PC1, environ_PC2, Shoreline,
                             mean_posterior_predict_nd, model))

nd <- read_tsv("output/results/brms_SBZR_control_spatialphylo_epreds.tsv", show_col_types = F) %>% 
  mutate(model = "Overnight-sailing distances\nisland group ~ spatialphylo") %>% 
  full_join(nd, by = join_by(rowname, EA033, Settlement_date_grouping_finer, environ_PC1, environ_PC2, Shoreline,
                             mean_posterior_predict_nd, model))

nd %>% 
  filter(model == "Shared language\nisland group ~ phylo"|
           model == "Shared language\nisland group ~ spatial"|
           model == "Overnight-sailing distances\nisland group ~ spatial") %>% 
  dplyr::rename(`Time depth` =`Settlement_date_grouping_finer`) %>% 
  ggplot() +
  geom_point(aes(x = EA033, y = mean_posterior_predict_nd, color = `Time depth` )) +
  facet_grid(~model) +
  theme_fivethirtyeight() +
  scale_color_viridis() +
theme(axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 8),
      legend.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "#ebeced"))+
  xlab("EA033 (political complexity)") +
  ylab("Predicted number of languages (new simulated data)") 
  
ggsave("../latex/epreds_nd_EA033.png", width = 9, height = 7)

nd %>% 
  filter(model == "Overnight-sailing distances\nisland group ~ spatial"|
           model == "Overnight-sailing distances\nisland group ~ phylo"|
           model == "Overnight-sailing distances\nisland group ~ spatialphylo"|
           model == "Shared language\nisland group ~ spatial") %>% 
  ggplot() +
  geom_point(aes(x = as.factor(Settlement_date_grouping_finer), y = mean_posterior_predict_nd, color = as.factor(EA033))) +
  theme_fivethirtyeight() +
  scale_color_viridis(discrete = T) +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 8),
        legend.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "#ebeced"))+
  xlab("Time depth") +
  ylab("Predicted number of languages (new simulated data)") + 
  guides(color=guide_legend(title="EA033 (political complexity)")) +
  facet_grid(~model) 
  

ggsave("../latex/epreds_nd_time.png", width = 9, height = 7)



#chain_joined %>% 
#  write_tsv(file = paste0("output/results/brms_", group, "_control_", control, "_full_chains.tsv"), na = "")
