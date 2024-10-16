source("01_requirements.R")


df <- SBZR_none_summary <- readRDS("output/results/brms_SBZR_control_none_model_summary_full.rds")$mo %>% 
  rownames_to_column("term") %>% 
  mutate(model = "SBZR_none")

df <- readRDS("output/results/brms_SBZR_control_spatial_model_summary_full.rds")$mo %>% 
  rownames_to_column("term") %>% 
  mutate(model = "SBZR_spatial") %>% 
    full_join(df, by = join_by(term, Estimate, Est.Error, `l-95% CI`, `u-95% CI`, Rhat, Bulk_ESS, Tail_ESS, model))

df <- readRDS("output/results/brms_SBZR_control_phylo_model_summary_full.rds")$mo %>% 
  rownames_to_column("term") %>% 
  mutate(model = "SBZR_phylo") %>% 
  full_join(df, by = join_by(term, Estimate, Est.Error, `l-95% CI`, `u-95% CI`, Rhat, Bulk_ESS, Tail_ESS, model))

df <- readRDS("output/results/brms_SBZR_control_spatialphylo_model_summary_full.rds")$mo %>% 
  rownames_to_column("term") %>% 
  mutate(model = "SBZR_sp") %>% 
  full_join(df, by = join_by(term, Estimate, Est.Error, `l-95% CI`, `u-95% CI`, Rhat, Bulk_ESS, Tail_ESS, model))

df <- medium_none_summary <- readRDS("output/results/brms_medium_control_none_model_summary_full.rds")$mo %>% 
  rownames_to_column("term") %>% 
  mutate(model = "medium_none")   %>% 
  full_join(df, by = join_by(term, Estimate, Est.Error, `l-95% CI`, `u-95% CI`, Rhat, Bulk_ESS, Tail_ESS, model))

df <- readRDS("output/results/brms_medium_control_spatial_model_summary_full.rds")$mo %>% 
  rownames_to_column("term") %>% 
  mutate(model = "medium_spatial") %>% 
  full_join(df, by = join_by(term, Estimate, Est.Error, `l-95% CI`, `u-95% CI`, Rhat, Bulk_ESS, Tail_ESS, model))

df <- readRDS("output/results/brms_medium_control_phylo_model_summary_full.rds")$mo %>% 
  rownames_to_column("term") %>% 
  mutate(model = "medium_phylo") %>% 
  full_join(df, by = join_by(term, Estimate, Est.Error, `l-95% CI`, `u-95% CI`, Rhat, Bulk_ESS, Tail_ESS, model))

df <- readRDS("output/results/brms_medium_control_spatialphylo_model_summary_full.rds")$mo %>% 
  rownames_to_column("term") %>% 
  mutate(model = "medium_sp") %>% 
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

