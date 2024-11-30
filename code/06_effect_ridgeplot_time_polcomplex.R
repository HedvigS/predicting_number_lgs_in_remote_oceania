source("01_requirements.R")

plot_time_pol_comples <- function(fns_full_chains = NULL, 
                                  fns_full_effects = NULL, 
                                  distinctive_plot_colors = NULL,
                                  models = NULL, xlim = NULL, 
                                  title = NULL){
  
  
  vars <- c(
    "moEA033:Shoreline",
    "moEA033",
    "moSettlement_date_grouping_finer", 
    "moSettlement_date_grouping_finer:Shoreline")
  
df <- data.frame(chain = as.numeric(), 
                 variable = as.character(), 
                 value = as.numeric(), 
                 model = as.character(),
                 straddle_zero_95 = as.character())



for(i in 1:4){

#  i <- 2
  
chain_joined <- read_tsv(fns_full_chains[i], show_col_types = F) %>% 
  mutate(model = models[i])

colnames(chain_joined) <- str_replace_all(colnames(chain_joined), "b_", "")  
colnames(chain_joined) <- str_replace_all(colnames(chain_joined), "bsp_", "")  

full_effects_table <- read_tsv(fns_full_effects[i], show_col_types = F)%>% 
  mutate(model = models[i]) %>% 
  dplyr::filter(term %in% vars) %>% 
  dplyr::filter(variable == "straddle_zero_95") %>% 
  dplyr::select(-variable) %>% 
  dplyr::select(straddle_zero_95 = value, variable = term, model)

df_spec <-  chain_joined %>% 
  reshape2::melt(id.vars = c("chain", "model")) %>%  
  dplyr::filter(variable %in% vars) %>% 
  left_join(full_effects_table, by = join_by(variable, model)) %>% 
  mutate(variable = str_replace_all(variable,":", ":\n")) %>% 
  mutate(variable = str_replace_all(variable,"moEA", "EA")) %>% 
  mutate(variable = str_replace_all(variable,"moSettlement_date_grouping_finer", "Time depth")) %>% 
  mutate(value = as.numeric(value))

df <- df %>% 
  full_join(df_spec, by = join_by(chain, variable, value, model, straddle_zero_95))

}





p <- df %>% 
  ggplot(aes(x = value, fill = model, 
             color = variable, y = model)) + 
      ggridges::geom_density_ridges(bandwidth = 0.035,# scale=1.4,
                                    mapping = aes(alpha = as.factor(straddle_zero_95),
                                                  linetype = as.factor(straddle_zero_95)
),
                   color = "darkgray",
                   linewidth = 0.6)   +
   lemon::facet_rep_wrap(~variable, 
                         #ncol = 3, 
                         #             scales = "free",
                         repeat.tick.labels = c('bottom'))  +
   suppressWarnings(  scale_alpha_discrete(range = c(0.75, 0.2)) ) +
  geom_vline(aes(xintercept = 0), linetype="dashed", color = "darkgray", alpha = 0.7) + 
   scale_color_manual(values = distinctive_plot_colors) +
   scale_fill_manual(values = distinctive_plot_colors) +

  theme_classic() +
  theme(legend.position = "none", 
        text = element_text(size = 14),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank()) +
  coord_cartesian(xlim = xlim) +
  ggtitle(title)
 p 
}

fns_full_chains <- c("output/results/brms_medium_control_spatialphylo_full_chains.tsv",
                     "output/results/brms_medium_control_spatial_full_chains.tsv",
                     "output/results/brms_medium_control_phylo_full_chains.tsv", 
                     "output/results/brms_medium_control_none_full_chains.tsv")

fns_full_effects <- c(                     "output/results/brms_medium_control_spatialphylo_full_effects_table.tsv",
                                           "output/results/brms_medium_control_spatial_full_effects_table.tsv", 
                                           "output/results/brms_medium_control_phylo_full_effects_table.tsv", 
                                           "output/results/brms_medium_control_none_full_effects_table.tsv")

models <- c("spatialphylo",
            "spatial",
            "phylo",
            "none")


plot_time_pol_comples(models = models, fns_full_chains = fns_full_chains, fns_full_effects = fns_full_effects, 
                      distinctive_plot_colors =  c(  "#87CEEB", "#5f99f5","#68b8cc", "#7DDAD9"), xlim = c(-3, 3))

ggsave(file = paste0("output/results/brms_medium_ridge_panels_plot_time_pol_complex.png"), height = 5, width = 5, dpi = 200)

ggsave(file = paste0("../latex/brms_medium_ridge_panels_plot_time_pol_complex.png"), height = 5, width = 5, dpi = 200)


fns_full_chains <- c("output/results/brms_SBZR_control_spatialphylo_full_chains.tsv",
                     "output/results/brms_SBZR_control_spatial_full_chains.tsv",
                     "output/results/brms_SBZR_control_phylo_full_chains.tsv", 
                     "output/results/brms_SBZR_control_none_full_chains.tsv")

fns_full_effects <- c(                     "output/results/brms_SBZR_control_spatialphylo_full_effects_table.tsv",
                                           "output/results/brms_SBZR_control_spatial_full_effects_table.tsv", 
                                           "output/results/brms_SBZR_control_phylo_full_effects_table.tsv", 
                                           "output/results/brms_SBZR_control_none_full_effects_table.tsv")

models <- c("spatialphylo",
            "spatial",
            "phylo",
            "none")

plot_time_pol_comples(models = models, fns_full_chains = fns_full_chains, fns_full_effects = fns_full_effects, 
                      distinctive_plot_colors =  c("#FFDAB9","#FFB6C1" ,   "#C8A2C8", "#ebb0df"), xlim = c(-4, 4))


ggsave(file = paste0("output/results/brms_SBZR_ridge_panels_plot_time_pol_complex.png"), height = 5, width = 5, dpi = 200)

ggsave(file = paste0("../latex/brms_SBZR_ridge_panels_plot_time_pol_complex.png"), height = 5, width = 5, dpi = 200)
