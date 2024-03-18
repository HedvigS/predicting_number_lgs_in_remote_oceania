#end of dropping out one for-loop

#df_all <- read_tsv(file = paste0("output/results/brms_", group, "_group_drop_one_out.tsv"))


df_all$dropped_obs <- fct_reorder(df_all$dropped_obs, df_all$diff_predicted_vs_observed_abs)

df_all %>% 
  ggplot() +
  geom_bar(aes(x = dropped_obs, y = diff_predicted_vs_observed_abs, fill = diff_predicted_vs_observed_abs), stat = "identity") +
  theme_fivethirtyeight() +
  theme(axis.text.x =  element_text(angle = 70, hjust = 1) , 
        legend.position = "none") +
  scale_fill_viridis(direction = -1)   +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"))

ggsave(filename = paste0("output/plots/brms_", group, "_control_", control, "_dropped_out_plot_diff.png"), width = 9, height = 9)
ggsave(filename = paste0("../latex/brms_", group, "_control_", control, "_dropped_out_plot_diff.png"), width = 9, height = 9)

df_all %>% 
  filter(diff_predicted_vs_observed_abs < 1.4) %>% 
  #  column_to_rownames("dropped_obs") %>% 
  data.table::transpose(make.names = "dropped_obs", keep.names = "variable") %>% 
  write_tsv(file = paste0("output/results/brms_", group, "_control_", control, "_dropped_effects_diff_below_1.4.tsv"), na = "")