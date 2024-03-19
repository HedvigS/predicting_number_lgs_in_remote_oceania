source("01_requirements.R")
source("fun_def_combine_tsvs.R")

#SBZR phylo
fns <- list.files(path = "output/results/drop_one_out/SBZR_phylo/", pattern = "diff_means", recursive = T, full.names = T)

diff_means_SBZR_phylo <- combine_tsvs(fns = fns) %>% 
  mutate(island_group_dropped = str_extract(filename, "phylo/.*/diff")) %>% 
  mutate(island_group_dropped = str_replace_all(island_group_dropped, "phylo", "")) %>% 
  mutate(island_group_dropped = str_replace_all(island_group_dropped, "diff", "")) %>% 
  mutate(island_group_dropped = str_replace_all(island_group_dropped, "/", "")) %>% 
  mutate(island_group_dropped = str_replace_all(island_group_dropped, "_", " ")) %>% 
  mutate(group = "SBZR") %>% 
  mutate(control = "phylo")

#SBZR none
fns <- list.files(path = "output/results/drop_one_out/SBZR_none//", pattern = "diff_means", recursive = T, full.names = T)

diff_means_SBZR_none <- combine_tsvs(fns = fns) %>% 
  mutate(island_group_dropped = str_extract(filename, "none/.*/diff")) %>% 
  mutate(island_group_dropped = str_replace_all(island_group_dropped, "none", "")) %>% 
  mutate(island_group_dropped = str_replace_all(island_group_dropped, "diff", "")) %>% 
  mutate(island_group_dropped = str_replace_all(island_group_dropped, "/", "")) %>% 
  mutate(island_group_dropped = str_replace_all(island_group_dropped, "_", " ")) %>% 
  mutate(group = "SBZR") %>% 
  mutate(control = "none")

#medium spatial
fns <- list.files(path = "output/results/drop_one_out/medium_spatial/", pattern = "diff_means", recursive = T, full.names = T)

diff_means_medium_spatial <- combine_tsvs(fns = fns) %>% 
  mutate(island_group_dropped = str_extract(filename, "spatial/.*/diff")) %>% 
  mutate(island_group_dropped = str_replace_all(island_group_dropped, "spatial", "")) %>% 
  mutate(island_group_dropped = str_replace_all(island_group_dropped, "diff", "")) %>% 
  mutate(island_group_dropped = str_replace_all(island_group_dropped, "/", "")) %>% 
  mutate(island_group_dropped = str_replace_all(island_group_dropped, "_", " ")) %>% 
    mutate(group = "medium") %>% 
  mutate(control = "spatial")

#medium none
fns <- list.files(path = "output/results/drop_one_out/medium_none//", pattern = "diff_means", recursive = T, full.names = T)

diff_means_medium_none <- combine_tsvs(fns = fns) %>% 
  mutate(island_group_dropped = str_extract(filename, "none/.*/diff")) %>% 
  mutate(island_group_dropped = str_replace_all(island_group_dropped, "none", "")) %>% 
  mutate(island_group_dropped = str_replace_all(island_group_dropped, "diff", "")) %>% 
  mutate(island_group_dropped = str_replace_all(island_group_dropped, "/", "")) %>% 
  mutate(island_group_dropped = str_replace_all(island_group_dropped, "_", " ")) %>% 
  mutate(group = "medium") %>% 
  mutate(control = "none")


plot_diff_cols <- function(df = NULL){


#df <- diff_means_medium_spatial
control <- unique(df$control) %>% as.character()
group <- unique(df$group) %>% as.character()

#plotting
df$island_group_dropped <- fct_reorder(df$island_group_dropped, df$diff_poisson_abs)

df %>% 
  ggplot() +
  geom_bar(aes(x = island_group_dropped, y = diff_poisson_abs, fill = diff_poisson_abs), stat = "identity") +
  theme_fivethirtyeight() +
  theme(axis.text.x =  element_text(angle = 70, hjust = 1) , 
        legend.position = "none") +
  scale_fill_viridis(direction = -1)   +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white")) +
  ylim(c(0, 1.5))
  

ggsave(filename = paste0("output/plots/brms_", group, "_control_", control, "_dropped_out_plot_diff.png"), width = 9, height = 9)
ggsave(filename = paste0("../latex/brms_", group, "_control_", control, "_dropped_out_plot_diff.png"), width = 9, height = 9)

}

plot_diff_cols(df = diff_means_medium_none)
plot_diff_cols(df = diff_means_medium_spatial)
plot_diff_cols(df = diff_means_SBZR_none)
plot_diff_cols(df = diff_means_SBZR_phylo)