source("01_requirements.R")
source("01_requirements_brms.R")

###SBZR
data <- read_tsv("output/processed_data/RO_aggregate_SBZR_group_scaled.tsv", show_col_types = F) %>%  
  rename(group = SBZR_group) %>% 
  mutate(spatial_id = group) %>% 
  mutate(phylo_id = group)

group = "SBZR"

phylo_vcv <- readRDS("output/processed_data/tree_SBZR_vcv.rds")
spatial_vcv <- readRDS("output/processed_data/spatial_vcv_SBZR.rds")
data2 = list(phylo_vcv = phylo_vcv, spatial_vcv = spatial_vcv)

formula <- lg_count  ~    (1 | gr(phylo_id, cov = phylo_vcv)) +
  (1 | gr(spatial_id, cov = spatial_vcv)) +
  environ_PC1*Shoreline +
  environ_PC2*Shoreline + 
  Shoreline*mo(EA033) + 
  Shoreline*mo(Settlement_date_grouping_finer)


iter = 30000
warmup = 1000
chains = 4
cores = 4
seed = 10
ndraws = 10000

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


ms_full <- summary(output_poisson)



### model output
chain_1 <- output_poisson$fit@sim$samples[[1]] %>% as.data.frame()  %>% mutate(chain = "1")
chain_2 <- output_poisson$fit@sim$samples[[2]] %>% as.data.frame()  %>% mutate(chain = "2")
chain_3 <- output_poisson$fit@sim$samples[[3]] %>% as.data.frame()  %>% mutate(chain = "3")
chain_4 <- output_poisson$fit@sim$samples[[4]] %>% as.data.frame()  %>% mutate(chain = "4")

chain_joined <- suppressMessages(full_join(chain_1, chain_2)) %>% 
  suppressMessages(full_join(chain_3)) %>%
  suppressMessages(full_join(chain_4))

df <- chain_joined %>% 
  reshape2::melt(id.vars = "chain") %>%   
  filter(str_detect(variable, "sd_phylo"))

df$value %>% hist()