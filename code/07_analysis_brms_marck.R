source("01_requirements.R")
source("01_requirements_brms.R")

data <- read_tsv("output/processed_data/RO_Hedvig_aggregate_marck_group_scaled.tsv", show_col_types = F) 



#inspiried by
#https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/counting-and-classification.html#binomial-regression


output <-
  brm(data = data, family = negbinomial,
      lg_count  ~  Annual_precipitation_mean * Precipitation_seasonality_mean +
        Annual_temperature_mean * Temperature_seasonality_mean +
        Latitude_abs_mean  +
        EA033 +  Isolation + 
        Shoreline * Settlement_date_grouping_finer +
        Area_land * Settlement_date_grouping_finer  +
        ratio_coastline_to_area* Settlement_date_grouping_finer,
      prior = c(prior(normal(0, 100), class = Intercept),
                prior(normal(0, 1), class = b)),
      iter = 3000, warmup = 1000, chains = 4, cores = 4,
      seed = 10) 