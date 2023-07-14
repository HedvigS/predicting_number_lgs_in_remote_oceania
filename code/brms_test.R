library(tidyverse)

library(rethinking)
data(Kline)
d <- Kline

detach(package:rethinking, unload = T)

library(brms)
rm(Kline)

d

d <-  d %>%
  mutate(log_pop      = log(population),
         contact_high = ifelse(contact == "high", 1, 0))

b10.10 <-  brm(data = d, family = poisson,
      total_tools ~ 1 + log_pop + contact_high + contact_high:log_pop,
      prior = c(prior(normal(0, 100), class = Intercept),
                prior(normal(0, 1), class = b)),
      iter = 3000, warmup = 1000, chains = 4, cores = 4,
      seed = 13,
      sample_prior = "yes",
      save_pars = save_pars(all = T),
      backend="cmdstanr") 


summary(b10.10)

b10.10

