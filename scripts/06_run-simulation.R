# run simulation

source("scripts/05_simulation-setup.R")
source("scripts/05b_simulation-function.R")
source("scripts/05a_marked-helper-functions.R")

# if using pushover for notifications
source("scripts/00_pushover.R")


# simulate data and fit models ####

sim.results <- sims %>%
  mutate(model.results = pmap(list(years, error, phi, p), sim.and.model.trend.clean))
pushover("simulation complete")

# extract estimates ####
# type 1 = no data cleaning
# type 2 = remove all single observations

slopes <- sim.results %>% 
  unnest(model.results) %>% 
  mutate(type = rep(1:2, nrow(sims))) %>% 
  mutate(beta = map_dbl(model.results, find.beta),
         lcl = map_dbl(model.results, find.beta.lcl),
         ucl = map_dbl(model.results, find.beta.ucl))

phis <- sim.results %>% 
  unnest(model.results) %>% 
  mutate(type = rep(1:2, nrow(sims))) %>% 
  mutate(ests = map(model.results, extract.reals.trend)) %>% 
  select(-model.results) %>% 
  unnest(ests) %>% 
  filter(parm == "phi")

ps <- sim.results %>% 
  unnest(model.results) %>% 
  mutate(type = rep(1:2, nrow(sims))) %>% 
  mutate(ests = map(model.results, extract.reals.trend)) %>% 
  select(-model.results) %>% 
  unnest(ests) %>% 
  filter(parm == "p")

saveRDS(sim.results, "output/all-results-1000-FINAL.rds")
saveRDS(slopes, "output/slopes-1000-FINAL.rds")
saveRDS(phis, "output/phis-1000-FINAL.rds")
saveRDS(ps, "output/ps-1000-FINAL.rds")
pushover("results saved")
