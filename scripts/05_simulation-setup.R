# simulation study

# what is the effect of misreads on estimates of apparent annual survival?
# define simulation parameters and create tibble to store results

# misread error rates to evaluate
error.rates = c(0, 0.005, 0.01, 0.05, 0.1)

# study lengths to evaluate
yrs = c(5, 10, 20)

# true survival probability
phi = 0.8

# true detection probability
p = 0.5

# number of individuals marked each year
n.ind = 250

# number of replicates of each scenario
reps = 1000

scenarios = as.tibble(expand.grid(years = yrs,
                                  error = error.rates))
scenarios$scenario = c(1:nrow(scenarios))


sims = as.tibble(expand.grid(years = yrs,
                             error = error.rates,
                             replicate = c(1:reps)))
sims %>%
  mutate(id = 1:nrow(sims)) %>% 
  full_join(scenarios) -> sims

head(sims)


