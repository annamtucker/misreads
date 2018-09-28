# data cleaning

# 29 aug 18
# update to include 2017 and 2018

source("scripts/00_load-packages.R")

# subset out 3-character lime flags on red knot from 2009-2016
# clean up observer names
# write cleaned data to output file

# files ####
input = "data/resights_DE.csv"
output = "output/resights_clean.csv"

withheld = read.csv("data/withheld_flags.csv", sep = "\t", stringsAsFactors = F)
first = read.csv("output/first-flagged.csv", stringsAsFactors = F)

# clean up data ####
obs.complete = read_csv(input)

obs <- obs.complete %>%
  rename(col = `Flag Color`, code = `Flag Code`, resight.date = `Resight Date`,
         obs.first = `Observer First`, obs.last = `Observer Last`) %>% 
  filter(Species == "REKN" & col == "FELG") %>%
  mutate(date = parse_date(resight.date, format = "%m/%d/%Y"),
         Obs = paste(substr(obs.first,1,1), obs.last),
         JDay = yday(date),
         Year = year(date),
         Day = ifelse(Year %in% c(2008, 2012, 2016),
                      JDay - 121, JDay - 120),
         Flag.Code = substr(code, 3, nchar(code)-1)) %>% 
  select(Year, Day, date, Obs, Species, col, Flag.Code) %>% 
  filter(Year > 2008 & nchar(Flag.Code) == 3) 


# clean up observer names
allobs = unique(obs$Obs)

# visual check for errors
problemobs = allobs[c(18, 19, 26, 46, 160, 198, 207)]
problemobs

# remove multiple observer/NAs
obs = filter(obs, !(Obs %in% problemobs[c(4,5)]))

# give Nigel, Jacquie, Richard, and Conor their resights back
obs$Obs[obs$Obs == problemobs[1]] = "N Clark"
obs$Obs[obs$Obs %in% problemobs[c(2,3)]] = "J Clark"
obs$Obs[obs$Obs == problemobs[6]] = "R Du Feu"
obs$Obs[obs$Obs == problemobs[7]] = "C McGowan"

# change case to match others
obs$Obs[obs$Obs == "M BENNETT"] = "M Bennett"

head(obs)

for(i in 1:nrow(obs)){
  flag = obs$Flag.Code[i]
  yr = obs$Year[i]
  
  df = subset(obs, Flag.Code == flag)
  
  obs$nyrs_lastseen[i] = ifelse(length(which(df$Year<yr)) == 0, 0,
                                    yr - max(df$Year[which(df$Year<yr)]))
}

obs.sum <- obs %>% 
  group_by(Year, Flag.Code) %>% 
  summarize(n_resights = length(Obs),
            n_observers = length(unique(Obs)),
            n_years = unique(nyrs_lastseen))  %>% 
  ungroup() 

write.csv(obs.sum, "output/flag-year-obs-sum.csv", row.names = F)


hist(obs.sum$n_resights, breaks = 100, freq = F)
lines(dnbinom(c(1:60), size = 1, mu = 5))


obs.sum %>% 
  count(n_resights) %>% 
  mutate(dens = dnbinom(n_resights, size = 1, mu = 5),
         prop = n/max(n)) %>% 
  ggplot(aes(x = n_resights)) +
  geom_bar(aes(y = n), stat = "identity", width = 1,
           fill = "gray60", col = "black") +
  xlab("Number of resightings in one year") +
  ylab("Number of individuals") +
  scale_x_continuous(breaks = seq(0, 60, 5)) 

save_plot(nresight, file = "figs/nresights-hist.jpg",
          base_width = 6)


ggplot(obs.sum, aes(x = n_observers)) +
  geom_histogram(binwidth = 1, fill = "gray60", col = "black") +
  xlab("Number of observers in one year") +
  ylab("Number of individuals") +
  scale_x_continuous(breaks = seq(0, 60, 5)) 



fitdist = cat(file = "scripts/fitdist.jags", "
model{
  
  for(i in 1:n){
    lambda[i] ~ dgamma(sh, ra)
    nresights[i] ~ dpois(lambda[i])
  }

  # separate gamma and poisson
  sh <- 1 + m*ra
  ra <- (m + sqrt(m^2 + 4*sd^2))/(2*sd^2)
  m ~ dunif(0, 100)
  sd ~ dunif(0, 100)

  # probability of one obs
  for(i in 1:n){
    ran[i] ~ dpois(lambda[i])
    p1[i] <- equals(ran[i], 1)
  }
  

}
              
")

dat = list(nresights = obs.sum$n_resights,
           n = nrow(obs.sum))

mod = jags(dat, inits = NULL, model.file = "scripts/fitdist.jags",
           parameters.to.save = c("sh", "ra", "m", "sd", "p1"),
           n.chains = 3, n.iter = 1000, n.burnin = 500, n.thin = 1)

# overdispersed poisson
l = rgamma(10000, shape = 1.3, rate = 0.25)
hist(rpois(10000, l), breaks = 50)

hist(obs.sum$n_resights, breaks =50, freq = F)
lines(density(rpois(1000, l)), col = "blue")

navail = c(round(runif(1000, 1, 60)), round(runif(1000, 1, 10))) 
navail = round(runif(1000, 1, 60))
nobs = rbinom(2000, navail, 0.13)
hist(nobs)
mean(nobs)
mean(nobs == 1)


# probability of nobs = 1
mean(mod$sims.list$p1)

# calculate error rate ####

# confirmed = withheld and/or not-yet-deployed
# possible = only seen once in a given year

first %>%
  filter(Species == "REKN" & Flag.Color.Out == "FELG") %>% 
  mutate(year = year(firstcap)) -> first


# identify potential misreads
obs %>%
  group_by(Flag.Code, Year) %>%
  mutate(SingleObs = ifelse(length(Day) == 1, 1, 0),
         Withheld = ifelse(Flag.Code %in% withheld$Code, 1, 0),
         NotDeployed = NA) %>%
  ungroup() -> obs

# for flags deployed by DE, seen before year of deployment?
for(i in 1:nrow(obs)){
  if(obs$Flag.Code[i] %in% first$Flag.Code.Out){
    obs$NotDeployed[i] = ifelse(obs$Year[i] < 
                                   first$year[first$Flag.Code.Out == obs$Flag.Code[i]], 1, 0)
  }
}

# assign code to each observer for anonymity
obs %>%
  distinct(Obs) %>%
  mutate(ObsCode = c(1:length(Obs))) %>%
  full_join(obs, by = "Obs") %>% 
  dplyr::select(-Obs) -> obs

obs %>% 
  full_join(obs.sum) %>% 
  dplyr::select(-nyrs_lastseen) -> obs.final

# write to output file
write.csv(obs.final, output, row.names = F)

