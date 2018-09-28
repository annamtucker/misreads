# exploratory data analysis

obs = read.csv("output/resights_clean.csv")

# overall misreads
obs %>%
  summarize(tot = length(Flag.Code),
            withheld = sum(Withheld, na.rm = T),
            singleobs = sum(SingleObs, na.rm = T),
            notdeployed = sum(NotDeployed, na.rm = T),
            knowndep = length(which(!is.na(NotDeployed))),
            err.w = withheld/tot,
            err.n = notdeployed/knowndep,
            err.min = (withheld + notdeployed)/tot,
            err.max = singleobs/tot) 


# number of observers with impossible resightings
obs %>% 
  filter(Withheld == 1) %>% 
  summarize(n = length(unique(ObsCode)))

obs %>% 
  filter(NotDeployed == 1) %>% 
  summarize(n = length(unique(ObsCode)))


# year-specific
obs %>%
  group_by(Year) %>%
  summarize(Nobservers = length(unique(ObsCode)),
            Nrekn = length(unique(Flag.Code)),
            Nflags = length(Flag.Code),
            avgresights = Nflags/Nrekn,
            imposs = sum(Withheld) + sum(NotDeployed, na.rm = T),
            susp  = sum(SingleObs),
            error.min = imposs/Nflags,
            error.max = susp/Nflags) %>%
  ungroup() -> year.summary

year.summary

write.csv(year.summary, "output/year_summary.csv")
