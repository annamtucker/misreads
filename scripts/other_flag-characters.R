# flag-specific factors contributing to misreads

obs = read.csv("output/resights_de_08_16_clean.csv")
first = read.csv("data/firstflagged.csv", stringsAsFactors = F)


# all resightings ####

first %>%
  filter(year < 2017) %>%
  mutate(char1 = substring(Flag.Code.Out, 1, 1),
         char2 = substring(Flag.Code.Out, 2, 2),
         char3 = substring(Flag.Code.Out, 3, 3)) %>%
  dplyr::select(Flag.Code.Out,char1, char2, char3) %>%
  gather(loc, char, 2:4) %>%
  count(char) %>%
  mutate(tot = sum(n),
         prop = n/tot,
         check = ifelse(char %in% c("B", "D", "F", "G", "O", "I",
                                    "Q", "R", "S", "W", "Z"), 
                        1, 0)) %>%
  select(char,n, tot, prop) %>%
  rename(n.deployed = n, prop.deployed=prop, tot.deployed = tot)->
  all.char.deployed

obs %>%
  mutate(char1 = substring(Flag.Code, 1, 1),
         char2 = substring(Flag.Code, 2, 2),
         char3 = substring(Flag.Code, 3, 3)) %>%
  dplyr::select(Flag.Code,char1, char2, char3) %>%
  gather(loc, char, 2:4) %>%
  count(char) %>%
  mutate(tot = sum(n),
         prop = n/tot,
         check = ifelse(char %in% c("B", "D", "F", "G", "O", "I",
                                    "Q", "R", "S", "W", "Z"), 
                        1, 0)) -> 
  all.char.resight

obs %>%
  filter(Flag.Code %in% first$Flag.Code.Out) %>%
  mutate(char1 = substring(Flag.Code, 1, 1),
         char2 = substring(Flag.Code, 2, 2),
         char3 = substring(Flag.Code, 3, 3)) %>%
  dplyr::select(Flag.Code,char1, char2, char3) %>%
  gather(loc, char, 2:4) %>%
  count(char) %>%
  mutate(tot = sum(n),
         prop = n/tot,
         check = ifelse(char %in% c("B", "D", "F", "G", "O", "I",
                                    "Q", "R", "S", "W", "Z"), 
                        1, 0)) %>%
  full_join(all.char.deployed, by = "char") %>%
  filter(check == 0) %>%
  mutate(n.rel = n/n.deployed)-> char.dep.res

char.dep.res %>%
  select(char, n, n.deployed) %>%
  rename(obs = n, exp = n.deployed) %>%
  gather(value, count, 2:3) -> n

char.dep.res %>%
  select(char, tot, tot.deployed) %>%
  rename(obs = tot, exp = tot.deployed) %>%
  gather(value, total, 2:3) -> tot

n %>%
  full_join(tot) %>%
  filter(value == "exp") %>%
  mutate(exp = count/total) %>%
  select(char, exp) -> expected

n %>%
  full_join(tot) %>%
  arrange(char) %>%
  mutate(prop = count/total) %>%
  full_join(expected, by = "char") -> dat

dat %>%
  mutate(low.ci = apply(dat[c("count", "total", "exp")], 
                        1, 
                        function(x) 
                          binom.test(x["count"], x["total"], x["exp"])$ conf.int[1]),
         upper.ci = apply(dat[c("count", "total", "exp")], 
                          1, 
                          function(x) 
                            binom.test(x["count"], x["total"], x["exp"])$ conf.int[2])) -> dat

dat$low.ci[dat$value == "exp"] = 0
dat$upper.ci[dat$value == "exp"] = 0

dat %>%
  rename(type = value) %>%
  mutate(type = case_when(type == "obs" ~ "resighted", type == "exp" ~"deployed")) %>%
  mutate(sig = ifelse(type == "resighted" && exp > low.ci & exp < upper.ci, 0, 1)) %>%
  select(char, type, low.ci, upper.ci, sig) -> obs.ci


# characters seen as often as expected
dat %>%
  filter(value == "obs") %>%
  mutate(sig = ifelse(exp > low.ci & exp < upper.ci, 0, 1)) %>%
  filter(sig == 0)

# characters seen less often than expected
dat %>%
  filter(value == "obs") %>%
  mutate(sig = ifelse(exp > low.ci & exp < upper.ci, 0, 1)) %>%
  filter(sig == 1 & prop < exp)

# characters seen more often than expected
dat %>%
  filter(value == "obs") %>%
  mutate(sig = ifelse(exp > low.ci & exp < upper.ci, 0, 1)) %>%
  filter(sig == 1 & prop > exp)

fig4A <- char.dep.res %>%
  select(char, prop.deployed, prop) %>%
  mutate(diff = prop-prop.deployed) %>%
  rename(deployed = prop.deployed, resighted = prop) %>%
  gather(type, prop, 2:3) %>%
  full_join(obs.ci, by = c("char", "type")) %>%
  arrange(diff) %>%
  mutate(char = factor(char, levels = unique(char))) %>%
  ggplot(aes(x = char, y = prop, fill = type)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.75) +
  annotate(geom = "rect", ymin = 0, ymax = 0.051, xmin = 0.5, xmax = 9.5,
           alpha = 0.3, fill = "dodgerblue2") +
  annotate(geom = "rect", ymin = 0, ymax = 0.051, xmin = 13.5, xmax = 25.5,
           alpha = 0.3, fill = "palegreen3") +
  geom_bar(position = "dodge", stat = "identity", width = 0.75) +
  geom_linerange(position = position_dodge(width = 0.75), 
                 aes(ymin = low.ci, ymax = upper.ci) ) +
  xlab("Flag character") +
  ylab("Frequency") +
  scale_fill_manual(values = c("gray20", "gray50")) +
  theme(legend.position = "none")
fig4A


# possible misreads ####
obs %>%
  filter(Flag.Code %in% first$Flag.Code.Out) %>%
  filter(SingleObs ==1) %>%
  mutate(char1 = substring(Flag.Code, 1, 1),
         char2 = substring(Flag.Code, 2, 2),
         char3 = substring(Flag.Code, 3, 3)) %>%
  dplyr::select(Flag.Code,char1, char2, char3) %>%
  gather(loc, char, 2:4) %>%
  count(char) %>%
  mutate(tot = sum(n),
         prop = n/tot,
         check = ifelse(char %in% c("B", "D", "F", "G", "O", "I",
                                    "Q", "R", "S", "W", "Z"), 
                        1, 0)) %>%
  full_join(all.char.deployed, by = "char") %>%
  filter(check == 0) %>%
  mutate(n.rel = n/n.deployed)-> char.dep.poss


char.dep.poss %>%
  select(char, n, n.deployed) %>%
  rename(obs = n, exp = n.deployed) %>%
  gather(value, count, 2:3) -> n

char.dep.poss %>%
  select(char, tot, tot.deployed) %>%
  rename(obs = tot, exp = tot.deployed) %>%
  gather(value, total, 2:3) -> tot

n %>%
  full_join(tot) %>%
  filter(value == "exp") %>%
  mutate(exp = count/total) %>%
  select(char, exp) -> expected

n %>%
  full_join(tot) %>%
  arrange(char) %>%
  mutate(prop = count/total) %>%
  full_join(expected, by = "char") -> dat

dat %>%
  mutate(low.ci = apply(dat[c("count", "total", "exp")], 
                        1, 
                        function(x) 
                          binom.test(x["count"], x["total"], x["exp"])$ conf.int[1]),
         upper.ci = apply(dat[c("count", "total", "exp")], 
                          1, 
                          function(x) 
                            binom.test(x["count"], x["total"], x["exp"])$ conf.int[2])) -> dat

dat$low.ci[dat$value == "exp"] = 0
dat$upper.ci[dat$value == "exp"] = 0

dat %>%
  rename(type = value) %>%
  mutate(type = case_when(type == "obs" ~ "resighted", type == "exp" ~"deployed")) %>%
  mutate(sig = ifelse(type == "resighted" && exp > low.ci & exp < upper.ci, 0, 1)) %>%
  select(char, type, low.ci, upper.ci, sig) -> obs.ci

dat %>%
  filter(value == "obs") %>%
  mutate(sig = ifelse(exp > low.ci & exp < upper.ci, 0, 1)) %>%
  filter(sig == 1) %>%
  mutate(diff = prop-exp) %>%
  arrange(diff)

fig4C <- char.dep.poss %>%
  select(char, prop.deployed, prop) %>%
  mutate(diff = prop-prop.deployed) %>%
  rename(deployed = prop.deployed, resighted = prop) %>%
  gather(type, prop, 2:3) %>%
  full_join(obs.ci, by = c("char", "type")) %>%
  arrange(diff) %>%
  mutate(char = factor(char, levels = unique(char))) %>%
  ggplot(aes(x = char, y = prop, fill = type)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.75) +
  annotate(geom = "rect", ymin = 0, ymax = 0.06, xmin = 0.5, xmax = 3.5,
           alpha = 0.3, fill = "dodgerblue2") +
  annotate(geom = "rect", ymin = 0, ymax = 0.06, xmin = 21.5, xmax = 25.5,
           alpha = 0.3, fill = "palegreen3") +
  geom_bar(position = "dodge", stat = "identity", width = 0.75) +
  geom_linerange(position = position_dodge(width = 0.75), 
                 aes(ymin = low.ci, ymax = upper.ci) ) +
  xlab("Flag character") +
  ylab("Frequency") +
  scale_fill_manual(values = c("gray20", "gray50")) +
  theme(legend.position = "none")
fig4C


# confirmed misreads ####

obs %>%
  filter(NotDeployed ==1 | Withheld == 1) %>%
  mutate(char1 = substring(Flag.Code, 1, 1),
         char2 = substring(Flag.Code, 2, 2),
         char3 = substring(Flag.Code, 3, 3)) %>%
  dplyr::select(Flag.Code,char1, char2, char3) %>%
  gather(loc, char, 2:4) %>%
  count(char) %>%
  mutate(tot = sum(n),
         prop = n/tot,
         check = ifelse(char %in% c("B", "D", "F", "G", "O", "I",
                                    "Q", "R", "S", "W", "Z"), 
                        1, 0)) %>%
  full_join(all.char.deployed, by = "char") %>%
  filter(check == 0) %>%
  mutate(n.rel = n/n.deployed)-> char.dep.nd


char.dep.nd %>%
  select(char, n, n.deployed) %>%
  rename(obs = n, exp = n.deployed) %>%
  gather(value, count, 2:3) -> n

char.dep.nd %>%
  select(char, tot, tot.deployed) %>%
  rename(obs = tot, exp = tot.deployed) %>%
  gather(value, total, 2:3) -> tot

n %>%
  full_join(tot) %>%
  filter(value == "exp") %>%
  mutate(exp = count/total) %>%
  select(char, exp) -> expected

n %>%
  full_join(tot) %>%
  arrange(char) %>%
  mutate(prop = count/total) %>%
  full_join(expected, by = "char") -> dat

dat %>%
  mutate(low.ci = apply(dat[c("count", "total", "exp")], 
                        1, 
                        function(x) 
                          binom.test(x["count"], x["total"], x["exp"])$ conf.int[1]),
         upper.ci = apply(dat[c("count", "total", "exp")], 
                          1, 
                          function(x) 
                            binom.test(x["count"], x["total"], x["exp"])$ conf.int[2])) -> dat

dat$low.ci[dat$value == "exp"] = 0
dat$upper.ci[dat$value == "exp"] = 0

dat %>%
  rename(type = value) %>%
  mutate(type = case_when(type == "obs" ~ "resighted", type == "exp" ~"deployed")) %>%
  mutate(sig = ifelse(type == "resighted" && exp > low.ci & exp < upper.ci, 0, 1)) %>%
  select(char, type, low.ci, upper.ci, sig) -> obs.ci

dat %>%
  filter(value == "obs") %>%
  mutate(sig = ifelse(exp > low.ci & exp < upper.ci, 0, 1)) %>%
  filter(sig == 1) %>%
  mutate(diff = prop-exp) %>%
  arrange(diff) %>%
  select(char, sig, diff)

fig4B <- char.dep.nd %>%
  select(char, prop.deployed, prop) %>%
  mutate(diff = prop-prop.deployed) %>%
  rename(deployed = prop.deployed, resighted = prop) %>%
  gather(type, prop, 2:3) %>%
  full_join(obs.ci, by = c("char", "type")) %>%
  arrange(diff) %>%
  mutate(char = factor(char, levels = unique(char))) %>%
  ggplot(aes(x = char, y = prop, fill = type)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.75) +
  annotate(geom = "rect", ymin = 0, ymax = 0.15, xmin = 0.5, xmax = 7.5,
           alpha = 0.3, fill = "dodgerblue2") +
  annotate(geom = "rect", ymin = 0, ymax = 0.15, xmin = 18.5, xmax = 25.5,
           alpha = 0.3, fill = "palegreen3") +
  geom_bar(position = "dodge", stat = "identity", width = 0.75) +
  geom_linerange(position = position_dodge(width = 0.75), 
                 aes(ymin = low.ci, ymax = upper.ci) ) +
  xlab("Flag character") +
  ylab("Frequency") +
  scale_fill_manual(values = c("gray20", "gray50")) +
  theme(legend.position = "none")
fig4B


# fig 4 ####

fig4 <- plot_grid(fig4A, fig4B, fig4C, NULL, ncol = 2, nrow = 2,align = "hv",
                 labels = c("A. All resightings", "B. Confirmed misreads",
                            "C. Possible misreads"), scale = 0.9)
save_plot(fig4, file = "figs/fig4_flag-characters.jpg",
          base_height = 7, base_aspect_ratio = 1.5)
