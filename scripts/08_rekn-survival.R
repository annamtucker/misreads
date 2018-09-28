# estimate REKN survival with and without data cleaning/processing 
# only birds flagged by DE
# 28 aug 18

source("scripts/00_load-packages.R")

# data import and organization ----

dfr = read_csv("output/resights_clean.csv")
dfb = read_csv("output/first-flagged.csv")

dfb %>% 
  filter(Species == "REKN" & Flag.Color.Out == "FELG" &
           nchar(Flag.Code.Out) == 3) %>% 
  mutate(Year = year(firstcap)) %>% 
  filter(Year > 2007) -> dfb

to.check <- dfr %>% 
  filter(NotDeployed == 1)

# only keep resights of flags deployed by DE and filter out NotDeployed
dfr %>% 
  filter(Flag.Code %in% dfb$Flag.Code.Out & NotDeployed == 0) %>% 
  mutate(type = "resight") %>% 
  select(Flag.Code, date, Year, type, ObsCode, SingleObs) -> dfr

dfall <- dfb %>% 
  select(Flag.Code.Out, firstcap, Year) %>% 
  mutate(type = "capture",
         ObsCode = 999,
         SingleObs = 0) %>% 
  full_join(dfr, by = c("Flag.Code.Out" = "Flag.Code", "Year", "type",
                        "firstcap" = "date", "ObsCode", "SingleObs")) %>% 
  rename(flag = Flag.Code.Out, date = firstcap) %>% 
  arrange(flag, date)

# now the first year should be capture year and all the rest resights


# GoF test ----
ch = table(dfall$flag, dfall$Year)
ch[ch > 1] = 1

mhist = group_data(ch, rep(1, nrow(ch)))

caphx = mhist[,1:(ncol(mhist)-1)]
freq = mhist[,ncol(mhist)]

# overall test - significant
overall_CJS(caphx, freq)

# test 2CT - significant
test2ct(caphx, freq)

# test 3SR - significant
test3sr(caphx, freq)

# test 2CL - significant
# test 3SM - not significant
test2cl(caphx, freq)
test3sm(caphx, freq)

# model = trap-dependence, transience, overdispersion




# MARK setup ----
create.td=function(ch,varname="td",begin.time=1)
  #
  # Arguments:
  # ch - capture history vector (0/1 values only)
  # varname - prefix for variable name
  # begin.time - time for first occasion
  #
  # Value:
  # returns a datframe with trap-dependent variables
  # named varnamet+1,...,varnamet+nocc-1
  # where t is begin.time and nocc is the
  # number of occasions
#
{
  # turn vector of capture history strings into a vector of characters
  char.vec=unlist(strsplit(ch,""))
  # test to make sure they only contain 0 or 1
  if(!all(char.vec %in% c(0,1)))
    stop("Function only valid for CJS model without missing values")
  else
  {
    # get number of occasions (nocc) and change it into a matrix of numbers
    nocc=nchar(ch[1])
    tdmat=matrix(as.numeric(char.vec),ncol=nocc,byrow=TRUE)
    # remove the last column which is not used
    tdmat=tdmat[,1:(nocc-1)]
    # turn it into a dataframe and assign the field (column) names
    tdmat=as.data.frame(tdmat)
    names(tdmat)=paste(varname,(begin.time+1):(begin.time+nocc-1),sep="")
    return(tdmat)
  }
}

# gof testing and median c-hat for time varying model
df= dfall
ch = table(df$flag, df$Year)
ch[ch > 1] = 1

Phi.t = list(formula = ~time)
p.t = list(formula = ~time)
dat = data.frame(ch = apply(ch, 1, function(x) paste(x, collapse = "")))
dat$ch = as.character(dat$ch)

proct = process.data(dat, model = "CJS")
ddlt = make.design.data(proct)

glob.t = mark(proct, ddlt, 
              model.parameters = list(Phi = Phi.t, p = p.t))
export.MARK(proct, "time-vary", model = glob.t, replace = T)




process.and.run = function(df){
  ch = table(df$flag, df$Year)
  ch[ch > 1] = 1
  
  # use dummy age to estimate transience
  f = apply(ch, 1, function(x) min(which(x != 0)))
  
  age = matrix(0, nrow = nrow(ch), ncol = ncol(ch))
  for(i in 1:nrow(age)){
    age[i,f[i]] = 1
    if(f[i] < ncol(age)) age[i,(f[i]+1):ncol(age)] = 2
  }
  agedf = as.data.frame(age)
  names(agedf) = paste("res",1:ncol(ch),sep="")
  
  # process data and bind covars to capture histories
  dat = data.frame(ch = apply(ch, 1, function(x) paste(x, collapse = "")))
  dat$ch = as.character(dat$ch)
  
  # create td covariate that == 1 if seen last year and 0 if not
  m = create.td(dat$ch)
  dat = cbind(dat, m, agedf)
  
  cap.processed=process.data(dat, model="CJS")
  cap.ddl=make.design.data(cap.processed)
  
  # set up CJS models for marked
  Phi.t.trans = list(formula = ~time + res)
  Phi.trend.trans = list(formula = ~Time + res)
  Phi.trans = list(formula = ~res)
  p.t.td = list(formula = ~time + td)
  p.td = list(formula = ~td)

  cml = create.model.list("CJS")
  mark.wrapper(cml, data = cap.processed, ddl = cap.ddl)
  #export.MARK(cap.processed, "rekn_test_new", mlist, replace = T)
}


cjs1 = process.and.run(dfall)
cjs1_adj = adjust.chat(1.14, cjs1)

d0 = cjs1$Phi.trans.p.t.td$results$deviance
da = cjs1$Phi.t.trans.p.t.td$results$deviance
dt = cjs1$Phi.trend.trans.p.t.td$results$deviance

(dt-d0)/(da-d0)

# model average
cjs1_modavg_phi = model.average(cjs1_adj, "Phi", vcv = T)



# predicted values - survival
cov.df = data.frame(rbind(diag(rep(1, 10)),
                          diag(rep(2, 10))))
names(cov.df) = paste("res", 1:10, sep = "")
cov.df$index = rep(1:10, 2)

phi.est = covariate.predictions(cjs1$Phi.trend.trans.p.t.td, 
                                data = cov.df)

trend.ests = data.frame(year = c(1:10),
                        method = "remove impossible only",
                        phi = phi.est$estimates$estimate[11:20],
                        lcl = phi.est$estimates$lcl[11:20],
                        ucl = phi.est$estimates$ucl[11:20])




# remove observers with < 300 resightings ----
obs.errors = read_csv("output/observer_errors.csv")

to.keep <- obs.errors %>% 
  filter(Tot > 300) %>% 
  pull(ObsCode) 

to.keep = c(to.keep, 999)

df3 <- dfall %>% 
  filter(ObsCode %in% to.keep)

cjs3 = process.and.run(df3)
cjs3_adj = adjust.chat(1.13, cjs3)

# predicted values - survival
cov.df = data.frame(rbind(diag(rep(1, 10)),
                          diag(rep(2, 10))))
names(cov.df) = paste("res", 1:10, sep = "")
cov.df$index = rep(1:10, 2)

phi.est = covariate.predictions(cjs3$Phi.trend.trans.p.t.td, 
                                data = cov.df)

df = data.frame(year = c(1:10),
                        method = "remove inexperienced observers",
                        phi = phi.est$estimates$estimate[11:20],
                        lcl = phi.est$estimates$lcl[11:20],
                        ucl = phi.est$estimates$ucl[11:20])
trend.ests = rbind(trend.ests, df)


# remove all single obs ----
df4 <- dfall %>% 
  filter(SingleObs == 0)
  
cjs4 = process.and.run(df4)
cjs4_adj = adjust.chat(1.09, cjs4)


# predicted values - survival
cov.df = data.frame(rbind(diag(rep(1, 10)),
                          diag(rep(2, 10))))
names(cov.df) = paste("res", 1:10, sep = "")
cov.df$index = rep(1:10, 2)

phi.est = covariate.predictions(cjs4$Phi.trend.trans.p.t.td, 
                                data = cov.df)

df = data.frame(year = c(1:10),
                method = "remove single-observations",
                phi = phi.est$estimates$estimate[11:20],
                lcl = phi.est$estimates$lcl[11:20],
                ucl = phi.est$estimates$ucl[11:20])
trend.ests = rbind(trend.ests, df)



# remove inexperienced observers and single obs ----
df5 <- dfall %>% 
  filter(SingleObs == 0) %>% 
  filter(ObsCode %in% to.keep)

cjs5 = process.and.run(df5)
cjs5_adj = adjust.chat(1.09, cjs5)


# predicted values - survival
cov.df = data.frame(rbind(diag(rep(1, 10)),
                          diag(rep(2, 10))))
names(cov.df) = paste("res", 1:10, sep = "")
cov.df$index = rep(1:10, 2)

phi.est = covariate.predictions(cjs5$Phi.trend.trans.p.t.td, 
                                data = cov.df)

df = data.frame(year = c(1:10),
                method = "remove single-observations and\ninexperienced observers",
                phi = phi.est$estimates$estimate[11:20],
                lcl = phi.est$estimates$lcl[11:20],
                ucl = phi.est$estimates$ucl[11:20])
trend.ests = rbind(trend.ests, df)

saveRDS(trend.ests, "rekn-survival.rds")

# compare estimated phi among methods
fig5B <- trend.ests %>% 
  mutate(Year = c(2008:2017)[year]) %>% 
  ggplot(aes(x = Year, y = phi, fill = method)) +
  geom_line(lty = 2, alpha = 0.5) +
  geom_linerange(aes(ymin = lcl, ymax = ucl), lwd = 1,
                 position = position_dodge(width = 0.75)) +
  geom_point(aes(y = phi), position = position_dodge(width = 0.75),
             size = 3, pch = 21) +
  scale_x_continuous(breaks = seq(2008, 2017, 1)) +
  ylim(0.5, 1) +
  scale_fill_viridis_d(name = "Data filtering method") +
  ylab("Apparent annual survival probability") +
  theme(legend.position = c(0.5, 0.25),
        legend.background = element_rect(linetype = 1, colour = "black"))
fig5B

# extract beta estimates

betas = data.frame(method = c(1,3,4,5),
                   beta = c(cjs1$Phi.trend.trans.p.t.td$results$beta$estimate[2],
                            cjs3$Phi.trend.trans.p.t.td$results$beta$estimate[2],
                            cjs4$Phi.trend.trans.p.t.td$results$beta$estimate[2],
                            cjs5$Phi.trend.trans.p.t.td$results$beta$estimate[2]),
                   lcl = c(cjs1$Phi.trend.trans.p.t.td$results$beta$lcl[2],
                           cjs3$Phi.trend.trans.p.t.td$results$beta$lcl[2],
                           cjs4$Phi.trend.trans.p.t.td$results$beta$lcl[2],
                           cjs5$Phi.trend.trans.p.t.td$results$beta$lcl[2]),
                   ucl = c(cjs1$Phi.trend.trans.p.t.td$results$beta$ucl[2],
                           cjs3$Phi.trend.trans.p.t.td$results$beta$ucl[2],
                           cjs4$Phi.trend.trans.p.t.td$results$beta$ucl[2],
                           cjs5$Phi.trend.trans.p.t.td$results$beta$ucl[2]))



fig5A <- betas %>% 
  mutate(method = fct_relevel(c("Remove impossible only", NA, 
                    "Remove inexperienced observers",
                    "Remove single-observations",
                    "Remove inexperienced observers and \n single-observations")[method],
                    c("Remove inexperienced observers and \n single-observations",
                      "Remove single-observations",
                      "Remove inexperienced observers",
                      "Remove impossible only")))%>% 
  ggplot(aes(x = method, y = beta))+
  geom_linerange(aes(ymin = lcl, ymax = ucl), lwd = 1,
                 position = position_dodge(width = 0.5)) +
  geom_point(aes(y = beta, fill = method), size= 4, pch = 21,
             position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_fill_viridis_d(name = "", direction = -1) +
  coord_flip() +
  ylim(-0.15, 0.15) +
  xlab("") +
  ylab("Estimated annual trend") +
  theme(legend.position = "none")
fig5A

fig5 <- plot_grid(fig5A, fig5B, rel_widths = c(2.5,4), ncol = 2,
                  labels = c("A", "B"), align ="h", scale = 0.9)
fig5

save_plot(fig5, filename = "figs/fig5_rekn-survival.jpg",
          base_width = 14, base_height = 6)
