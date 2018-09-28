# simulate misreads in mark-resight data

# simulation function that permits multiple encounters of an 
# individual during each occasion. At each occasion, the number of times an individual 
# is encountered is a Poisson random variable with lambda based on input detection probability (p)

# clean data by removing single-observations and refit CJS model

sim.and.model.trend.clean = function(years, error, phi, p){
  
  phi = rep(phi, years)ars))
  
  lambda = -log(1-p)
  
  marked = rep(n.ind, years-1)
  n.occasions = years
  
  CH <- matrix(0, ncol = n.occasions, nrow = sum(marked))
  CH.n = CH
  
  mark.occ <- rep(1:length(marked), marked[1:length(marked)])
  
  for (i in 1:sum(marked)){
    CH[i, mark.occ[i]] <- 1
    CH.n[i, mark.occ[i]] <- 1
    if (mark.occ[i]==n.occasions) next
    for (t in (mark.occ[i]+1):n.occasions){
      sur <- rbinom(1, 1, phi[t])
      if (sur==0) break		
      rp <- rbinom(1, 1, p)
      if (rp==1){
        CH[i,t] <- 1
        
        # if seen, how many times?
        nobs = max(rpois(1, lambda), 1)
        CH.n[i,t] <- nobs
      }
    } 
  } 
  
  # loop through occasions, introduce misreads by re-assigning detections
  # assume correct ID on first capture, potential misread only on subsequent detections
  # only allow false positives on individuals already captured
  # for each individual that was detected, potential for multiple misreads
  for(i in 2:ncol(CH)){
    if(i <= ncol(CH)-2) ch = CH.n[c(1:(n.ind*i-n.ind), (n.ind*i+1):nrow(CH)),i]
    if(i > ncol(CH)-2) ch = CH.n[1:(n.ind*i-n.ind),i]
    
    # detected individuals - each detection gives potential misread
    x = which(ch > 0)
    misreads = rbinom(rep(1, length(x)), ch[x], error)
    
    # if the number of misreads is >= number of obs, switch that ind to not detected
    nd = which(misreads >= ch[x])
    CH[nd,i] <- 0
    
    # not detected individuals - only those previously marked
    # randomly choose based on number of misreads and switch to 1
    ch2 = CH[1:(n.ind*i-n.ind), i]
    y = which(ch2 == 0)
    CH[sample(y, sum(misreads), replace = F),i] = 1
  }
  
  # remove rows with no detections
  if(length(which(rowSums(CH) == 0))){
    CH = CH[-which(rowSums(CH) == 0),]
  }
  
  # remove single-observations
  CH.clean = CH.n
  CH.clean[CH == 1 & CH.n == 0] = 1
  first = apply(CH.clean, 1, function(x) min(which(x == 1)))
  for(i in 1:length(first)){
    CH.clean[i,first[i]] = 2
  }
  CH.clean[which(CH.clean == 1)] = 0
  CH.clean[which(CH.clean > 0)] = 1
  
  # format for marked - no data cleaning
  dat = data.frame(ch = apply(CH, 1, function(x) paste(x, collapse = "")))
  dat$ch = as.character(dat$ch)
  
  proc.dat = process.data(dat)
  ddl.dat = make.design.data(proc.dat)
  
  # with data cleaning
  dat_clean = data.frame(ch = apply(CH.clean, 1, function(x) paste(x, collapse = "")))
  dat_clean$ch = as.character(dat_clean$ch)
  
  proc.dat_clean = process.data(dat_clean)
  ddl.dat_clean = make.design.data(proc.dat_clean)

  
  # fit both data sets
  Phi.trend = list(formula = ~Time)
  p.dot = list(formula = ~1)
  
  results1 = mark(proc.dat, ddl.dat, 
                 model.parameters = list(Phi = Phi.trend, p = p.dot))
  results2 = mark(proc.dat_clean, ddl.dat_clean, 
                  model.parameters = list(Phi = Phi.trend, p = p.dot))
  return(list(results1, results2))
}
