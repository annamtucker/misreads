# simulation function

# this function simulates a capture history (adapted from Kery and Schaub 2012), 
# and then fits four models using marked and saves the outputs in a tibble  


sim.and.model = function(years, error){
  
  marked = rep(n.ind, years-1)
  n.occasions = years
  
  CH <- matrix(0, ncol = n.occasions, nrow = sum(marked))
  
  mark.occ <- rep(1:length(marked), marked[1:length(marked)])
  
  # simulate capture history as normal
  for (i in 1:sum(marked)){
    CH[i, mark.occ[i]] <- 1      
    if (mark.occ[i]==n.occasions) next
    for (t in (mark.occ[i]+1):n.occasions){
      sur <- rbinom(1, 1, phi)
      if (sur==0) break		
      rp <- rbinom(1, 1, p)
      if (rp==1) CH[i,t] <- 1
    } 
  } 
  
  # loop through occasions, introduce misreads by re-assigning detections
  # assume correct ID on first capture, potential misread only on subsequent detections
  # only allow false positives on individuals already captured
  for(i in 2:ncol(CH)){
    if(i <= ncol(CH)-2) ch = CH[c(1:(n.ind*i-n.ind), (n.ind*i+1):nrow(CH)),i]
    if(i > ncol(CH)-2) ch = CH[1:(n.ind*i-n.ind),i]
    
    # detected individuals - switch subset to 0
    x = which(ch == 1)
    switch = rbinom(length(x), 1, error)
    CH[x[which(switch == 1)],i] = 0
    
    # not detected individuals - switch same number of misreads to 1
    ch2 = CH[1:(n.ind*i-n.ind), i]
    y = which(ch2 == 0)
    CH[sample(y, sum(switch), replace = F),i] = 1
  }
  
  # remove rows with no detections
  if(length(which(rowSums(CH) == 0))){
    CH = CH[-which(rowSums(CH) == 0),]
  }
  
  # format for marked
  CH %>%
    as.data.frame() %>%
    unite(ch, 1:years, sep = "") -> dat
  
  proc.dat = process.data(dat)
  ddl.dat = make.design.data(proc.dat)
  
  results = crm.wrapper(model.list = cml, data = proc.dat, ddl = ddl.dat,
                        external = F, accumulate = F)
  return(results)
}