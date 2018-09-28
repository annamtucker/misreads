# simulation helper functions

# extract information from RMark model objects

find.beta = function(x){
  x$results$beta$estimate[2]
}

find.beta.lcl = function(x){
  x$results$beta$lcl[2]
}

find.beta.ucl = function(x){
  x$results$beta$ucl[2]
}

extract.reals.trend = function(x){
  n = nrow(x$results$real)
  
  df = x$results$real[,c(1,3,4)]
  df$parm = c(rep("phi", n-1), "p")
  df$year = c(1:(n-1), NA)
  rownames(df) = NULL
  
  return(df)
}
