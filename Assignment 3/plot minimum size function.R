get_n_eq <- function(dfData, index, sTreatment, dAlpha, dPower, MinDE=0.1, dSigma_sq){
  # T_statistics of alpha and power levels
  t_stats_alpha <- qnorm(1-dAlpha/2, 0,dSigma_sq)
  t_power <- qnorm(1-dPower, 0, dSigma_sq)
  
  # get the number of observations
  n_obs <- 156
  # Observations in treated group
  n_treatment <- index
  # Proportion of treatment observations
  proportion <- n_treatment/n_obs
  print(proportion)
  
  # get the desired sample size
  sample_size = (dSigma_sq/(proportion*(1-proportion)))*((t_stats_alpha-t_power)/MinDE)^2
  return(round(sample_size))
}

# Initialize, set sigma and alpha fixed
dSigma_sq = 1
dAlpha= 0.05
dPower = 0.8

dfFunc <- data.frame(seq(1,155, by=1), rep(1, 155))

for(row in 1:nrow(dfFunc)){
  dfFunc[row, 2] <- get_n_eq(dfData=dfFunc, row, "bonus500", dAlpha = dAlpha, dPower = dPower, dSigma_sq = dSigma_sq)
}
