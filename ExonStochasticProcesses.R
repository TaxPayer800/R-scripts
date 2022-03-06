# Set random seed for reproducibility
set.seed(121)

# Daily random shocks for a 5-year period, with mean 0 and unit st. deviation
# Simulated 1,000 times
# So we arrange the data in a matrix (rows = days; columns = simulation runs)
# For brevity, we use the same set of random shocks to simulate a generalized Wiener
# and a geometric Brownian motion. This is not strictly necessary!
eps <- matrix( rnorm(1250*1000,mean = 0, sd = 1) , nrow = 1250, ncol = 1000)

#   GENERALIZED WIENER PROCESS
# Simulating 1,000 times dx = 1dt + 6dz, and `stock price' X, starting from X0 = 20 
# (assume a daily time step of 1/250 of one year - i.e., there are 250 trading days per year)
dX = (1/250) + 6*eps*sqrt(1/250)
X = as.data.frame( 20 + apply(dX,2,cumsum) )


# Computing percentiles across all simulations and storing it in a data frame. 
minSim <- apply(X,1,min)
prob05Sim <- apply(X,1,quantile,probs=c(.05))
prob25Sim <- apply(X,1,quantile,probs=c(.25))
prob50Sim <- apply(X,1,quantile,probs=c(.50))
prob75Sim <- apply(X,1,quantile,probs=c(.75))
prob95Sim <- apply(X,1,quantile,probs=c(.95))
maxSim <- apply(X,1,max)

statsGenWiener <- as.data.frame(cbind(minSim,p05Sim,p25Sim,p50Sim,p75Sim,p95Sim,maxSim))

# Histogram plot of the terminal values
gwTV = as.numeric(tail(X,n=1))
hist(gwTV)

# Cleanup
rm(dX,X,minSim,p05Sim,p25Sim,p50Sim,p75Sim,p95Sim,maxSim)



#   GEOMETRIC BROWNIAN MOTION

logRet = 0.05/250 + 0.30*epsilon*sqrt(1/250)
S = exp( as.data.frame( log(20) + apply(logRet,2,cumsum) ) )

# Computing percentiles across all simulations and storing it in a data frame. 
minSim <- apply(S,1,min)
prob05Sim <- apply(S,1,quantile,probs=c(.05))
prob25Sim <- apply(S,1,quantile,probs=c(.25))
prob50Sim <- apply(S,1,quantile,probs=c(.50))
prob75Sim <- apply(S,1,quantile,probs=c(.75))
prob95Sim <- apply(S,1,quantile,probs=c(.95))
maxSim <- apply(S,1,max)

statsGBM <- as.data.frame(cbind(minSim,p05Sim,p25Sim,p50Sim,p75Sim,p95Sim,maxSim))

# Histogram plot of the terminal values
gbmTV = as.numeric(tail(S,n=1))
hist(gbmTV)

# Cleanup
rm(logRet,minSim,p05Sim,p25Sim,p50Sim,p75Sim,p95Sim,maxSim)
