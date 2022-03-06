# Parameters
S0 <- 100
mu <- .10
sigma <- .25
r <- .05
T <- .67
K <- 80

# Set time interval (as a fraction of time to maturity; the finer, the more precise the price output)
nsteps <- 25
dt <- T/nsteps

# Up and down factors
u <- exp(sigma*dt)
d <- exp(-sigma*dt)

# Risk-neutral probability of an "up" move
q <- (exp(r*dt) - d)/(u - d)

# Matrices that contain the trees
stock_tree <- matrix(0, nsteps, nsteps)
call_tree <- matrix(0, nsteps, nsteps)

# Stock tree
for ( i in 1:nsteps ) {
  for ( j in i:nsteps ) {
    stock_tree[i,j] <- S0 * u^((j-1)-(i-1)) * d^(i-1)
  }
}

# Call option tree, by backwards induction

# Start from the last period...
call_tree[,nsteps] = pmax(stock_tree[,nsteps] - K, 0)
 
# ...Define the values of the option at every step...
for ( j in seq(from = nsteps-1, to = 1, by = -1) ) {
  for ( i in 1:j ) {
    call_tree[i,j] <- exp(-r*dt) * ( q*call_tree[i,j+1] + (1-q)*call_tree[i+1,j+1] )
  }
}

# ...The price of the option at time 0 is element [1,1]
bin_price <- call_tree[1,1]