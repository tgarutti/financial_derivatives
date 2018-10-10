##-------------------------------------------------------------------------------##
## Simulation study.
##-------------------------------------------------------------------------------##
path_sim <- function(length, asset1, asset2, corr, t)
{
  paths <- matrix(ncol = 2, nrow = length)
  S1 <- asset1$s0
  S2 <- asset2$s0
  for (i in 1:length) 
  {
    W <- rnorm(2, 0, 1)
    e1 <- W[1]
    e2 <- corr*W[1] + W[2]*sqrt(1-corr^2)
    paths[i,1] <- S1 * exp(asset1$mu*t + asset1$sigma*sqrt(t)*e1)
    paths[i,2] <- S2 * exp(asset2$mu*t + asset2$sigma*sqrt(t)*e2)
    S1 <- paths[i,1]
    S2 <- paths[i,2]
  }
  
  return(paths)
}

n_paths <- function(N, length, asset1, asset2, corr, t)
{
  Npaths <- matrix(ncol = 2, nrow = N)
  for (i in 1:N) 
  {
    Npaths[i,] <- path_sim(length, asset1, asset2, corr, t)[length,]
  }
  
  return(Npaths)
}

plot_Npaths <- function(N, length, asset1, asset2, corr, t)
{
  par(mfrow=c(2,1)) 
  plot(NULL, type="n", xlab="Days (t)", ylab="Price of asset 1 at time t", xlim=c(0, 21), ylim=c(asset1$s0-20, asset1$s0+20))
  grid (NULL,NULL, lty = 6, col = "gray") 
  for (i in 1:N) 
  {
    v <- cbind(0:(length),c(asset1$s0, path_sim(length, asset1, asset2, corr, t)[,1]))
    lines(v, col = rainbow(N*4)[3*i])
  }
  
  plot(NULL, type="n", xlab="Days (t)", ylab="Price of asset 2 at time t", xlim=c(0, 21), ylim=c(asset2$s0-20, asset2$s0+20))
  grid (NULL,NULL, lty = 6, col = "gray") 
  for (i in 1:N) 
  {
    v <- cbind(0:(length),c(asset2$s0, path_sim(length, asset1, asset2, corr, t)[,2]))
    lines(v, col = rainbow(N*4)[3*i])
  }
}

calculate_payoffs <- function(ST, K)
{
  # Call on max payoff
  callMax <- rowMax(ST)-K[1]
  callMax[callMax<0] <- 0
  
  # Call on min payoff
  callMin <- rowMin(ST)-K[1]
  callMin[callMin<0] <- 0
  
  # Put on max payoff
  putMax <- K[2] - rowMax(ST)
  putMax[putMax<0] <- 0
  
  # Put on min payoff
  putMin <- K[2] - rowMin(ST)
  putMin[putMin<0] <- 0
  
  # Call on asset1 payoff
  call1 <- ST[,1] - K[1]
  call1[call1<0] <- 0
  
  # Put on asset1 payoff
  put1 <- K[2] - ST[,1]
  put1[put1<0] <- 0
  
  #Call on asset2 payoff
  call2 <- ST[,2] - K[1]
  call2[call2<0] <- 0
  
  #Put on asset2 payoff
  put2 <- K[2] - ST[,2]
  put2[put2<0] <- 0
  
  return(cbind(callMax, callMin, putMax, putMin, call1, put1, call2, put2))
}

rowMax <- function(m)
{
  rMax <- apply(m, 1, function(i){max(i)})
  return(rMax)
}

rowMin <- function(m)
{
  rMin <- apply(m, 1, function(i){min(i)})
  return(rMin)
}

simulation_study <- function(N, length, asset1, asset2, corr, t, K)
{
  sim_payoffs <- matrix(nrow = 8, ncol = N)
  for (i in 1:N) 
  {
    Npaths <- n_paths(10*i, 20, asset1, asset2, 0.5, 1/20)
    payoffs <- calculate_payoffs(Npaths, K)
    avg_payoffs <- colMeans(payoffs)
    sim_payoffs[,i] <- avg_payoffs
  }
  
  return(sim_payoffs)
}

plot_simulation <- function(sim_payoffs, N)
{
  par(mfrow=c(4,1)) 
  plot(NULL, type="n", xlab="N, number of paths simulated", ylab="Price of Call on Max", xlim=c(10, 10*N), ylim=c(6, 14))
  grid (NULL,NULL, lty = 6, col = "gray")
  lines(x = 10*(1:N), y = sim_payoffs[1,])
  
  plot(NULL, type="n", xlab="N, number of paths simulated", ylab="Price of Call on Min", xlim=c(10, 10*N), ylim=c(0, 5))
  grid (NULL,NULL, lty = 6, col = "gray")
  lines(x = 10*(1:N), y = sim_payoffs[2,])
  
  plot(NULL, type="n", xlab="N, number of paths simulated", ylab="Price of Put on Max", xlim=c(10, 10*N), ylim=c(0, 2))
  grid (NULL,NULL, lty = 6, col = "gray")
  lines(x = 10*(1:N), y = sim_payoffs[3,])
  
  plot(NULL, type="n", xlab="N, number of paths simulated", ylab="Price of Put on Min", xlim=c(10, 10*N), ylim=c(2, 8))
  grid (NULL,NULL, lty = 6, col = "gray")
  lines(x = 10*(1:N), y = sim_payoffs[4,])
}
##-------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------##