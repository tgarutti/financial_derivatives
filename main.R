##-------------------------------------------------------------------------------##
## Main.
##-------------------------------------------------------------------------------##
setwd("~/Documents/Repositories/financial_derivatives/")
source("source_file.R")

asset1 <- list(s0 = 220, mu = 0.015, sigma = 0.08)
asset2 <- list(s0 = 220, mu = 0.02, sigma = 0.11)

Npaths <- n_paths(10000, 20, asset1, asset2, 0.5, 1/20)
# plot_Npaths(20, 20, asset1, asset2, 0.5, 1/20)
K <- c(230, 210)
payoffs <- calculate_payoffs(Npaths, K)
avg_payoffs <- colMeans(payoffs)
sim_payoffs <- simulation_study(100, 20, asset1, asset2, 0.5, 1/20, K)
discounted_payoffs <- sim_payoffs*exp(-0.1/12)
# plot_simulation(discounted_payoffs[1:4,], 1000)
##-------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------##