##----------------------------------------------------------------------------------------------------------##
## Main.
##----------------------------------------------------------------------------------------------------------##
setwd("~/Documents/Repositories/financial_derivatives/")
source("source_file.R")

asset1 <- list(s0 = 220, mu = 0.015, sigma = 0.08)
asset2 <- list(s0 = 220, mu = 0.02, sigma = 0.11)

Npaths <- n_paths(1000, 20, asset1, asset2, 0.5, 1/20)
plot_Npaths(20, 20, asset1, asset2, 0.5, 1/20)
payoffs <- calculate_payoffs(Npaths, c(240, 230, 210, 200))
avg_payoffs <- colMeans(payoffs)