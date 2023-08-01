library(tidyverse)


##################################################
### Simulating data from a KNOWN simple regression model
### to explore distributions of coefficient estimates
##################################################

# Set up KNOWN parameters in simulation 
beta = c(0.2, 0.5)
n = 200
trials = 1000 # Number of trials

# Set up empty list to store estimates
my_estimates = vector(mode = "list", length = trials)

set.seed(100484) # Make simulation reproducible

x = runif(n, min = -1, max = 1)

# Repeat the following steps in the simulation
for(i in 1:trials){
  # 1. Simulate the y values
  y = beta[1] + beta[2] * x + rnorm(n, mean = 0, sd = (exp(1.5 * x)))
  y2 = beta[1] + beta[2] * x + rnorm(n, mean = 0, sd = 1.885259)
  
  # 2. Fit regression model to simulated y values and X
  fitted_model = lm(y ~ 1 + x)
  fitted_model2 = lm(y2 ~ 1 + x)
  
  # 3. Store estimated values in b matrix (row i, columns 1 and 2)
  my_estimates[[i]][1] = coef(fitted_model)[[1]] #intercept estimate
  my_estimates[[i]][2] = coef(fitted_model)[[2]] # slope estimate
  my_estimates[[i]][3] = summary(fitted_model)$sigma #residual standard error
  
  my_estimates[[i]][4] = coef(fitted_model2)[[1]] #intercept estimate
  my_estimates[[i]][5] = coef(fitted_model2)[[2]] # slope estimate
  my_estimates[[i]][6] = summary(fitted_model2)$sigma #residual standard error
}

# Convert the list to a data frame for easier computing
b = data.frame(do.call(rbind, my_estimates))
names(b) = c("b_0", "b_1", "rse", "b_02", "b_12", "rse2")

mean(b$b_0)
mean(b$b_1)
mean(b$rse)
mean(b$rse2)


