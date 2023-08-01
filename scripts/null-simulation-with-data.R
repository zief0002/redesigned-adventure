### Load libraries

library(broom)
library(tidyverse)


### Import Data
keith2 = read_csv(file = "~/Desktop/keith2.csv")
keith2


# Fit Model
lm.1 = lm(gpa ~ 1 + homework, data = keith2)
tidy(lm.1)


#############
# Our simulation in going to test whether beta_1=0
# TO do this we need to simulate from the intercept-only model
# To simulate from this model we need estimates of beta_0 and sigma
###################################

# Fit intercept-only mode
lm.0 = lm(gpa ~ 1, data = keith2)

# Obtain beta_0 esrtimate
coef(lm.0)

# Obtain sigma estimate
summary(lm.0)$sigma

#######################
# Simulate from the intercept-only model
#######################

# Set up parameters in simulation 
beta = c(81.16667, 0)
sigma = 7.856837
n = 30
trials = 1500 #Number of trials


# Use x-values from data to use in each trial of the simulation
x = keith2$homework


# Set up empty list to store simulation results
my_estimates = vector(mode = "list", length = trials)


# Repeat the following steps in the simulation
for(i in 1:trials){
  # 1. Simulate the y values
  y = beta[1] + beta[2] * x + rnorm(n, mean = 0, sd = sigma)
  
  # 2. Fit regression model to simulated y values and X
  fitted_model = lm(y ~ 1 + x)
  
  # 3. Store estimated values in my_estimates matrix (in the first and second element of the ith list slot)
  my_estimates[[i]][1] = coef(fitted_model)[[1]] #Extract intercept estimate
  my_estimates[[i]][2] = coef(fitted_model)[[2]] #Extract slope estimate
  my_estimates[[i]][3] = summary(fitted_model)$r.squared #Extract R2 estimate
}

# Convert the list to a data frame for easier computing
results = data.frame(do.call(rbind, my_estimates))
names(results) = c("b_0", "b_1", "R2") # name columns

ggplot(data = results, aes(x = b_1)) +
  geom_density(color = "blue")



#################################
# Does this follow a theoretical t(28) distribution?
#################################

# Set up data frame of (x,y) values to draw the theoretical t(28) distribution
t_data = data.frame(
  x = seq(from = -3, to = 3, by = 0.0001)
) |>
  mutate(
    y = dt(x, df = 28)
  )

# COmpute the SE(b_1)
se_b1 = sd(results$b_1) * 29 / 28



ggplot(data = results, aes(x = (b_1/se_b1))) + #Convert b_1 to t-values
  geom_density(color = "blue") +
  theme_bw() +
  geom_line(data = t_data, aes(x = x, y = y), color = "red") #Draw line for theoretical t(28) distribution


#################################
# Compute p-value using simulated data
#################################

# Which results are more extreme than 0.978 (the observed t-value)
abs(results$b_1) >= 0.978

# Count results more extreme than 0.978 (the observed t-value)
sum(abs(results$b_1) >= 0.978)

# Compute the proportion of results more extreme than 0.978 (the observed t-value)...which is the p-value
# Adjust this so we don't get 0 by adding 1 to both numerator and denominator
(sum(abs(results$b_1) >= 0.978) + 1) / (1500 +1)




#################################
# Compute 95% CI using theory
#################################

confint(lm.1)



#################################
# Compute 95% CI using simulation
# Simulate from the model that includes parameters based on including b_1 (e.g., lm.1)
#################################

# coef(lm.1)
# summary(lm.1)$sigma
beta = c(76.4053951, 0.9783435)
sigma = 7.611593
n = 30
trials = 1500 #Number of trials


# Set up empty list to store simulation results
my_estimates = vector(mode = "list", length = trials)


# Repeat the following steps in the simulation
for(i in 1:trials){
  # 1. Simulate the y values
  y = beta[1] + beta[2] * x + rnorm(n, mean = 0, sd = sigma)
  
  # 2. Fit regression model to simulated y values and X
  fitted_model = lm(y ~ 1 + x)
  
  # 3. Store estimated values in my_estimates matrix (in the first and second element of the ith list slot)
  my_estimates[[i]][1] = coef(fitted_model)[[1]] #Extract intercept estimate
  my_estimates[[i]][2] = coef(fitted_model)[[2]] #Extract slope estimate
  my_estimates[[i]][3] = summary(fitted_model)$r.squared #Extract R2 estimate
}

# Convert the list to a data frame for easier computing
results = data.frame(do.call(rbind, my_estimates))
names(results) = c("b_0", "b_1", "R2") # name columns



# Find the 2.5th and 97.5th percentiles of the simulated b_1 values
quantile(results$b_1, probs = c(.025, .975))




