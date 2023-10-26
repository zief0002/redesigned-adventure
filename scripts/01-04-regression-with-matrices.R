library(tidyverse)
library(broom)


# Create data
d = data.frame(
  ID = 1:6,
  SAT = c(560, 780, 620, 600, 720, 380),
  GPA = c(3.0, 3.9, 2.9, 2.7, 3.7, 2.4),
  Self = c(11, 10, 19, 7, 18, 13),
  IQ = c(112, 143, 124, 129, 130, 82)
)

lm.1 = lm(GPA ~ 1 + SAT + IQ, data = d)
tidy(lm.1) #Coefficent-level output
glance(lm.1) |> print(width = Inf) #Model-level output
# summary(lm.1)
anova(lm.1) #Analysis of Variance


# Design matrix
# lm(GPA ~ 1 + SAT)

X = matrix(
  data = c(
    rep(1, 6), 
    560, 780, 620, 600, 720, 380
    ),
  ncol = 2
  )

X


# Design matrix
# lm(GPA ~ 1 + SAT + IQ)

X = matrix(
  data = c(
    rep(1, 6), 
    560, 780, 620, 600, 720, 380,
    112, 143, 124, 129, 130, 82
    ),
  ncol = 3
)

X


# Vector of outcomes
y = c(3.0, 3.9, 2.9, 2.7, 3.7, 2.4)


# Coefficients
b = solve(t(X) %*% X) %*% t(X) %*% y
b



# Get y-hat values
yhat = X %*% b
yhat


# Get Hat matrix (H-matrix)
H = X %*% solve(t(X) %*% X) %*% t(X)
H


# COmpute trace of H
k = sum(diag(H))
k


# Obtain residuals
e = y - yhat
e

# Or, (I - H)y
(diag(6) - H) %*% y


# SST
t(y - mean(y)) %*% (y - mean(y))

# SSM
t(yhat - mean(y)) %*% (yhat - mean(y))

# SSE
t(e) %*% e


# Variance of residuals
var_e = (t(e) %*% e) / (6 - k)
var_e
#glance(lm.1)


# Variance-covariance matrix of the coefficients
V_b = as.numeric(var_e) * solve(t(X) %*% X)
V_b
#vcov(lm.1)

se = sqrt(diag(V_b))
se
