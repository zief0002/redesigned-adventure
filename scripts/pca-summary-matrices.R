##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(tidyverse)
library(tidymodels)
library(patchwork)



##################################################
### Import and prepare data
##################################################

eeo = read_csv("https://github.com/zief0002/redesigned-adventure/raw/main/data/equal-education-opportunity.csv")
head(eeo)


##################################################
### Create matrices and data sets of predictors
##################################################

# Create matrix of predictors
X_p = as.matrix(eeo[ , c("faculty", "peer", "school")])


# Create correlation matrix
r_xx = cor(X_p) 


#Cretae covariation matrix
s_xx = cov(X_p) 



##################################################
### PCA on centered predictors
##################################################

# Fit the PCA using SVD decomposition
svd_center = eeo |>
  select(faculty, peer, school) |>
  #scale(center = TRUE, scale = FALSE) |> # center data
  prcomp(center = TRUE, scale = FALSE)


# View standard deviations and rotation matrix (eigenvector matrix)
svd_center




##################################################
### PCA on covariation matrix
##################################################

svd_cov = svd(s_xx)

svd_cov


##################################################
### PCA on Centered and Scaled Data
##################################################

svd_std = eeo |>
  select(faculty, peer, school) |>
  scale(center = TRUE, scale = TRUE) |> # center data
  prcomp()


svd_std


##################################################
### PCA on correlation matrix
##################################################

svd_cor = svd(r_xx)

svd_cor
