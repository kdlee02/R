# load data
options(scipen = 999)
data <- read.table("longley.txt", header = TRUE)

# standardize the data
data_standardized <- as.data.frame(scale(data))

# fit a regression model to the standardized data
model <- lm(Y ~ ., data = data_standardized)
model2 <- lm(Y ~ ., data = data)

if(!require("olsrr")) install.packages("olsrr")
library(olsrr)
ols_regress(Y ~ ., data = data_standardized) 

# coefficients from model (standardized data)
coef_standardized <- coef(model)

# calculate mean and sd
means <- colMeans(data)
sds <- apply(data, 2, sd)

# unscale
real_coef <- coef_standardized[-1] * (sds["Y"] / sds[-1]) 
intercept <- means["Y"] - sum(real_coef * means[-1])

# combine
real_coef <- c(intercept, real_coef)
names(real_coef) <- names(coef(model2))

# result
real_coef

# compare with model2 cofficients (They should match)
coef(model2)

# check for multicollinearity
cor(data_standardized)
pairs(data_standardized)

# PCA
pc <- prcomp((data_standardized[,2:7]))   
# eigenvalue
round(pc$sdev**2, digits = 3)
# eigenvector
round(pc$rotation, digits = 3)  
# principal component
round(pc$x, digits = 3) 

# condition number
singular_values = pc$sdev
condition_number <- max(singular_values)/min(singular_values)
condition_number


if(!require("MASS")) install.packages("MASS")
library(MASS)

# ridge regression
ridge <- lm.ridge(Y~., data = data,
                  lambda = seq(0, 0.05, 0.01)) 

# Ridge Trace Plot
plot(ridge)
select(ridge)

# select 0.04 as regularization parameter
round(coef(ridge)[ridge$lambda == 0.04,], 3)
round
coefficientss = round(coef(ridge)[ridge$lambda == 0.04, c("X1", "X2", "X3", "X4", "X5", "X6")], 3)
intercept = round(coef(ridge)[ridge$lambda == 0.04, c(1)], 3)

# calculate yhat
result <- sweep(data[, 2:7], 2, coefficientss, `*`)
yhat = rowSums(result) + intercept

# correlation
correlation <- cor(yhat, data['Y'])
correlation

# mse
mse <- mean((yhat - as.numeric(data$Y))^2)
mse

# excluding y value, scale it and perform prcomp
pc <- prcomp(scale(data[,2:7]))
# PCA assumes that the data is centered around the mean, meaning the mean of each variable is zero
data_centered <- scale(data[, 2:7], center = TRUE, scale = FALSE)
# compute the principal component
original_pc_scores <- as.data.frame(as.matrix(data_centered) %*% pc$rotation)

# combine with y values and run the regression model
combined_data <- data.frame(Y = data$Y, original_pc_scores)

# evalulate and compare ridge regression with pca 
ols_regress(Y ~ PC1+PC2, data = combined_data)  



