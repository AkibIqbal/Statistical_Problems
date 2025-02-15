set.seed(123)
n <- 100  
beta_0 <- 3  
beta_1 <- 2 
x <- rnorm(n, mean = 0, sd = 1)
error <- rnorm(n, mean = 0, sd = 0.5)
y <- beta_0 + beta_1 * x + error
model <- lm(y ~ x)
summary(model)

Beta_hat_0 = coef(model)[1]
Beta_hat_1 = coef(model)[2]

norm = sqrt(sum((beta_1 - Beta_hat_1)^2+(beta_0 - Beta_hat_0)^2)/2)

cat("\n\nEstimated β₀ (Intercept) =", Beta_hat_0)
cat("\n\nEstimated β₁ (Slope) =", Beta_hat_1)
cat("\n\nNorm =", norm)

#outliers
x_with_outliers <- x
outlier_positions <- c(25)
x_with_outliers[outlier_positions] <- c(5000)

y_with_outliers <- beta_0 + beta_1 * x_with_outliers + error
model_with_outliers <- lm(y_with_outliers ~ x_with_outliers)

Beta_hat_0_outliers = coef(model_with_outliers)[1]
Beta_hat_1_outliers = coef(model_with_outliers)[2]

norm_outliers = sqrt(sum((beta_1 - Beta_hat_1_outliers)^2+(beta_0 - Beta_hat_0_outliers)^2)/2)

cat("\n\nEstimated β₀ with outliers (Intercept) =", Beta_hat_0_outliers)
cat("\n\nEstimated β₁ with outliers (Slope) =", Beta_hat_1_outliers)
cat("\n\nNorm with outliers =", norm_outliers)
