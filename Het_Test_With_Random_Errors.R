library(nlme)
library(lmtest)

set.seed(153)
n <-1000
beta_0 <- 3  
beta_1 <- 2 

x <- rnorm(n, mean = 0, sd = 1)

random_sds <- runif(n, min = 0.1, max =10)  

error <- rnorm(n, mean = 0, sd = random_sds)

y <- beta_0 + beta_1 * x + error

model <- lm(y ~ x)
summary(model)

# Perform Breusch-Pagan test
bp_test <- bptest(model)

# Print BP test results
cat("\n\nBreusch-Pagan Test Results:")
cat("\n----------------------------")
cat("\nTest statistic:", bp_test$statistic)
cat("\np-value:", bp_test$p.value)
cat("\n\nInterpretation:")
cat("\nH₀: Homoscedasticity (constant variance)")
cat("\nH₁: Heteroscedasticity (non-constant variance)")
if(bp_test$p.value < 0.05) {
    cat("\nResult: Reject H₀ - Evidence of heteroscedasticity (p < 0.05)")
} else {
    cat("\nResult: Fail to reject H₀ - No strong evidence of heteroscedasticity (p >= 0.05)")
}




Beta_hat_0 = coef(model)[1]
Beta_hat_1 = coef(model)[2]

norm = sqrt(sum((beta_1 - Beta_hat_1)^2+(beta_0 - Beta_hat_0)^2)/2)

cat("\n\nEstimated β₀ (Intercept) =", Beta_hat_0)
cat("\n\nEstimated β₁ (Slope) =", Beta_hat_1)


weights <- 1/random_sds^2

gls_model <- gls(y ~ x, weights = varFixed(~random_sds))
summary(gls_model)


Beta_hat_0_gls = coef(gls_model)[1]
Beta_hat_1_gls = coef(gls_model)[2]

# Calculate norm for GLS estimates
norm_gls = sqrt(sum((beta_1 - Beta_hat_1_gls)^2 + (beta_0 - Beta_hat_0_gls)^2)/2)

# Print GLS results
cat("\n\nGLS Estimated β₀ (Intercept) =", Beta_hat_0_gls)
cat("\n\nGLS Estimated β₁ (Slope) =", Beta_hat_1_gls)

# Compare OLS and GLS norms
cat("\n\nOLS Norm =", norm)
cat("\n\nGLS Norm =", norm_gls)


# Set up a 1x2 plotting layout
par(mfrow = c(1, 2))

# OLS residual plot
plot(x, residuals(model), 
     xlab = "X", 
     ylab = "OLS Residuals",
     main = "OLS Residual Plot",
     pch = 16)
abline(h = 0, col = "red", lty = 2)

# GLS residual plot
plot(x, residuals(gls_model), 
     xlab = "X", 
     ylab = "GLS Residuals",
     main = "GLS Residual Plot",
     pch = 16)
abline(h = 0, col = "red", lty = 2)

# Reset plotting layout
par(mfrow = c(1, 1))