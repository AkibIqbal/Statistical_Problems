set.seed(123)
n <- 100  # নমুনা সাইজ
p <- 100  # ভেরিয়েবল সংখ্যা

# বিটা প্যারামিটার তৈরি
beta_0 <- 3
betas <- runif(p, -2, 2)  # -2 থেকে 2 এর মধ্যে র‍্যান্ডম বিটা ভ্যালু

# X ম্যাট্রিক্স তৈরি (n x p)
X <- matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
error <- rnorm(n, mean = 0, sd = 0.5)

# Y ভেক্টর তৈরি
y <- beta_0 + X %*% betas + error

# ডেটাফ্রেম তৈরি
df <- data.frame(y = y, X)
colnames(df)[-1] <- paste0("x", 1:p)

# মডেল ফিট
formula <- as.formula(paste("y ~", paste(paste0("x", 1:p), collapse = " + ")))
model <- lm(formula, data = df)
model_summary <- summary(model)

# বিটা এস্টিমেট
Beta_hats <- coef(model)
Beta_hat_0 <- Beta_hats[1]
Beta_hat_others <- Beta_hats[-1]

# নর্ম ক্যালকুলেশন
norm <- sqrt(sum((c(beta_0, betas) - Beta_hats)^2)/(p + 1))

# রেজাল্ট প্রিন্ট
cat("\nEstimated β₀ (Intercept) =", Beta_hat_0)
cat("\nNorm =", norm)

# আউটলায়ার সহ মডেল
X_with_outliers <- X
outlier_position <- 25

# আউটলায়ার যোগ
X_with_outliers[outlier_position, ] <- rep(5000, p)
y_with_outliers <- beta_0 + X_with_outliers %*% betas + error

# আউটলায়ার সহ ডেটাফ্রেম
df_outliers <- data.frame(y = y_with_outliers, X_with_outliers)
colnames(df_outliers)[-1] <- paste0("x", 1:p)

# আউটলায়ার সহ মডেল ফিট
model_with_outliers <- lm(formula, data = df_outliers)
Beta_hats_outliers <- coef(model_with_outliers)
Beta_hat_0_outliers <- Beta_hats_outliers[1]
Beta_hat_others_outliers <- Beta_hats_outliers[-1]

# আউটলায়ার সহ নর্ম
norm_outliers <- sqrt(sum((c(beta_0, betas) - Beta_hats_outliers)^2)/(p + 1))

cat("\n\nEstimated β₀ with outliers (Intercept) =", Beta_hat_0_outliers)
cat("\nNorm with outliers =", norm_outliers)

# R-squared ভ্যালু প্রিন্ট
cat("\n\nR-squared without outliers:", model_summary$r.squared)
cat("\nR-squared with outliers:", summary(model_with_outliers)$r.squared)

# প্রথম কয়েকটি বিটা কোএফিশিয়েন্ট প্রিন্ট
cat("\n\nFirst few beta coefficients without outliers:")
print(head(Beta_hat_others))
cat("\nFirst few beta coefficients with outliers:")
print(head(Beta_hat_others_outliers))