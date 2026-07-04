# ======================================================================
# Chapter 6 Solutions — Multicollinearity
# Logistic Regression: Bridging Theory and Practice (Doosti, 2026)
# ======================================================================
#
# NOTE: Some solutions build on objects (e.g. `model`, `data`, `Heart`) created by the
#       corresponding chapter script. Run that script first, or source it, e.g.:
#           source("../R/Chapter06_Multicollinearity.R")
#       Package dependencies are listed at the top of each solution.


# ------------------------------------------------------------------
# Solution 6.1
# ------------------------------------------------------------------
# Multicollinearity inflates variance of individual coefficients but doesn't affect predictions because collinear variables compensate for each other. The model predicts well but can't isolate individual effects.

# ------------------------------------------------------------------
# Solution 6.2
# ------------------------------------------------------------------
# (a) Weight (VIF=12.4) and Height (VIF=8.7) are concerning.
#
# (b) Source: BMI = Weight/Height^2 creates structural collinearity.
#
# (c) Solutions: (1) Keep only BMI, remove Weight/Height; (2) Use ridge regression to stabilize estimates.

# ------------------------------------------------------------------
# Solution 6.3
# ------------------------------------------------------------------
# Structural: Built into variable definitions (BMI, Weight, Height). Solution: Reconceptualize variables.
#
# Data-based: Arises from sampling (e.g., income and education highly correlated in sample). Solution: Larger/different sample, or accept limited precision.

# ------------------------------------------------------------------
# Solution 6.4
# ------------------------------------------------------------------

# Solution 6.4
set.seed(123)
rho_values <- seq(0, 0.99, by = 0.05)
n <- 200; n_sim <- 500
sd_beta <- numeric(length(rho_values))

for(r in seq_along(rho_values)) {
  betas <- numeric(n_sim)
  for(i in 1:n_sim) {
    X <- MASS::mvrnorm(n, c(0,0), matrix(c(1,rho_values[r],rho_values[r],1), 2))
    y <- rbinom(n, 1, plogis(0.5*X[,1] + 0.5*X[,2]))
    betas[i] <- coef(glm(y ~ X, family = binomial))[2]
  }
  sd_beta[r] <- sd(betas)
}

plot(rho_values, sd_beta, type = "l", xlab = "Correlation", ylab = "SD of beta1")
# Variance increases dramatically after rho > 0.8


# ------------------------------------------------------------------
# Solution 6.5
# ------------------------------------------------------------------
# VIF and condition indices generally agree but can differ: VIF is variable-specific; condition indices assess overall matrix conditioning. Very high condition index with moderate VIFs suggests multiple moderate correlations rather than one extreme pair.

# ------------------------------------------------------------------
# Solution 6.6
# ------------------------------------------------------------------

# Solution 6.6
library(glmnet)

# High collinearity data
X <- MASS::mvrnorm(200, c(0,0), matrix(c(1,0.95,0.95,1), 2))
y <- rbinom(200, 1, plogis(0.5*X[,1] + 0.5*X[,2]))

# Standard
std_mod <- glm(y ~ X, family = binomial)

# Ridge path
ridge <- glmnet(X, y, family = "binomial", alpha = 0)
plot(ridge, xvar = "lambda")

# CV for optimal lambda
cv_ridge <- cv.glmnet(X, y, family = "binomial", alpha = 0)
coef(cv_ridge, s = "lambda.min")


# ------------------------------------------------------------------
# Solution 6.7
# ------------------------------------------------------------------
# Apply to Boston housing data: identify collinearity (tax, rad often correlated), implement ridge or variable removal, compare interpretability and predictive performance.

# ------------------------------------------------------------------
# Solution 6.8
# ------------------------------------------------------------------
# PCR: (1) Compute PCs; (2) Fit logistic on first k components; (3) Transform back: beta_original = V_k beta_PC where V_k contains first k eigenvectors. Ridge and PCR equivalent when PCs are used with appropriate shrinkage.
#
# %% ============================================================
# %% CHAPTER 7 SOLUTIONS
# %% ============================================================
