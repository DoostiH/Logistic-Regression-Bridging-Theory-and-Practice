# ======================================================================
# Chapter 13 Solutions — Handling Missing Data
# Logistic Regression: Bridging Theory and Practice (Doosti, 2026)
# ======================================================================
#
# NOTE: Some solutions build on objects (e.g. `model`, `data`, `Heart`) created by the
#       corresponding chapter script. Run that script first, or source it, e.g.:
#           source("../R/Chapter13_MissingData.R")
#       Package dependencies are listed at the top of each solution.


# ------------------------------------------------------------------
# Solution 13.1
# ------------------------------------------------------------------
# MCAR: Missingness unrelated to any variables. Example: Data randomly lost due to computer crash.
#
# MAR: Missingness related to observed variables. Example: Younger patients more likely to skip follow-up.
#
# MNAR: Missingness related to the missing value itself. Example: Depressed patients less likely to report depression scores.

# ------------------------------------------------------------------
# Solution 13.2
# ------------------------------------------------------------------
# Complete case analysis valid only under MCAR. Problems: (1) Reduced power; (2) Bias if MAR or MNAR; (3) May lose important subgroups.

# ------------------------------------------------------------------
# Solution 13.3
# ------------------------------------------------------------------
# Single imputation replaces missing values once, treating imputed values as real. This underestimates variance. Rubin's rules: (1) Analyze each imputed dataset; (2) Pool estimates: beta = (1)/(m) beta_hat_i; (3) Combine variances: T = W + (1+1/m)B where W is within-imputation variance and B is between-imputation variance.

# ------------------------------------------------------------------
# Solution 13.4
# ------------------------------------------------------------------

# Solution 13.4
library(mice)

# Create MAR data
data_complete <- data.frame(x1 = rnorm(500), x2 = rnorm(500))
data_complete$y <- rbinom(500, 1, plogis(0.5*data_complete$x1))

# MAR: missingness depends on x2
miss_prob <- plogis(-1 + data_complete$x2)
data_mar <- data_complete
data_mar$x1[runif(500) < miss_prob] <- NA

# Compare methods
cc_est <- coef(glm(y ~ x1, binomial, data_mar, na.action = na.omit))

imp <- mice(data_mar, m = 20, printFlag = FALSE)
mi_results <- with(imp, glm(y ~ x1, family = binomial))
mi_est <- pool(mi_results)


# ------------------------------------------------------------------
# Solution 13.5
# ------------------------------------------------------------------

# Solution 13.5
library(mice)

# Examine patterns
md.pattern(data)

# Impute
imp <- mice(data, m = 20, method = c("pmm", "logreg", "norm"))

# Analyze and pool
fit <- with(imp, glm(y ~ x1 + x2, family = binomial))
summary(pool(fit))


# ------------------------------------------------------------------
# Solution 13.6
# ------------------------------------------------------------------

# Solution 13.6
library(naniar)
library(finalfit)

# Little's MCAR test
mcar_test(data)

# Model missingness
data$x1_missing <- is.na(data$x1)
miss_model <- glm(x1_missing ~ x2 + y, family = binomial, data = data)
# If significant predictors exist, not MCAR


# ------------------------------------------------------------------
# Solution 13.7
# ------------------------------------------------------------------

# Solution 13.7
library(mice)
data(nhanes)

vis_miss(nhanes)
imp <- mice(nhanes, m = 20)
fit <- with(imp, glm(hyp ~ age + bmi, family = binomial))
summary(pool(fit))


# ------------------------------------------------------------------
# Solution 13.8
# ------------------------------------------------------------------
# Pattern-mixture model separates analysis by missingness pattern. Tipping point analysis: shift MNAR estimates by  and observe when conclusions change. If large  required, results are robust.
#
# %% ============================================================
# %% CHAPTER 14 SOLUTIONS
# %% ============================================================
