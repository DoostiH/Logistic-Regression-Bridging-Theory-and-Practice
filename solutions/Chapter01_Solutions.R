# ======================================================================
# Chapter 1 Solutions — Review of Logistic Regression Fundamentals
# Logistic Regression: Bridging Theory and Practice (Doosti, 2026)
# ======================================================================
#
# NOTE: Some solutions build on objects (e.g. `model`, `data`, `Heart`) created by the
#       corresponding chapter script. Run that script first, or source it, e.g.:
#           source("../R/Chapter01_Fundamentals.R")
#       Package dependencies are listed at the top of each solution.

# NOTE: Throughout these solutions, `Heart` refers to the cleaned Cleveland
#       dataset prepared in Chapter 1 (missing values removed; Sex, AHD,
#       ChestPain converted to factors).  e.g.  Heart <- na.omit(read.csv(...))


# ------------------------------------------------------------------
# Solution 1.1
# ------------------------------------------------------------------
# The coefficient beta_1 = 0.7 can be interpreted in three ways:
#
# (a) Log-odds interpretation: A one-unit increase in x_1 is associated with a 0.7 increase in the log-odds of the outcome Y = 1, holding other predictors constant.
#
# (b) Odds ratio interpretation: Exponentiating gives (0.7) = 2.01. A one-unit increase in x_1 is associated with a doubling (2.01 times) of the odds of the outcome, holding other predictors constant.
#
# (c) Probability interpretation: The change in probability depends on the baseline probability. Using the marginal effect formula:
#
# ( P(Y=1))/( x_1) = beta_1 * pi(1-pi)
#
# At pi = 0.5: Delta P = 0.7 x 0.5 x 0.5 = 0.175 (17.5 percentage points)
#
# At pi = 0.1: Delta P = 0.7 x 0.1 x 0.9 = 0.063 (6.3 percentage points)
#
# At pi = 0.9: Delta P = 0.7 x 0.9 x 0.1 = 0.063 (6.3 percentage points)
#
# The probability interpretation is more complex because the effect is non-linear--it depends on where you start on the probability scale. The maximum effect occurs at pi = 0.5.

# ------------------------------------------------------------------
# Solution 1.2
# ------------------------------------------------------------------
# The statement is technically incorrect. The odds ratio of 2.5 means smokers have 2.5 times higher odds of lung cancer, not 2.5 times higher probability (risk).
#
# Key distinctions:
#
# Odds ratio (OR): odds in exposedodds in unexposed = (p_1/(1-p_1))/(p_0/(1-p_0))
#
# Risk ratio (RR): (p_1)/(p_0)
#
# When OR ~ RR: The odds ratio approximates the risk ratio when the outcome is rare (typically < 10%) in both groups. This is because when p is small, odds = p/(1-p) ~ p.
#
# For lung cancer (rare outcome), OR = 2.5 would be close to RR = 2.5. But for common outcomes (e.g., 40% prevalence), the OR substantially overestimates the RR.
#
# Example: If p_0 = 0.40 and p_1 = 0.60: RR = 0.60/0.40 = 1.5, but OR = (0.60/0.40) / (0.40/0.60) = 2.25.

# ------------------------------------------------------------------
# Solution 1.3
# ------------------------------------------------------------------
# (a) The largest change in predicted probability occurs at baseline probability = 0.5. At this point, pi(1-pi) = 0.25, which is maximized.
#
# (b) Logistic regression uses the logit link function, which creates an S-shaped (sigmoid) relationship between the linear predictor and probability. The slope of this curve is steepest at pi = 0.5 and flattens near 0 and 1. Linear regression assumes a constant effect regardless of the baseline, which would allow impossible probabilities (below 0 or above 1).
#
# (c) Practical implications for communication: (1) Always specify the baseline when reporting probability changes; (2) Use predicted probability plots showing the full range; (3) Consider reporting effects at clinically meaningful baseline values; (4) Average marginal effects (AMEs) provide a single summary measure; (5) For non-technical audiences, statements like "the probability increases from X% to Y%" are more intuitive than odds ratios.

# ------------------------------------------------------------------
# Solution 1.4
# ------------------------------------------------------------------

# Solution 1.4
set.seed(123)

# (a) Single simulation
n <- 500
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rbinom(n, 1, 0.5)

# True coefficients
beta0 <- -0.5; beta1 <- 1.2; beta2 <- -0.8; beta3 <- 0.5

linear_pred <- beta0 + beta1*x1 + beta2*x2 + beta3*x3
p <- plogis(linear_pred)
y <- rbinom(n, 1, p)

model <- glm(y ~ x1 + x2 + x3, family = binomial)

# Compare to true values
cbind(True = c(beta0, beta1, beta2, beta3), 
      Estimate = coef(model))

# (b) Odds ratios with 95% CI
or_ci <- exp(cbind(OR = coef(model), confint.default(model)))
print(or_ci)

# Check if CIs contain true ORs
true_or <- exp(c(beta0, beta1, beta2, beta3))
within_ci <- true_or >= or_ci[,2] & true_or <= or_ci[,3]

# (c) Coverage probability simulation
n_sim <- 1000
coverage <- matrix(NA, n_sim, 4)

for(i in 1:n_sim) {
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rbinom(n, 1, 0.5)
  linear_pred <- beta0 + beta1*x1 + beta2*x2 + beta3*x3
  y <- rbinom(n, 1, plogis(linear_pred))
  
  model <- glm(y ~ x1 + x2 + x3, family = binomial)
  ci <- confint.default(model)
  
  true_beta <- c(beta0, beta1, beta2, beta3)
  coverage[i,] <- true_beta >= ci[,1] & true_beta <= ci[,2]
}

# Coverage probabilities (should be ~0.95)
colMeans(coverage)

# Expected output: Coverage probabilities should be approximately 0.95 for all parameters, confirming that the Wald confidence intervals have correct nominal coverage in this setting with n = 500.

# ------------------------------------------------------------------
# Solution 1.5
# ------------------------------------------------------------------

# Solution 1.5
library(marginaleffects)

# Fit model (assuming Heart data loaded)
model <- glm(AHD ~ Age + Sex + ChestPain + MaxHR + Oldpeak, 
             family = binomial, data = Heart)

# (a) Average Marginal Effect of Age
# Method 1: Manual calculation
beta_age <- coef(model)["Age"]
pred_prob <- predict(model, type = "response")
marginal_effects <- beta_age * pred_prob * (1 - pred_prob)
AME_age <- mean(marginal_effects)
cat("AME of Age:", round(AME_age, 4), "\n")

# Method 2: Using marginaleffects package
avg_slopes(model, variables = "Age")

# (b) Marginal Effect at the Mean (MEM)
# Create data point at means of continuous, mode of categorical
newdata_mean <- data.frame(
  Age = mean(Heart$Age),
  Sex = factor("Male", levels = levels(Heart$Sex)),
  ChestPain = factor("asymptomatic", levels = levels(Heart$ChestPain)),
  MaxHR = mean(Heart$MaxHR),
  Oldpeak = mean(Heart$Oldpeak)
)
pi_mean <- predict(model, newdata = newdata_mean, type = "response")
MEM_age <- beta_age * pi_mean * (1 - pi_mean)
cat("MEM of Age:", round(MEM_age, 4), "\n")

# (c) AME and MEM differ when:
# - Distribution of predicted probabilities is skewed
# - Substantial heterogeneity in the sample
# - Non-linear effects or interactions exist


# ------------------------------------------------------------------
# Solution 1.6
# ------------------------------------------------------------------

# Solution 1.6: Publication-ready table function
make_or_table <- function(model, digits = 3) {
  coefs <- coef(model)
  se <- sqrt(diag(vcov(model)))
  z <- coefs / se
  p <- 2 * pnorm(-abs(z))
  ci <- confint.default(model)
  or <- exp(coefs)
  or_lower <- exp(ci[, 1])
  or_upper <- exp(ci[, 2])
  
  stars <- cut(p, breaks = c(0, 0.001, 0.01, 0.05, 0.1, 1),
               labels = c("***", "**", "*", ".", ""), right = FALSE)
  
  result <- data.frame(
    Estimate = round(coefs, digits),
    OR = round(or, digits),
    CI_Lower = round(or_lower, digits),
    CI_Upper = round(or_upper, digits),
    SE = round(se, digits),
    p_value = format.pval(p, digits = 3),
    Sig = as.character(stars)
  )
  
  result$OR_95CI <- paste0(result$OR, " (", result$CI_Lower, 
                           "-", result$CI_Upper, ")")
  return(result)
}

# Test on model
table_output <- make_or_table(model)
print(table_output[, c("Estimate", "OR_95CI", "SE", "p_value", "Sig")])


# ------------------------------------------------------------------
# Solution 1.7
# ------------------------------------------------------------------

# Solution 1.7
library(ISLR2)
library(pROC)
library(dplyr)   # for ntile() in part (d)
data(Default)

# Fit model
model <- glm(default ~ balance + income + student, 
             family = binomial, data = Default)

# (a) Interpret odds ratios
exp(cbind(OR = coef(model), confint.default(model)))
# Balance: OR = 1.0057 per dollar; per $1000: exp(0.0057*1000) = 298.9
# Income: OR $\approx$ 1.0 (not significant after controlling for balance)
# Student: OR = 0.48, students have 52% lower odds (controlling for balance)

# (b) Predicted probability plot
library(ggplot2)
newdata <- expand.grid(
  balance = seq(0, 2700, length.out = 100),
  income = mean(Default$income),
  student = c("No", "Yes")
)
newdata$prob <- predict(model, newdata, type = "response")

ggplot(newdata, aes(x = balance, y = prob, color = student)) +
  geom_line(size = 1.2) +
  labs(x = "Credit Card Balance ($)", y = "P(Default)") +
  theme_minimal()

# (c) ROC curve and AUC
pred_prob <- predict(model, type = "response")
roc_obj <- roc(Default$default, pred_prob)
plot(roc_obj)
auc(roc_obj)  # ~0.95

# Youden's J optimal threshold
coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))

# (d) Calibration plot
Default$pred_prob <- pred_prob
Default$decile <- ntile(pred_prob, 10)

cal_data <- aggregate(cbind(pred_prob, as.numeric(default == "Yes")) ~ decile, 
                      data = Default, FUN = mean)
names(cal_data) <- c("decile", "mean_pred", "obs_rate")

plot(cal_data$mean_pred, cal_data$obs_rate, 
     xlab = "Predicted", ylab = "Observed", xlim = c(0, 0.8), ylim = c(0, 0.8))
abline(0, 1, lty = 2)


# ------------------------------------------------------------------
# Solution 1.8
# ------------------------------------------------------------------
# For logistic regression with a single predictor: logit(pi_i) = beta_0 + beta_1 x_i
#
# The Fisher Information Matrix is: I(beta) = X^T W X, where W = diag(pi_i(1-pi_i)).
#
# For a single predictor:
#
# Var(beta_hat_1) = (1)/(_i=1)^n (x_i - x)^2 pi_i(1-pi_i)
#
# This depends on: (1) Sample size n: Larger n decreases variance; (2) Spread of x: Larger (x_i - x)^2 decreases variance; (3) Balance of outcomes: pi_i(1-pi_i) is maximized when pi_i = 0.5.
#
# Optimal predictor distribution: Minimizing Var(beta_hat_1) involves a trade-off --
# spreading x increases (x_i - xbar)^2, but pushing x to extremes drives pi_i toward 0 or 1,
# which shrinks pi_i(1 - pi_i). The optimal balance is a two-point design placing observations
# symmetrically at the x-values where pi ~ 0.18 and pi ~ 0.82 (NOT at pi = 0.5 or at the extremes of x).
#
# %% ============================================================
# %% CHAPTER 2 SOLUTIONS
# %% ============================================================

