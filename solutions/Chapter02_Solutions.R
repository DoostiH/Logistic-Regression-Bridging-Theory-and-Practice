# ======================================================================
# Chapter 2 Solutions — Complete and Quasi-Complete Separation
# Logistic Regression: Bridging Theory and Practice (Doosti, 2026)
# ======================================================================
#
# NOTE: Some solutions build on objects (e.g. `model`, `data`, `Heart`) created by the
#       corresponding chapter script. Run that script first, or source it, e.g.:
#           source("../R/Chapter02_Separation.R")
#       Package dependencies are listed at the top of each solution.


# ------------------------------------------------------------------
# Solution 2.1
# ------------------------------------------------------------------
# Complete Separation: A predictor (or linear combination) perfectly predicts the outcome. All observations with x > c have Y = 1, and all with x < c have Y = 0.
#
# Example: Treatment where all treated patients survive, all controls die:
#
# & Y = 0 & Y = 1 \\
#
# Treatment = 0 & 20 & 0 \\
# Treatment = 1 & 0 & 20 \\
#
# Quasi-Complete Separation: Almost perfect prediction with observations only on the boundary.
#
# Example:
#
# & Y = 0 & Y = 1 \\
#
# Treatment = 0 & 20 & 0 \\
# Treatment = 1 & 2 & 18 \\

# ------------------------------------------------------------------
# Solution 2.2
# ------------------------------------------------------------------
# When separation occurs, the likelihood function becomes monotonically increasing in the separating parameter.
#
# Intuitive explanation: In logistic regression, increasing |beta_1| pushes predicted probabilities closer to 0 or 1. If x perfectly separates outcomes, this always increases the likelihood. The likelihood keeps increasing as beta_1  , so no finite MLE exists.

# ------------------------------------------------------------------
# Solution 2.3
# ------------------------------------------------------------------
# (a) Problem: The output indicates separation--extremely large estimates, huge SEs, non-significant p-values despite large effects.
#
# (b) Diagnostics:

library(detectseparation)
glm(y ~ x1, family = binomial, method = "detect_separation")
table(data$x1, data$y)  # Cross-tabulate

# (c) Solutions: (1) Firth's penalized likelihood: logistf::logistf(); (2) Bayesian approach: rstanarm::stan_glm(); (3) Exact logistic regression for small samples; (4) Collect more data in the boundary region.

# ------------------------------------------------------------------
# Solution 2.4
# ------------------------------------------------------------------
# p4cmp4cmp3cm
#
# Method & Advantages & Disadvantages & When to use \\
#
# Firth's & Simple, fast, finite estimates & Slight bias toward null & Default choice \\
#
# Bayesian & Full posterior, flexible priors & Requires prior specification & Uncertainty quantification \\
#
# Exact & Exact inference & Computationally expensive & Small samples \\

# ------------------------------------------------------------------
# Solution 2.5
# ------------------------------------------------------------------

# Solution 2.5
library(logistf)
library(rstanarm)

set.seed(123)
n <- 100
x <- rnorm(n)
y <- as.numeric(x > 0.5)
flip_idx <- which(x > 0.5)[1:2]
y[flip_idx] <- 0  # Create quasi-separation

data <- data.frame(x = x, y = y)

# Standard MLE
mle_model <- glm(y ~ x, family = binomial, data = data)

# Firth's method
firth_model <- logistf(y ~ x, data = data)

# Bayesian
bayes_model <- stan_glm(y ~ x, family = binomial, data = data, refresh = 0)

# Compare
data.frame(
  Method = c("MLE", "Firth", "Bayesian"),
  Intercept = c(coef(mle_model)[1], coef(firth_model)[1], coef(bayes_model)[1]),
  Slope = c(coef(mle_model)[2], coef(firth_model)[2], coef(bayes_model)[2])
)

# Finding: MLE has inflated coefficients and huge SEs. Firth and Bayesian produce stable, reasonable estimates.

# ------------------------------------------------------------------
# Solution 2.6
# ------------------------------------------------------------------

# Solution 2.6
library(detectseparation)

check_separation <- function(formula, data) {
  sep_check <- glm(formula, family = binomial, data = data,
                   method = "detect_separation")
  sep_info <- sep_check$separation
  
  if(any(sep_info)) {
    problem_vars <- names(sep_info)[sep_info]
    return(list(separated = TRUE, variables = problem_vars,
                message = paste("Separation detected in:", 
                               paste(problem_vars, collapse = ", "))))
  } else {
    return(list(separated = FALSE, variables = NULL,
                message = "No separation detected."))
  }
}


# ------------------------------------------------------------------
# Solution 2.7
# ------------------------------------------------------------------

# Solution 2.7
library(logistf)
data(infert)

# (a) Standard model
model_std <- glm(case ~ induced + spontaneous + induced:spontaneous,
                 family = binomial, data = infert)

# (b) Check separation and apply Firth if needed
model_firth <- logistf(case ~ induced + spontaneous + induced:spontaneous,
                       data = infert)

# (c) Compare ORs
data.frame(Standard = exp(coef(model_std)), 
           Firth = exp(coef(model_firth)))

# (d) Both induced and spontaneous abortions increase infertility risk


# ------------------------------------------------------------------
# Solution 2.8
# ------------------------------------------------------------------
# Firth's modified score maximizes: ^*(beta) = (beta) + (1)/(2)|I(beta)|
#
# Jeffreys' prior is: pi(beta)  |I(beta)|^1/2
#
# The posterior with Jeffreys' prior:  p(beta|y) = (beta) + (1)/(2)|I(beta)| + const
#
# This equals Firth's penalized likelihood. Thus, Firth's estimates are posterior modes under Jeffreys' prior, providing a Bayesian justification for the frequentist method.
#
# %% ============================================================
# %% CHAPTER 3 SOLUTIONS
# %% ============================================================
