# ======================================================================
# Chapter 7 Solutions — Non-linearity in Predictors
# Logistic Regression: Bridging Theory and Practice (Doosti, 2026)
# ======================================================================
#
# NOTE: Some solutions build on objects (e.g. `model`, `data`, `Heart`) created by the
#       corresponding chapter script. Run that script first, or source it, e.g.:
#           source("../R/Chapter07_Nonlinearity.R")
#       Package dependencies are listed at the top of each solution.


# ------------------------------------------------------------------
# Solution 7.1
# ------------------------------------------------------------------
# Linearity in log-odds: (pi/(1-pi)) = beta_0 + beta_1 x. A unit increase in x changes log-odds by beta_1 regardless of starting point. Linearity in probability would allow impossible values outside [0,1].

# ------------------------------------------------------------------
# Solution 7.2
# ------------------------------------------------------------------
# Polynomials: Simple but global, can behave badly at extremes.
#
# Categorization: Loses information, arbitrary cutpoints.
#
# Splines: Flexible, local, well-behaved, but more parameters.
#
# GAMs: Most flexible, automatic smoothness selection, harder to interpret.

# ------------------------------------------------------------------
# Solution 7.3
# ------------------------------------------------------------------
# Critique: Categorization loses information (10-20% power loss), creates arbitrary boundaries, assumes constant effect within groups. Better: use splines to model continuous age effect.

# ------------------------------------------------------------------
# Solution 7.4
# ------------------------------------------------------------------

# Solution 7.4: Empirical logit plots
empirical_logit_plot <- function(x, y, ngroups = 10) {
  groups <- cut(x, breaks = quantile(x, probs = seq(0, 1, length = ngroups + 1)),
                include.lowest = TRUE)
  agg <- aggregate(y ~ groups, FUN = function(z) {
    p <- mean(z)
    c(mean_x = mean(x[groups == unique(groups)[1]]), 
      logit = log(p/(1-p)))
  })
  # Plot with loess smooth
}


# ------------------------------------------------------------------
# Solution 7.5
# ------------------------------------------------------------------

# Solution 7.5
library(splines)

# True quadratic relationship
x <- rnorm(500)
y <- rbinom(500, 1, plogis(-1 + x - 0.5*x^2))

# Fit models
mod_lin <- glm(y ~ x, family = binomial)
mod_quad <- glm(y ~ x + I(x^2), family = binomial)
mod_ns3 <- glm(y ~ ns(x, df = 3), family = binomial)
mod_ns5 <- glm(y ~ ns(x, df = 5), family = binomial)

# Compare AIC
AIC(mod_lin, mod_quad, mod_ns3, mod_ns5)


# ------------------------------------------------------------------
# Solution 7.6
# ------------------------------------------------------------------
# LRT comparing linear to spline model. Under true linearity, Type I error should be ~ 0.05. The test has good power to detect moderate non-linearity.

# ------------------------------------------------------------------
# Solution 7.7
# ------------------------------------------------------------------

# Solution 7.7
library(mgcv)
data(Wage, package = "ISLR2")
Wage$high_wage <- as.numeric(Wage$wage > 100)

gam_mod <- gam(high_wage ~ s(age) + s(year) + education, 
               family = binomial, data = Wage)
plot(gam_mod, pages = 1)


# ------------------------------------------------------------------
# Solution 7.8
# ------------------------------------------------------------------
# Effective df for smoothing spline: edf = trace(S_) where S_ is the smoother matrix. As   0, edf  n; as   Inf, edf  2 (linear).
#
# %% ============================================================
# %% CHAPTER 8 SOLUTIONS
# %% ============================================================
