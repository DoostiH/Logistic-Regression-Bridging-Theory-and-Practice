# ======================================================================
# Chapter 15 Solutions — Bayesian and Causal Methods
# Logistic Regression: Bridging Theory and Practice (Doosti, 2026)
# ======================================================================
#
# NOTE: Some solutions build on objects (e.g. `model`, `data`, `Heart`) created by the
#       corresponding chapter script. Run that script first, or source it, e.g.:
#           source("../R/Chapter15_BayesianCausal.R")
#       Package dependencies are listed at the top of each solution.


# ------------------------------------------------------------------
# Solution 15.1
# ------------------------------------------------------------------
# Confidence interval: In repeated sampling, 95% of such intervals contain the true value.
#
# Credible interval: There is 95% probability the parameter lies in this interval (given the data).
#
# The credible interval interpretation matches common intuition better.

# ------------------------------------------------------------------
# Solution 15.2
# ------------------------------------------------------------------
# Flat priors can be improper and lead to numerical issues with separation. Weakly informative priors (e.g., beta  N(0, 2.5) for standardized predictors) provide regularization while letting data dominate inference.

# ------------------------------------------------------------------
# Solution 15.3
# ------------------------------------------------------------------
# Confounding: A common cause of treatment and outcome. Adjusting for more variables can introduce collider bias (adjusting for a common effect of treatment and outcome) or mediator bias (blocking causal pathways). Use DAGs to identify appropriate adjustment sets.

# ------------------------------------------------------------------
# Solution 15.4
# ------------------------------------------------------------------
# PS Matching: Intuitive, handles many confounders, may discard data.
#
# IPW: Uses all data, directly models treatment mechanism, sensitive to extreme weights.
#
# Prefer matching for small samples; IPW for large samples with good overlap.

# ------------------------------------------------------------------
# Solution 15.5
# ------------------------------------------------------------------

# Solution 15.5
library(rstanarm)

bayes_model <- stan_glm(y ~ x1 + x2, family = binomial, data = data,
                        prior = normal(0, 2.5), prior_intercept = normal(0, 10))

# Diagnostics
mcmc_trace(bayes_model)
summary(bayes_model)

# Credible intervals
posterior_interval(bayes_model, prob = 0.95)


# ------------------------------------------------------------------
# Solution 15.6
# ------------------------------------------------------------------

# Solution 15.6: Propensity score methods simulation
library(MatchIt)
library(WeightIt)

# Generate confounded data with known true effect
n <- 1000
x <- rnorm(n)
a <- rbinom(n, 1, plogis(0.5*x))  # Treatment depends on x
y <- rbinom(n, 1, plogis(-1 + 0.5*a + 0.3*x))  # True effect = 0.5

# (a) Unadjusted
unadj <- glm(y ~ a, binomial)

# (b) Covariate adjustment
adj <- glm(y ~ a + x, binomial)

# (c) PS matching
m_out <- matchit(a ~ x, method = "nearest")
matched <- match.data(m_out)
ps_match <- glm(y ~ a, binomial, data = matched)

# (d) IPW
W <- weightit(a ~ x, method = "ps")
ipw_mod <- glm(y ~ a, binomial, weights = W$weights)

# (e) Doubly robust
dr_mod <- glm(y ~ a + x, binomial, weights = W$weights)


# ------------------------------------------------------------------
# Solution 15.7
# ------------------------------------------------------------------

# Solution 15.7: Balance diagnostics
library(cobalt)

# Before matching
bal.tab(a ~ x, data = data)

# After matching
bal.tab(m_out)

# Love plot
love.plot(m_out, threshold = 0.1)

# Threshold: Standardized mean difference < 0.1 indicates good balance.

# ------------------------------------------------------------------
# Solution 15.8
# ------------------------------------------------------------------

# Solution 15.8: E-value
library(EValue)

# OR = 2.5, 95% CI: 1.8-3.5
evalues.OR(2.5, 1.8, 3.5, rare = FALSE)

# E-value for point estimate
# E-value for CI bound

# E-value = minimum strength of unmeasured confounding (on RR scale) needed to explain away the observed association. Larger E-value = more robust to unmeasured confounding.

# ------------------------------------------------------------------
# Solution 15.9
# ------------------------------------------------------------------
# Comprehensive analysis: Create DAG, estimate PS, check overlap and balance, apply multiple methods, compare estimates, conduct sensitivity analysis with E-values.

# ------------------------------------------------------------------
# Solution 15.10
# ------------------------------------------------------------------
# Instrumental variable conditions: (1) Relevance: IV predicts treatment; (2) Exclusion: IV affects outcome only through treatment; (3) Independence: IV independent of unmeasured confounders. Implementation via two-stage methods or ivprobit.
#
# %% ============================================================
# %% CHAPTER 16 SOLUTIONS
# %% ============================================================
