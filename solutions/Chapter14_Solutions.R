# ======================================================================
# Chapter 14 Solutions — Survey Data and Complex Sampling
# Logistic Regression: Bridging Theory and Practice (Doosti, 2026)
# ======================================================================
#
# NOTE: Some solutions build on objects (e.g. `model`, `data`, `Heart`) created by the
#       corresponding chapter script. Run that script first, or source it, e.g.:
#           source("../R/Chapter14_SurveyData.R")
#       Package dependencies are listed at the top of each solution.


# ------------------------------------------------------------------
# Solution 14.1
# ------------------------------------------------------------------
# Ignoring weights biases estimates when selection probability relates to outcome. Example: Oversampling smokers in health survey--unweighted smoking prevalence estimate is biased upward.

# ------------------------------------------------------------------
# Solution 14.2
# ------------------------------------------------------------------
# Design effect: DEFF = Var_design/Var_SRS. DEFF > 1 due to: clustering (positive ICC), stratification (can reduce DEFF), unequal weights. Effective sample size = n/DEFF.

# ------------------------------------------------------------------
# Solution 14.3
# ------------------------------------------------------------------
# Clustering creates correlation; even with correct weights, independence assumption is violated. Sandwich estimator accounts for within-cluster correlation empirically.

# ------------------------------------------------------------------
# Solution 14.4
# ------------------------------------------------------------------

# Solution 14.4: Simulate complex survey
N_clusters <- 100
cluster_sizes <- rpois(N_clusters, 50)
# ... create population with clustering
# Stratified cluster sample
# Calculate weights = 1/selection_probability


# ------------------------------------------------------------------
# Solution 14.5
# ------------------------------------------------------------------

# Solution 14.5
library(survey)

# Specify design
svy_design <- svydesign(ids = ~cluster, strata = ~stratum, 
                        weights = ~weight, data = survey_data)

# Weighted logistic regression
svy_model <- svyglm(y ~ x1 + x2, design = svy_design, family = binomial)
summary(svy_model)

# Compare to unweighted
unwt_model <- glm(y ~ x1 + x2, family = binomial, data = survey_data)


# ------------------------------------------------------------------
# Solution 14.6
# ------------------------------------------------------------------
# Taylor series linearization vs bootstrap/jackknife replication. They should give similar results; bootstrap may be preferred for complex statistics.

# ------------------------------------------------------------------
# Solution 14.7
# ------------------------------------------------------------------

# Solution 14.7
library(survey)
library(nhanesA)

# Load and merge NHANES data
# Specify survey design with appropriate weight, strata, PSU
nhanes_design <- svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA,
                           weights = ~WTMEC2YR, nest = TRUE, data = nhanes)

svy_model <- svyglm(diabetes ~ age + sex + bmi, 
                    design = nhanes_design, family = binomial)


# ------------------------------------------------------------------
# Solution 14.8
# ------------------------------------------------------------------

# Correct: use subset argument
svyglm(y ~ x, design = svy_design, subset = female == 1)

# Incorrect: subsetting before analysis changes weights


# ------------------------------------------------------------------
# Solution 14.9
# ------------------------------------------------------------------
# MRP: (1) Fit multilevel model with demographic effects; (2) Predict for all population cells; (3) Weight predictions by cell population counts. Advantages: can estimate for small areas; doesn't require design variables. Disadvantages: model-dependent, requires auxiliary population data.
#
# %% ============================================================
# %% CHAPTER 15 SOLUTIONS
# %% ============================================================
