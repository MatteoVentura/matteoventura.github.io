# ======================================
# Analyzing Disciplinary Decisions in Soccer
# ======================================

# Load necessary packages
library(ordinal)  # For clm()
library(ggplot2)  # Optional: for visualization
library(dplyr)    # For data manipulation

# --------------------------------------
# Step 1: Load and Inspect the Data
# --------------------------------------
url <- "http://peopleanalytics-regression-book.org/data/soccer.csv"
soccer <- read.csv(url)

# Initial inspection
head(soccer)
str(soccer)

# --------------------------------------
# Step 2: Variable Type Conversion
# --------------------------------------

# Convert response variable to ordered factor
soccer$discipline <- ordered(soccer$discipline, 
                             levels = c("None", "Yellow", "Red"))

# Convert categorical predictors to factors
soccer$position <- factor(soccer$position)
soccer$result   <- factor(soccer$result)
soccer$country  <- factor(soccer$country)
soccer$level    <- factor(soccer$level)

# Check structure again
str(soccer)

# --------------------------------------
# Step 3: Fit Full Proportional Odds Model
# --------------------------------------
model_clm <- clm(
  discipline ~ n_yellow_25 + n_red_25 + position + 
    result + country + level,
  data = soccer
)

# View model summary
summary(model_clm)

# Odds Ratios for interpretation
exp(coef(model_clm))

# --------------------------------------
# Step 4: Simplify Model by Removing Non-significant Predictors
# --------------------------------------
model_clm_reduced <- clm(
  discipline ~ n_yellow_25 + n_red_25 + position + result,
  data = soccer
)

# Compare AICs
AIC(model_clm, model_clm_reduced)

# --------------------------------------
# Step 5: Goodness of Fit Evaluation
# --------------------------------------

# Log-likelihoods
ll_full <- logLik(model_clm_reduced)
ll_null <- logLik(update(model_clm_reduced, . ~ 1))

# Sample size
n <- nobs(model_clm_reduced)

# McFadden's R^2
r2_mcfadden <- 1 - (as.numeric(ll_full) / as.numeric(ll_null))

# Cox-Snell's R^2
r2_coxsnell <- 1 - exp((2 / n) * (as.numeric(ll_null) - as.numeric(ll_full)))

# Summarize results
data.frame(
  McFadden = r2_mcfadden,
  CoxSnell = r2_coxsnell,
  AIC = AIC(model_clm_reduced)
)

# --------------------------------------
# Step 6: Test the Proportional Odds Assumption
# --------------------------------------
nominal_test(model_clm_reduced)

# --------------------------------------
# Step 7: Confidence Intervals (Wald)
# --------------------------------------
confint(model_clm_reduced, type = "Wald")
