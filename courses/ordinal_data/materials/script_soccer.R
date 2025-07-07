# ======================================
# Analyzing Disciplinary Decisions in Soccer
# ======================================

# Load necessary packages
library(ordinal)   # For clm()
library(ggplot2)   # Optional: for visualization
library(dplyr)     # For data manipulation
library(nnet)      # For multinom()
library(broom)     # For tidy summaries

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
# Step 3: Proportional Odds Model (Reduced)
# --------------------------------------
model_clm_reduced <- clm(
  discipline ~ n_yellow_25 + n_red_25 + position + result,
  data = soccer
)

summary(model_clm_reduced)
exp(coef(model_clm_reduced))

# --------------------------------------
# Step 4: Linear Regression (not appropriate, for comparison)
# --------------------------------------
model_lm <- lm(as.numeric(discipline) ~ n_yellow_25 + n_red_25 + position + result, data = soccer)
summary(model_lm)

# --------------------------------------
# Step 5: Binary Logistic Regression (None vs Yellow/Red)
# --------------------------------------
soccer$discipline_bin <- ifelse(soccer$discipline == "None", "No", "Yes")
soccer$discipline_bin <- factor(soccer$discipline_bin)

model_logit <- glm(discipline_bin ~ n_yellow_25 + n_red_25 + position + result, 
                   data = soccer, family = "binomial")
summary(model_logit)
exp(coef(model_logit))

# --------------------------------------
# Step 6: Multinomial Logistic Regression (Non-Ordinal)
# --------------------------------------
model_multinom <- multinom(discipline ~ n_yellow_25 + n_red_25 + position + result, data = soccer)
summary(model_multinom)

# Odds Ratios for multinomial model
exp(coef(model_multinom))

# --------------------------------------
# Step 7: Model Comparison (AIC and BIC)
# --------------------------------------
BIC(model_clm_reduced, model_logit, model_multinom, model_lm)

# --------------------------------------
# Step 8: Goodness-of-Fit for Ordinal Model
# --------------------------------------
ll_full <- logLik(model_clm_reduced)
ll_null <- logLik(update(model_clm_reduced, . ~ 1))
n <- nobs(model_clm_reduced)

r2_mcfadden <- 1 - (as.numeric(ll_full) / as.numeric(ll_null))
r2_coxsnell <- 1 - exp((2 / n) * (as.numeric(ll_null) - as.numeric(ll_full)))

data.frame(
  McFadden = r2_mcfadden,
  CoxSnell = r2_coxsnell,
  AIC = AIC(model_clm_reduced)
)

# --------------------------------------
# Step 9: Proportional Odds Assumption
# --------------------------------------
nominal_test(model_clm_reduced)

# --------------------------------------
# Step 10: Confidence Intervals (Wald)
# --------------------------------------
confint(model_clm_reduced, type = "Wald")
