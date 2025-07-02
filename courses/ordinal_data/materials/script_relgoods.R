############################################################
## CUB Model with Shelter Effect - Writing Variable
############################################################

# 1. LOAD REQUIRED PACKAGE AND DATASET
# ------------------------------------
library(CUB)

# Load the 'relgoods' dataset
data(relgoods)

# 2. VISUALIZE THE DISTRIBUTION OF THE WRITING VARIABLE
# ------------------------------------------------------

# Plot the frequency of responses for 'Writing'
plot(table(as.factor(relgoods$Writing)),
     main = "Frequency Distribution of Writing Ratings",
     xlab = "Rating (1 to 10)",
     ylab = "Frequency",
     col = "black")


# 3. FIT A STANDARD CUB MODEL (WITHOUT SHELTER OPTION)
# ----------------------------------------------------

# Fit a standard CUB model to the 'Writing' ratings
cub_writing <- GEM(Formula(Writing ~ 0 | 0 | 0),
                   family = "cub",
                   maxiter = 500,
                   toler = 1e-3,
                   data = relgoods)

# Display model summary
summary(cub_writing)

# Plot observed vs fitted probabilities
makeplot(cub_writing)

# 4. FIT A CUB MODEL WITH SHELTER EFFECT
# --------------------------------------

# Fit a CUB model where category 1 is treated as a shelter
cub_she <- GEM(Formula(Writing ~ 0 | 0 | 0),
               family = "cub",
               shelter = 1,    # Category 1 is the shelter
               maxiter = 500,
               toler = 1e-3,
               data = relgoods)

# Display model summary
summary(cub_she)

# Plot observed vs fitted probabilities
makeplot(cub_she)


# Compute BIC values for model comparison
bic <- BIC(cub_writing)
bic_shelter  <- BIC(cub_she)


