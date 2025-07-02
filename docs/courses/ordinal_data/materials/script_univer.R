############################################################
## CUB Model with Covariates on Both Uncertainty and Feeling
############################################################

# Load the dataset
data(univer)

# 1. PREPARING VARIABLES
# ----------------------

# Compute centered log-transformed age:
# lage = log(age) - mean(log(age))
age <- univer$age
lage <- log(age) - mean(log(age))

# Add 'lage' to the dataset
univer$lage <- lage

# 2. FITTING THE CUB MODEL WITH COVARIATES
# ----------------------------------------

# Model specification:
# officeho ~ lage + gender       --> covariates for uncertainty (pi)
#            lage + freqserv   --> covariates for feeling (xi)
#             0                 --> no covariates for shelter (delta)
library(CUB)

cub_pai_csi <- GEM(
  Formula(officeho ~ lage + gender | lage + freqserv | 0),
  family = "cub",
  data = univer
)

# 3. MODEL RESULTS
# ----------------

# Print a detailed summary including coefficient correlations
summary(cub_pai_csi, correlation = TRUE, digits = 3)

# Extract and display estimated coefficients
coef(cub_pai_csi, digits = 3)

# The logistic regressions correspond to:
# logit(1 - pi_i) = -β0 - β1 * lage - β2 * gender
# logit(1 - xi_i) = -γ0 - γ1 * lage - γ2 * freqserv

# 4. VISUALIZING THE EFFECTS OF COVARIATES
# ----------------------------------------

# Create a sequence of centered log(age) values for prediction
ageseq <- log(seq(17, 51, by = 0.1)) - mean(log(age))
param <- coef(cub_pai_csi)

# Compute probabilities for different covariate profiles

# Males (gender = 0), Non-users (freqserv = 0)
paicov0 <- logis(cbind(ageseq, 0), param[1:3])
csicov0 <- logis(cbind(ageseq, 0), param[4:6])

# Males (gender = 0), Users (freqserv = 1)
csicov1 <- logis(cbind(ageseq, 1), param[4:6])

# Females (gender = 1), Non-users (freqserv = 0)
paicov1 <- logis(cbind(ageseq, 1), param[1:3])

# Females (gender = 1), Users (freqserv = 1)
# (same csicov1 as above, since freqserv is the only covariate for feeling)

# 5. PLOTTING THE PROFILES
# ------------------------

plot(1 - paicov0, 1 - csicov0, type = "n", col = "blue",
     xlim = c(0, 0.6), ylim = c(0.4, 0.9),
     main = "CUB models with covariates",
     xlab = expression(paste("Uncertainty ", (1 - pi))),
     ylab = expression(paste("Feeling ", (1 - xi))),
     cex.main = 0.9, cex.lab = 0.9, las = 1)

# Add the 4 profiles:
lines(1 - paicov0, 1 - csicov1, col = "black", lwd = 4)  # Man - User
lines(1 - paicov0, 1 - csicov0, col = "blue",  lwd = 4)  # Man - Non-user
lines(1 - paicov1, 1 - csicov1, col = "red",   lwd = 4)  # Woman - User
lines(1 - paicov1, 1 - csicov0, col = "green", lwd = 4)  # Woman - Non-user

legend("bottomleft", legend = c("Man - User", "Man - Non-user",
                                "Woman - User", "Woman - Non-user"),
       col = c("black", "blue", "red", "green"), lty = 1,
       text.col = c("black", "blue", "red", "green"), cex = 0.6)

# Add age annotations
text(0.1, 0.85, labels = "Young",   offset = 0.3, cex = 0.8, font = 4)
text(0.5, 0.5,  labels = "Elderly", offset = 0.3, cex = 0.8, font = 4)
