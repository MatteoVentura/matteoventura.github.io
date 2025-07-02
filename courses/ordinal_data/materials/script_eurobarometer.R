############################################################
## CUB Models with DK Treatment - Eurobarometer Data
############################################################

# 1. LOAD LIBRARIES AND DATA
# --------------------------
library(CUB)
library(ggplot2)

# Load Eurobarometer dataset
load("materials/microdata_eurobarometer.Rdata")
str(eurobarometer)

# 2. FIT CUB MODELS EXCLUDING DK RESPONSES
# ----------------------------------------

# Initialize storage
CUB_list <- list()
pi <- numeric(6)
xi <- numeric(6)

# Loop through items QA19.1 to QA19.6 (columns 2 to 7)
for (i in 2:7) {
  ordinal <- eurobarometer[, i]
  
  # Remove DK responses
  ordinal <- as.numeric(ordinal[ordinal != "dk"])
  
  # Estimate CUB model
  CUB_list[[i - 1]] <- GEM(Formula(ordinal ~ 0 | 0 | 0), family = "cub")
  
  # Store parameters
  pi[i - 1] <- CUB_list[[i - 1]]$estimates[1]  # feeling
  xi[i - 1] <- CUB_list[[i - 1]]$estimates[2]  # uncertainty
}

# Create parameter summary table
param <- data.frame(
  label = paste0("QA19.", 1:6),
  pi = pi,
  xi = xi,
  stringsAsFactors = FALSE
)

# 3. VISUALIZE CUB ESTIMATES WITHOUT DK
# -------------------------------------

# Plotting in (1-pi, 1-xi) space
plot1 <- ggplot(param, aes(x = (1 - pi), y = (1 - xi), label = label)) +
  geom_point(size = 3) +
  geom_text(vjust = -1, hjust = 0.5) +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_bw() +
  labs(x = expression(1 - pi), y = expression(1 - xi),
       title = "CUB Estimates Excluding DK Responses") +
  theme(panel.grid = element_blank())
print(plot1)

# 4. COMPUTE DK PROPORTIONS
# --------------------------

p_DK <- numeric(6)

for (i in 2:7) {
  item_responses <- eurobarometer[, i]
  total_responses <- length(item_responses)
  dk_count <- sum(item_responses == "dk", na.rm = TRUE)
  p_DK[i - 1] <- dk_count / total_responses
}

# Add DK proportions to the param table
param$p_DK <- p_DK

# 5. COMPUTE CORRECTED π VALUES INCLUDING DK
# ------------------------------------------

# π_corrected = (1 - p_DK) * π_S
param$pi_cor <- (1 - param$p_DK) * param$pi

# Prepare data for visual comparison plot
param$x_old <- 1 - param$pi       # Original (1 - π)
param$x_new <- 1 - param$pi_cor   # Corrected (1 - π_corrected)
param$y_val <- 1 - param$xi       # Uncertainty remains unchanged

# 6. VISUALIZE THE EFFECT OF DK ON π
# -----------------------------------

plot2 <- ggplot(param) +
  # Arrows show shift from original to corrected π
  geom_segment(aes(x = x_old, y = y_val, xend = x_new - 0.015, yend = y_val),
               arrow = arrow(length = unit(0.2, "cm")), color = "darkgray") +
  geom_point(aes(x = x_old, y = y_val), color = "gray30", size = 2) +
  geom_point(aes(x = x_new, y = y_val), color = "black", size = 3) +
  geom_text(aes(x = x_new, y = y_val, label = label),
            vjust = -1, hjust = 0.5, size = 3.5) +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_bw() +
  labs(x = expression(1 - pi), y = expression(1 - xi),
       title = "Shift in Feeling After Accounting for DK Responses") +
  theme(panel.grid = element_blank())
print(plot2)
