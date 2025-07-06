# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
# install.packages("ggmosaic") # Uncomment and run if ggmosaic is not installed
library(ggmosaic)


# --- Bar Chart: Customer Satisfaction Levels ---

# Create sample data for customer satisfaction
satisfaction <- data.frame(
  level = factor(c("Very Dissatisfied", "Dissatisfied", "Neutral", "Satisfied", "Very Satisfied"),
                 levels = c("Very Dissatisfied", "Dissatisfied", "Neutral", "Satisfied", "Very Satisfied")),
  count = c(15, 23, 30, 45, 27)
)

# Create bar chart with ordered categories
ggplot(satisfaction, aes(x = level, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Customer Satisfaction Levels",
       x = "Satisfaction Level",
       y = "Number of Responses") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# --- Horizontal Bar Chart: Likert Scale Responses ---

# Create sample Likert scale data for one survey question
likert_data_single <- data.frame(
  response_category = factor(
    c("Strongly Disagree", "Disagree",
      "Neutral", "Agree", "Strongly Agree"),
    levels = c("Strongly Disagree", "Disagree",
               "Neutral", "Agree", "Strongly Agree")
  ),
  frequency = c(15, 27, 43, 85, 30)
)

# Create horizontal bar chart with properly ordered categories
ggplot(likert_data_single, aes(x = response_category,
                               y = frequency,
                               fill = response_category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(
    "Strongly Disagree" = "#d7191c",
    "Disagree" = "#fdae61",
    "Neutral" = "#ffffbf",
    "Agree" = "#abd9e9",
    "Strongly Agree" = "#2c7bb6"
  )) +
  coord_flip() + # Horizontal orientation for better label readability
  theme_minimal() +
  labs(
    title = "Responses to: 'The new software \ninterface is intuitive to use'", # Added newline for better title display
    subtitle = "Distribution of 200 employee responses",
    x = "",
    y = "Number of Responses"
  ) +
  theme(
    legend.position = "none", # Remove legend as colors are self-explanatory
    axis.text.y = element_text(size = 12),
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank() # Remove horizontal grid lines
  )



# --- Stacked Bar Chart: Semantic Differential Scale Evaluations ---

# Create sample semantic differential scale data
# This represents evaluations of three different smartphones on five dimensions
semantic_data <- data.frame(
  product = rep(c("Smartphone A", "Smartphone B", "Smartphone C"), each = 5),
  dimension = rep(c("Ineffective - Effective",
                    "Complicated - Simple",
                    "Unreliable - Reliable",
                    "Outdated - Innovative",
                    "Unattractive - Attractive"), 3),
  rating_1 = c(5, 8, 3, 10, 7,       # Smartphone A
               2, 5, 2, 3, 4,        # Smartphone B
               8, 6, 4, 5, 3),       # Smartphone C
  rating_2 = c(10, 12, 15, 8, 13,    # Smartphone A
               8, 10, 7, 9, 11,      # Smartphone B
               12, 8, 9, 10, 7),     # Smartphone C
  rating_3 = c(25, 20, 22, 18, 20,   # Smartphone A
               15, 20, 18, 13, 15,   # Smartphone B
               20, 22, 18, 15, 20),  # Smartphone C
  rating_4 = c(35, 30, 32, 40, 35,   # Smartphone A
               45, 35, 40, 38, 35,   # Smartphone B
               30, 35, 38, 40, 35),  # Smartphone C
  rating_5 = c(25, 30, 28, 24, 25,   # Smartphone A
               30, 30, 33, 37, 35,   # Smartphone B
               30, 29, 31, 30, 35)   # Smartphone C
)

# Reshape data for ggplot
semantic_long <- semantic_data %>%
  pivot_longer(cols = starts_with("rating_"),
               names_to = "rating_level",
               values_to = "count") %>%
  mutate(
    rating_number = as.numeric(substr(rating_level, 8, 8)),
    rating_label = factor(
      case_when(
        rating_number == 1 ~ "1 (Negative)",
        rating_number == 2 ~ "2",
        rating_number == 3 ~ "3 (Neutral)",
        rating_number == 4 ~ "4",
        rating_number == 5 ~ "5 (Positive)"
      ),
      levels = c("1 (Negative)", "2", "3 (Neutral)", "4", "5 (Positive)")
      # levels = c("5 (Positive)", "4", "3 (Neutral)", "2", "1 (Negative)")
    )
  )

# Calculate percentages for each product-dimension combination
semantic_pct <- semantic_long %>%
  group_by(product, dimension) %>%
  mutate(
    percentage = count / sum(count) * 100,
    total = sum(count)
  ) %>%
  ungroup()

# Create stacked bar chart
ggplot(semantic_pct, aes(x = dimension, y = percentage, fill = rating_label)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  facet_wrap(~ product) +
  scale_fill_manual(values = c("1 (Negative)" = "#d7191c",
                               "2" = "#fdae61",
                               "3 (Neutral)" = "#ffffbf",
                               "4" = "#a6d96a",
                               "5 (Positive)" = "#1a9641")) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Smartphone Evaluations using Semantic Differential Scales",
       subtitle = "Distribution of ratings across five dimensions",
       x = "",
       y = "Percentage of Responses",
       fill = "Rating") +
  # guides(fill = guide_legend(reverse = TRUE)) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "lightblue", color = NA),
    strip.text = element_text(face = "bold")
  )



# --- Divergent Stacked Bar Chart: Likert Scale Survey Responses ---

# Install and load required package
# install.packages("HH")
library(HH)

# Frequency data for three items (rows) on a 5-point Likert scale
likert_data <- data.frame(
  "Easy" = c(5, 15, 20, 40, 20),
  "Helpful" = c(10, 18, 25, 30, 17),
  "Recommend" = c(8, 12, 15, 35, 30)
)

# Set column names (Likert scale labels)
rownames(likert_data) <- c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
likert_data <- t(likert_data)  # Transpose: items as columns, scale points as rows

# Create the Likert plot
likert_plot <- likert(likert_data,
                      main = "Customer Feedback on Product Experience",
                      xlab = "Percentage of Responses",
                      ylab = NULL,
                      positive.order = TRUE,
                      reference = 0)


# Display the plot
print(likert_plot)


# --- Mosaic Plot: Education Level and Job Satisfaction ---

# Create sample data for education level (ordinal) and job satisfaction (ordinal)
set.seed(123)
n <- 500

education_levels <- c("High School", "Associate's", "Bachelor's",
                      "Master's", "Doctorate")
satisfaction_levels <- c("Very Dissatisfied", "Dissatisfied",
                         "Neutral", "Satisfied", "Very Satisfied")

# Create sample data with a pattern (higher education tends to correlate with higher satisfaction)
mosaic_data <- data.frame(
  education = factor(sample(education_levels, n, replace = TRUE,
                            prob = c(0.3, 0.25, 0.25, 0.15, 0.05)),
                     levels = education_levels),
  satisfaction = factor(NA, levels = satisfaction_levels)
)

# Generate satisfaction levels with some correlation to education
for (i in 1:n) {
  edu_level <- which(education_levels == mosaic_data$education[i])
  probs <- c(0.25, 0.25, 0.2, 0.2, 0.1) # Base probabilities
  shift <- (edu_level - 3) * 0.05 # Shift factor based on education
  adjusted_probs <- probs + c(-0.1, -0.05, 0, 0.05, 0.1) * edu_level
  adjusted_probs <- pmax(adjusted_probs, 0.01)
  adjusted_probs <- adjusted_probs / sum(adjusted_probs)
  mosaic_data$satisfaction[i] <- sample(satisfaction_levels, 1, prob = adjusted_probs)
}

# Create mosaic plot
ggplot(data = mosaic_data) +
  geom_mosaic(aes(x = product(education),
                  fill = satisfaction)) +
  scale_fill_brewer(palette = "RdYlGn", direction = 1) +
  labs(title = "Relationship Between\nEducation Level and Job Satisfaction", # Added newline
       subtitle = "Mosaic plot showing the\ndistribution of satisfaction within\neach education level", # Added newline
       x = "Education Level",
       y = "Job Satisfaction",
       fill = "Satisfaction Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")



# --- Bump Chart: Product Rankings Over Time ---

# Create sample data for product rankings over time
rankings <- data.frame(
  product = rep(c("Product A", "Product B",
                  "Product C", "Product D", "Product E"), 4),
  quarter = rep(c("Q1 2024", "Q2 2024",
                  "Q3 2024", "Q4 2024"), each = 5),
  rank = c(1, 2, 3, 4, 5,       # Q1 rankings
           1, 3, 2, 5, 4,       # Q2 rankings
           2, 1, 3, 5, 4,       # Q3 rankings
           3, 1, 2, 4, 5)       # Q4 rankings
)

# Create bump chart
ggplot(rankings, aes(x = quarter, y = rank,
                     group = product, color = product)) +
  geom_line(size = 1.5) +
  geom_point(size = 4) +
  scale_y_reverse(breaks = 1:5) + # Reverse Y-axis so rank 1 is at the top
  theme_minimal() +
  labs(title = "Product Rankings by Quarter",
       subtitle = "Showing changes in ranking \nposition over time", # Added newline
       x = "Quarter",
       y = "Rank (Lower is Better)",
       color = "Product") +
  theme(legend.position = "bottom")

