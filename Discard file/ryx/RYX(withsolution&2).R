# ===============================
# Load packages & data
# ===============================
library(tidyverse)
library(sirt)
library(ggpubr)
library(correlation)

# Read student judgement datasets
students_withsolutions <- read_csv("students-withsolutions.csv")
students_withsolutions2 <- read_csv("students-withsolutions2.csv")

# ===============================
# Fit BTM model for students_withsolutions
# ===============================
sirt_messages <- capture.output(btm_results_studentswithsolution <- sirt::btm(
  data = students_withsolutions %>%
    select(candidate_chosen, candidate_not_chosen) %>%
    mutate(result = 1) %>%
    data.frame(),
  maxiter = 400,
  fix.eta = 0,
  judge = students_withsolutions$judge
))
btm_estimates_studentswithsolution <- btm_results_studentswithsolution$effects %>%
  select(individual, theta_withsol = theta, se_withsol = se.theta)

# ===============================
# Fit BTM model for students_withsolutions2
# ===============================
sirt_messages <- capture.output(btm_results_studentswithsolution2 <- sirt::btm(
  data = students_withsolutions2 %>%
    select(candidate_chosen, candidate_not_chosen) %>%
    mutate(result = 1) %>%
    data.frame(),
  maxiter = 400,
  fix.eta = 0,
  judge = students_withsolutions2$judge
))
btm_estimates_studentswithsolution2 <- btm_results_studentswithsolution2$effects %>%
  select(individual, theta_withsol2 = theta, se_withsol2 = se.theta)

# ===============================
# Merge & Compare
# ===============================
comparison_withsolution <- left_join(btm_estimates_studentswithsolution, btm_estimates_studentswithsolution2, by = "individual")

# ===============================
# Plot Student vs Student Theta Comparison
# ===============================
comparison_withsolution %>%
  ggplot(aes(x = theta_withsol, y = theta_withsol2)) +
  geom_point(size = 2) +
  geom_text(aes(label = individual), hjust = -0.1, vjust = 0, size = 3) +
  geom_errorbar(aes(ymin = theta_withsol2 - se_withsol2, ymax = theta_withsol2 + se_withsol2), width = 0.05) +
  geom_errorbarh(aes(xmin = theta_withsol - se_withsol, xmax = theta_withsol + se_withsol), height = 0.05) +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  ggpubr::stat_cor(method = "pearson") +
  labs(
    title = "Students 20 Comparison vs 40 Comparison (With Solutions)",
    subtitle = "Comparison of BTM Theta estimates from two datasets",
    x = "Student Theta (Dataset 1)",
    y = "Student Theta (Dataset 2)"
  ) +
  theme_minimal(base_size = 14)

# Correlation
correlation::correlation(
  comparison_withsolution %>% select(theta_withsol, theta_withsol2),
  method = "pearson"
)


# Another visualization plot
# Add group label to each set of theta estimates
theta_withsol_long <- btm_estimates_studentswithsolution %>%
  mutate(judge_group = "20 comparisons") %>%
  rename(theta = theta_withsol, se = se_withsol)

theta_withsol2_long <- btm_estimates_studentswithsolution2 %>%
  mutate(judge_group = "40 comparisons") %>%
  rename(theta = theta_withsol2, se = se_withsol2)

# Combine into one long dataset
theta_combined <- bind_rows(theta_withsol_long, theta_withsol2_long)

# Use 20-comparison group to define theta-based ordering of individuals
theta_order <- theta_withsol_long %>%
  arrange(theta) %>%
  pull(individual)

# Plot
theta_combined %>%
  mutate(
    individual = factor(individual, levels = theta_order),
    judge_group = factor(judge_group, levels = c("20 comparisons", "40 comparisons"))
  ) %>%
  ggplot(aes(x = theta, y = individual)) +
  geom_point() +
  facet_grid(cols = vars(judge_group)) +
  labs(
    title = "Perceived Difficulty by Students (20 vs 40 Comparisons)",
    x = "Theta Estimate",
    y = "Candidate"
  ) +
  theme_minimal()





