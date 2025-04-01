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
    title = "Student vs Student (With Solutions)",
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

# ===============================
# SECTION 1: SSR - students_withsolutions & students_withsolutions2
# ===============================
ssr_compare_table <- tibble(
  Dataset = c("students_withsolutions", "students_withsolutions2"),
  SSR = c(
    btm_results_studentswithsolution$sepG^2 / (1 + btm_results_studentswithsolution$sepG^2),
    btm_results_studentswithsolution2$sepG^2 / (1 + btm_results_studentswithsolution2$sepG^2)
  )
)
print(ssr_compare_table)


# ===============================
# SECTION 2: SHR - students_withsolutions & students_withsolutions2
# ===============================
compute_split_half_irr <- function(decisions_data) {
  decisions <- decisions_data %>%
    mutate(winning_column = 1) %>%
    select(candidate_chosen, candidate_not_chosen, winning_column, judge)
  
  judge_group1 <- decisions %>% distinct(judge) %>% slice_sample(prop = 0.5)
  judge_group2 <- decisions %>% distinct(judge) %>% anti_join(judge_group1, by = "judge")
  
  judgements1 <- decisions %>% semi_join(judge_group1, by = "judge")
  judgements2 <- decisions %>% semi_join(judge_group2, by = "judge")
  
  btm1 <- purrr::quietly(sirt::btm)(judgements1 %>% data.frame(), maxit = 400, fix.eta = 0)$result
  btm2 <- purrr::quietly(sirt::btm)(judgements2 %>% data.frame(), maxit = 400, fix.eta = 0)$result
  
  merged <- merge(btm1$effects, btm2$effects, by = "individual")
  return(cor(merged$theta.x, merged$theta.y, method = "pearson"))
}

set.seed(10108)
shr_compare_table <- tibble(
  Dataset = c("students_withsolutions", "students_withsolutions2"),
  SHR = c(
    median(replicate(100, compute_split_half_irr(students_withsolutions))),
    median(replicate(100, compute_split_half_irr(students_withsolutions2)))
  )
)
print(shr_compare_table)


# ===============================
# SECTION 3: Raw Score vs Theta
# ===============================
compare_raw_vs_theta <- function(btm_result) {
  btm_result$effects %>%
    select(theta, propscore) %>%
    correlation::correlation(method = "pearson", p_adjust = "none")
}

raw_vs_theta_list <- list(
  Student1 = btm_results_studentswithsolution,
  Student2 = btm_results_studentswithsolution2
)

raw_vs_theta_table <- map_df(names(raw_vs_theta_list), function(name) {
  res <- correlation::correlation(
    raw_vs_theta_list[[name]]$effects %>% select(theta, propscore),
    method = "pearson", p_adjust = "none"
  )
  tibble(
    Dataset = name,
    r = round(res$r, 3),
    CI = paste0("[", round(res$CI_low, 3), ", ", round(res$CI_high, 3), "]"),
    p = signif(res$p, 3)
  )
})
print(raw_vs_theta_table)


# ===============================
# SECTION 4: Combine SSR + SHR
# ===============================
reliability_table <- left_join(ssr_compare_table, shr_compare_table, by = "Dataset")
print(reliability_table)




