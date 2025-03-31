# =========================================
# SECTION 8: Compare Students With vs Without Solution
# =========================================

# Load the data
students_withsolutions <- read_csv("students-withsolutions.csv")
students_withoutsolutions <- read_csv("students-withoutsolutions.csv")

# Fit BT model for WITH solution
sirt_messages <- capture.output(btm_students_with <- sirt::btm(
  data = students_withsolutions %>%
    select(candidate_chosen, candidate_not_chosen) %>%
    mutate(result = 1) %>%
    data.frame(),
  maxiter = 400,
  fix.eta = 0,
  judge = students_withsolutions$judge
))
theta_with <- btm_students_with$effects %>%
  select(individual, theta_with = theta, se_with = se.theta)

# Fit BT model for WITHOUT solution
sirt_messages <- capture.output(btm_students_without <- sirt::btm(
  data = students_withoutsolutions %>%
    select(candidate_chosen, candidate_not_chosen) %>%
    mutate(result = 1) %>%
    data.frame(),
  maxiter = 400,
  fix.eta = 0,
  judge = students_withoutsolutions$judge
))
theta_without <- btm_students_without$effects %>%
  select(individual, theta_without = theta, se_without = se.theta)

# Merge results
theta_compare <- left_join(theta_with, theta_without, by = "individual")

# Plot
theta_compare %>%
  ggplot(aes(x = theta_with, y = theta_without)) +
  geom_point() +
  geom_text(aes(label = individual), hjust = -0.1, vjust = 0) +
  geom_errorbar(aes(ymin = theta_without - se_without, ymax = theta_without + se_without), width = 0.05) +
  geom_errorbarh(aes(xmin = theta_with - se_with, xmax = theta_with + se_with), height = 0.05) +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +
  ggpubr::stat_cor() +
  labs(title = "Student Ratings: With vs Without Solution",
       x = "Theta (With Solution)", y = "Theta (Without Solution)")

# Correlation test
cor.test(theta_compare$theta_with, theta_compare$theta_without)




#----plot----
# =========================================
# Compare Students With vs Without Solution
# =========================================

# Load data
students_with <- read_csv("students-withsolutions.csv") %>%
  mutate(judge_group = "with_solution")
students_without <- read_csv("students-withoutsolutions.csv") %>%
  mutate(judge_group = "without_solution")

# Combine two student groups
student_combined <- bind_rows(students_with, students_without)

# === SHR function (same as before) ===
compute_split_half_irr <- function(decisions_data) {
  decisions <- decisions_data %>%
    mutate(winning_column = 1) %>%
    select(candidate_chosen, candidate_not_chosen, winning_column, judge)
  
  judge_group1 <- decisions %>% distinct(judge) %>% slice_sample(prop = 0.5)
  judge_group2 <- decisions %>% distinct(judge) %>% anti_join(judge_group1, by = "judge")
  
  judgements1 <- decisions %>% semi_join(judge_group1, by = "judge")
  judgements2 <- decisions %>% semi_join(judge_group2, by = "judge")
  
  btm1 <- purrr::quietly(sirt::btm)(judgements1 %>% data.frame, maxit = 400, fix.eta = 0)$result
  btm2 <- purrr::quietly(sirt::btm)(judgements2 %>% data.frame, maxit = 400, fix.eta = 0)$result
  
  merged <- merge(btm1$effects, btm2$effects, by = "individual")
  return(cor(merged$theta.x, merged$theta.y, method = "pearson"))
}

# === Compute SHR for both student groups ===
set.seed(10108)
shr_student_solution <- student_combined %>%
  nest(.by = judge_group) %>%
  mutate(
    split_half_irr = purrr::map(data, \(x) replicate(100, compute_split_half_irr(x)))
  ) %>%
  unnest(split_half_irr) %>%
  summarise(SHR = median(split_half_irr), .by = judge_group)

print(shr_student_solution)



# =========================
# Compute SSR
# =========================

# Fit BTM model separately to get sepG values for SSR
btm_students_with <- sirt::btm(
  data = students_with %>%
    select(candidate_chosen, candidate_not_chosen) %>%
    mutate(result = 1) %>%
    data.frame(),
  maxiter = 400,
  fix.eta = 0,
  judge = students_with$judge
)

btm_students_without <- sirt::btm(
  data = students_without %>%
    select(candidate_chosen, candidate_not_chosen) %>%
    mutate(result = 1) %>%
    data.frame(),
  maxiter = 400,
  fix.eta = 0,
  judge = students_without$judge
)

# Compute SSR values
ssr_student_solution <- tibble(
  judge_group = c("with_solution", "without_solution"),
  SSR = c(
    btm_students_with$sepG^2 / (1 + btm_students_with$sepG^2),
    btm_students_without$sepG^2 / (1 + btm_students_without$sepG^2)
  )
)

# =========================
# -----Combine SSR + SHR------
# =========================
reliability_student_solution <- left_join(
  ssr_student_solution,
  shr_student_solution,
  by = "judge_group"
) %>%
  mutate(Dataset = case_when(
    judge_group == "with_solution" ~ "With Solution",
    judge_group == "without_solution" ~ "Without Solution"
  )) %>%
  select(Dataset, SSR, SHR)

# Print final reliability table
print(reliability_student_solution)


# === Fit BTM models for both groups ===
btm_results_students <- student_combined %>%
  nest(.by = judge_group) %>%
  mutate(
    btm_results = purrr::map(data, \(x)
                             purrr::quietly(sirt::btm)(
                               x %>% transmute(candidate_chosen, candidate_not_chosen, col = 1) %>% data.frame,
                               maxit = 400,
                               fix.eta = 0,
                               ignore.ties = TRUE
                             )$result
    )
  ) %>%
  select(-data)

# === Extract theta scores ===
theta_scores_students <- btm_results_students %>%
  mutate(effects = purrr::map(btm_results, \(x) x$effects)) %>%
  unnest(effects) %>%
  select(-btm_results)

# === Visualise comparison ===
theta_scores_students %>%
  filter(judge_group %in% c("with_solution", "without_solution")) %>%
  select(judge_group, id, theta) %>%
  pivot_wider(names_from = judge_group, values_from = theta) %>%
  ggplot(aes(x = with_solution, y = without_solution)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#999999", linewidth = 0.2) +
  geom_point(size = 2) +
  coord_fixed() +
  geom_text(aes(label = id), vjust = 1, hjust = 0, nudge_x = 0.01, nudge_y = -0.01, color = "black") +
  ggpubr::stat_cor() +
  labs(title = "Student Comparison: With vs Without Solution",
       x = "Theta (With Solution)", y = "Theta (Without Solution)")

# === Visualise distributions across items ===
theta_scores_students %>%
  left_join(
    theta_scores_students %>%
      filter(judge_group == "with_solution") %>%
      select(id, with_theta = theta),
    join_by(id)
  ) %>%
  mutate(
    proof = as.factor(id) %>% fct_reorder(with_theta),
    judge_group = fct_relevel(judge_group, "with_solution", "without_solution")
  ) %>%
  ggplot(aes(x = theta, y = proof)) +
  geom_point() +
  facet_grid(cols = vars(judge_group)) +
  labs(title = "Perceived Difficulty by Students (With vs Without Solution)",
       x = "Theta", y = "Proof")
