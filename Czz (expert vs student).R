# ===============================
# SECTION 0: Load packages & data
# ===============================
library(tidyverse)
library(sirt)
library(ggpubr)
library(correlation)

# Load datasets
experts_withsolutions <- read_csv("experts-withsolutions.csv")
students_withsolutions <- read_csv("students-withsolutions.csv")

expert_even <- read_csv("experts-even.csv")
student_even <- read_csv("students-even.csv")

expert_odd <- read_csv("experts-odd.csv")
student_odd <- read_csv("students-odd.csv")

# ===============================
# SECTION 1: With Solutions
# ===============================

# Expert model
sirt_messages <- capture.output(btm_results_expert_all <- sirt::btm(
  data = experts_withsolutions %>%
    select(candidate_chosen, candidate_not_chosen) %>%
    mutate(result = 1) %>%
    data.frame(),
  maxiter = 400,
  fix.eta = 0,
  judge = experts_withsolutions$judge
))
btm_estimates_expert_all <- btm_results_expert_all$effects %>%
  select(individual, expert_theta = theta, expert_se = se.theta)

# Student model
sirt_messages <- capture.output(btm_results_student_all <- sirt::btm(
  data = students_withsolutions %>%
    select(candidate_chosen, candidate_not_chosen) %>%
    mutate(result = 1) %>%
    data.frame(),
  maxiter = 400,
  fix.eta = 0,
  judge = students_withsolutions$judge
))
btm_estimates_student_all <- btm_results_student_all$effects %>%
  select(individual, student_theta = theta, student_se = se.theta)

# Merge & Plot
comparison_all <- left_join(btm_estimates_student_all, btm_estimates_expert_all, by = "individual")

comparison_all %>%
  ggplot(aes(x = expert_theta, y = student_theta)) +
  geom_point() +
  geom_text(aes(label = individual), hjust = -0.1, vjust = 0, size = 3) +
  geom_errorbar(aes(ymin = student_theta - student_se, ymax = student_theta + student_se), width = 0.05) +
  geom_errorbarh(aes(xmin = expert_theta - expert_se, xmax = expert_theta + expert_se), height = 0.05) +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +
  ggpubr::stat_cor() +
  labs(title = "With Solution: Expert vs Student",
       x = "Expert Theta", y = "Student Theta")

# ===============================
# SECTION 2: Even Questions
# ===============================

# Expert
sirt_messages <- capture.output(btm_results_expert_even <- sirt::btm(
  data = expert_even %>%
    select(candidate_chosen, candidate_not_chosen) %>%
    mutate(result = 1) %>%
    data.frame(),
  maxiter = 400,
  fix.eta = 0,
  judge = expert_even$judge
))
btm_estimates_expert_even <- btm_results_expert_even$effects %>%
  select(individual, expert_theta = theta)

# Student
sirt_messages <- capture.output(btm_results_student_even <- sirt::btm(
  data = student_even %>%
    select(candidate_chosen, candidate_not_chosen) %>%
    mutate(result = 1) %>%
    data.frame(),
  maxiter = 400,
  fix.eta = 0,
  judge = student_even$judge
))
btm_estimates_student_even <- btm_results_student_even$effects %>%
  select(individual, student_theta = theta)

# Merge & plot
comparison_even <- left_join(btm_estimates_student_even, btm_estimates_expert_even, by = "individual")

comparison_even %>%
  ggplot(aes(x = expert_theta, y = student_theta)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggpubr::stat_cor() +
  labs(title = "Even Questions: Expert vs Student",
       x = "Expert Theta", y = "Student Theta")

# ===============================
# SECTION 3: Odd Questions
# ===============================

# Expert
sirt_messages <- capture.output(btm_results_expert_odd <- sirt::btm(
  data = expert_odd %>%
    select(candidate_chosen, candidate_not_chosen) %>%
    mutate(result = 1) %>%
    data.frame(),
  maxiter = 400,
  fix.eta = 0,
  judge = expert_odd$judge
))
btm_estimates_expert_odd <- btm_results_expert_odd$effects %>%
  select(individual, expert_theta = theta)

# Student
sirt_messages <- capture.output(btm_results_student_odd <- sirt::btm(
  data = student_odd %>%
    select(candidate_chosen, candidate_not_chosen) %>%
    mutate(result = 1) %>%
    data.frame(),
  maxiter = 400,
  fix.eta = 0,
  judge = student_odd$judge
))
btm_estimates_student_odd <- btm_results_student_odd$effects %>%
  select(individual, student_theta = theta)

# Merge & plot
comparison_odd <- left_join(btm_estimates_student_odd, btm_estimates_expert_odd, by = "individual")

comparison_odd %>%
  ggplot(aes(x = expert_theta, y = student_theta)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  ggpubr::stat_cor() +
  labs(title = "Odd Questions: Expert vs Student",
       x = "Expert Theta", y = "Student Theta")

# ===============================
# SECTION 4: Correlation Summary
# ===============================

cat("\n=== Correlation Summary ===\n")
list(
  WithSolution = correlation::correlation(comparison_all %>% select(expert_theta, student_theta), method = "pearson"),
  Even = correlation::correlation(comparison_even %>% select(expert_theta, student_theta), method = "pearson"),
  Odd = correlation::correlation(comparison_odd %>% select(expert_theta, student_theta), method = "pearson")
)

# ===============================
# SECTION 5: SSR Reliability
# ===============================
ssr_table <- tibble(
  Dataset = c("WithSolution", "Even", "Odd"),
  Expert_SSR = c(
    btm_results_expert_all$sepG^2 / (1 + btm_results_expert_all$sepG^2),
    btm_results_expert_even$sepG^2 / (1 + btm_results_expert_even$sepG^2),
    btm_results_expert_odd$sepG^2 / (1 + btm_results_expert_odd$sepG^2)
  ),
  Student_SSR = c(
    btm_results_student_all$sepG^2 / (1 + btm_results_student_all$sepG^2),
    btm_results_student_even$sepG^2 / (1 + btm_results_student_even$sepG^2),
    btm_results_student_odd$sepG^2 / (1 + btm_results_student_odd$sepG^2)
  )
)
print(ssr_table)

# ===============================
# SECTION 6: SHR Split-Half Reliability
# ===============================
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

# 定义每组数据集
sh_data_list <- list(
  WithSolution_Expert = experts_withsolutions,
  WithSolution_Student = students_withsolutions,
  Even_Expert = expert_even,
  Even_Student = student_even,
  Odd_Expert = expert_odd,
  Odd_Student = student_odd
)

# 多次 split-half 重复估计
set.seed(10108)
shr_results <- map_df(names(sh_data_list), function(name) {
  data <- sh_data_list[[name]]
  shr_vals <- replicate(100, compute_split_half_irr(data))
  tibble(Group = name, SHR = median(shr_vals))
})

print(shr_results)

# ===============================
# SECTION 7: Compare Raw Score vs BT Theta
# ===============================

# 用 propscore 代替 raw score，用 correlation 比较是否需要“fancy stats”
compare_raw_vs_theta <- function(btm_result) {
  btm_result$effects %>%
    select(theta, propscore) %>%
    correlation::correlation(method = "pearson", p_adjust = "none")
}

cat("\n--- Raw vs Theta Correlation ---\n")

list(
  Expert_WithSolution = compare_raw_vs_theta(btm_results_expert_all),
  Student_WithSolution = compare_raw_vs_theta(btm_results_student_all),
  Expert_Even = compare_raw_vs_theta(btm_results_expert_even),
  Student_Even = compare_raw_vs_theta(btm_results_student_even),
  Expert_Odd = compare_raw_vs_theta(btm_results_expert_odd),
  Student_Odd = compare_raw_vs_theta(btm_results_student_odd)
)

# ================
# Combine SSR + SHR
# ================
# 将 SHR 表分成 Group 和 Dataset 两列
shr_results_split <- shr_results %>%
  separate(Group, into = c("Dataset", "Group"), sep = "_")

# 同样，把 SSR 表整理为长格式
ssr_long <- ssr_table %>%
  pivot_longer(cols = c(Expert_SSR, Student_SSR),
               names_to = "Group",
               names_pattern = "(.*)_SSR",
               values_to = "SSR")

# 合并两个表
reliability_table <- left_join(ssr_long, shr_results_split, by = c("Dataset", "Group")) %>%
  select(Dataset, Group, SSR, SHR)

# 输出最终的 reliability 表
print(reliability_table)


#'To evaluate the consistency of the judgements, we computed both Single Sample Reliability (SSR)
#'and Split-Half Reliability (SHR) for each dataset and group. The SSR values were all above 0.8,
#'suggesting a generally acceptable level of internal consistency. Notably, the highest SSR was 
#'observed for the Even Questions – Student group (SSR = 0.896), indicating particularly strong
#' agreement among student raters in that subset.

However, the SHR values were notably lower than the SSRs, which is expected due to the stricter split-half criterion. The WithSolution – Student group had the lowest SHR (0.597), potentially suggesting greater variability between subgroups of student judges. In contrast, Even – Student group reached an SHR of 0.730, indicating reasonably stable results across judge subsets.

Overall, expert judgements showed moderately higher SSR than students on WithSolution and Odd datasets, whereas students outperformed experts in the Even dataset.



# ============================================
# Combine Raw Score vs Theta Correlations
# ============================================

raw_vs_theta_list <- list(
  WithSolution_Expert = btm_results_expert_all,
  WithSolution_Student = btm_results_student_all,
  Even_Expert = btm_results_expert_even,
  Even_Student = btm_results_student_even,
  Odd_Expert = btm_results_expert_odd,
  Odd_Student = btm_results_student_odd
)

raw_vs_theta_table <- map_df(names(raw_vs_theta_list), function(name) {
  parts <- str_split(name, "_")[[1]]
  dataset <- parts[1]
  group <- parts[2]
  
  cor_result <- correlation::correlation(
    raw_vs_theta_list[[name]]$effects %>% select(theta, propscore),
    method = "pearson", p_adjust = "none"
  )
  
  tibble(
    Dataset = dataset,
    Group = group,
    r = round(cor_result$r, 3),
    CI = paste0("[", round(cor_result$CI_low, 3), ", ", round(cor_result$CI_high, 3), "]"),
    p = signif(cor_result$p, 3)
  )
})

# 输出最终的 raw vs theta 表
print(raw_vs_theta_table)
#'
#'To evaluate whether the use of the Bradley-Terry model adds substantial value over simple win 
#'proportions (raw scores), we computed the Pearson correlation between theta estimates and raw 
#'scores (propscore) for each group and dataset.

#'The results (Table X) indicate extremely high correlations in all cases (r > 0.97), with p-values 
#'far below conventional significance thresholds. For instance, in the WithSolution – Student group,
#' the correlation reached r = 0.996 with a 95% CI of [0.990, 0.999]. Even in smaller subsets such 
#' as Odd – Expert, the correlation remained as high as r = 0.976.

#'These findings suggest that while the Bradley-Terry model provides a more principled probabilistic
#' framework, in this context the simpler raw scores yield rankings nearly identical to the more
#'  complex estimates. This echoes findings in the literature (e.g., Lecture 9), supporting the
#'   robustness of comparative judgement even when analysed without “fancy statistics”.

