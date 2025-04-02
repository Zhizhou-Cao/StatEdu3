library(tidyverse)
library(readr)

#' Questions:
#'1. odd : expert vs student
#'2. even: expert vs student
#'3. with solution : expert vs student
#'4. 2020 actual vs expert 
#'5. 2020 actual vs student

# ----Import dataset----
Actual_grade <- read_csv("ANON_2020-21diagtestSep.csv")

expert_even <- read_csv("experts-even.csv")
expert_odd <- read_csv("experts-odd.csv")
experts_withsolutions <- read_csv("experts-withsolutions.csv")

students_even <- read_csv("students-even.csv")
students_odd <- read_csv("students-odd.csv")

students_withoutsolutions <- read_csv("students-withoutsolutions.csv")

students_withsolutions <- read_csv("students-withsolutions.csv") # 20 judges, 20 comparisons
students_withsolutions1 <- read_csv("students-withsolutions1.csv") # 10 judges, 20 comparisons
students_withsolutions2 <- read_csv("students-withsolutions2.csv") # 10 judges, 40 comparisons


# ----Comparison Score Transform----
# 把comparison转换成分数
# From pairwise comparisons to scores
#' To generate scores for the items that were compared, we can use the Bradley-Terry model
#' which assumes that the items have scores on a unidimensional scale.
library(sirt)
# -----3. with solution : expert vs student------
# Expert
sirt_messages <- capture.output(btm_results_experts <- sirt::btm(
  data = experts_withsolutions %>% select(candidate_chosen, candidate_not_chosen) %>%
    mutate(result = 1) %>% data.frame,
  maxiter = 400,
  fix.eta = 0, # the "home advantage" for the left column
  judge = experts_withsolutions %>%pull(judge)
))
btm_estimates_experts <- btm_results_experts$effects %>% 
  select(individual, expert_theta = theta,  expert_se = se.theta)
btm_estimates_experts

# Students
sirt_messages <- capture.output(btm_results_students <- sirt::btm(
  data = students_withsolutions %>% select(candidate_chosen, candidate_not_chosen) %>%
    mutate(result = 1) %>% data.frame,
  maxiter = 400,
  fix.eta = 0, # the "home advantage" for the left column
  judge = students_withsolutions %>%pull(judge)
))
btm_estimates_students <- btm_results_students$effects %>% 
  select(individual, student_theta = theta, student_se = se.theta)

# join the two sets of scores together
btm_estimates <- btm_estimates_students %>% 
  left_join(btm_estimates_experts, join_by(individual))
btm_estimates

# make a version of Figure 1
btm_estimates %>% 
  ggplot(aes(x = expert_theta, y = student_theta)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#999999", linewidth = 0.2) +
  geom_point(size = 2) +
  geom_text(aes(label = individual), vjust = 1, hjust = 0, nudge_x = 0.02, nudge_y = -0.02, color = "black") +
  geom_errorbar(aes(xmin = expert_theta - expert_se, xmax = expert_theta + expert_se), linewidth = 0.1, width = 0.05) +
  geom_errorbar(aes(ymin = student_theta - student_se, ymax = student_theta + student_se), linewidth = 0.1, width = 0.05) +
  coord_fixed() +
  ggpubr::stat_cor() +
  labs(x = "Perceived Quality (Experts)", y = "Perceived Quality (Students)")


# ---SSR----
ssr_values <- tibble(
  "Experts" = btm_results_experts$sepG^2 / (1 + btm_results_experts$sepG^2),
  "Students" = btm_results_students$sepG^2 / (1 + btm_results_students$sepG^2)
)
ssr_values

# ----- SHR-----
# 给 expert_data 添加 judge_group = "expert"
experts_withsolutions<- experts_withsolutions %>%
  mutate(judge_group = "expert")

# 给 student_data 添加 judge_group = "student"
students_withsolutions <- students_withsolutions %>%
  mutate(judge_group = "student")

# 合并两者
withsolution_combined <- bind_rows(experts_withsolutions, students_withsolutions)
# copied from the lecture notes:
compute_split_half_irr <- function(decisions_data) {
  
  # Prepare to use sirt::btm - the third column indicates
  # which of the first two was the winner, with 1 = leftmost
  decisions <- decisions_data %>% 
    mutate(winning_column = 1) %>% 
    select(candidate_chosen, candidate_not_chosen, winning_column, judge)
  
  # Group 1: sample half of the judges at random
  judge_group1 <- decisions %>%
    select(judge) %>%
    distinct() %>%
    slice_sample(prop = 0.5)
  # Group 2: the remaining judges
  judge_group2 <- decisions %>%
    select(judge) %>%
    distinct() %>%
    anti_join(judge_group1, by = c("judge"))
  
  # Separate the judgements from each group
  judgements1 <- decisions %>% semi_join(judge_group1, by = "judge")
  judgements2 <- decisions %>% semi_join(judge_group2, by = "judge")
  
  # Fit the Bradley-Terry model for each group separately
  # (using purrr::quietly to suppress output from sirt::btm)
  btm1 <- purrr::quietly(sirt::btm)(
    judgements1 %>% data.frame,
    maxit = 400,
    fix.eta = 0,
    ignore.ties = TRUE
  )$result
  btm2 <- purrr::quietly(sirt::btm)(
    judgements2 %>% data.frame,
    maxit = 400,
    fix.eta = 0,
    ignore.ties = TRUE
  )$result
  
  # Combine the estimates from each group in a single table
  merged_effects <- merge(btm1$effects, btm2$effects, by = "individual")
  
  # Report the correlation between the two groups
  return(cor(merged_effects$theta.x,
             merged_effects$theta.y,
             method = "pearson"))
}

# now use the function to compute SHR:
set.seed(10108)
split_halves <- withsolution_combined %>% 
  nest(.by = judge_group) %>% 
  mutate(
    split_half_irr = purrr::map(
      data,
      \(x) replicate(n = 100, compute_split_half_irr(x))
    )
  ) %>% 
  unnest(split_half_irr)

split_halves %>% 
  summarise(SHR = median(split_half_irr), .by = judge_group)




rigour_btm_results <- withsolution_combined %>% 
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

rigour_btm_results

rigour_scores <- rigour_btm_results %>% 
  mutate(effects = purrr::map(btm_results, \(x) x$effects)) %>% 
  unnest(effects) %>% 
  select(-btm_results)


rigour_scores %>% 
  filter(judge_group %in% c("student", "expert")) %>% 
  select(judge_group, id, theta) %>% 
  pivot_wider(names_from = judge_group, values_from = theta) %>% 
  ggplot(aes(x = expert, y = student)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#999999", linewidth = 0.2) +
  geom_point(size = 2) +
  coord_fixed() +
  geom_text(aes(label = id), vjust = 1, hjust = 0, nudge_x = 0.01, nudge_y = -0.01, color = "black") +
  ggpubr::stat_cor()

# Visualising the results for all judge groups
rigour_scores %>% 
  left_join(
    rigour_scores %>% filter(judge_group == "expert") %>% select(id, researcher_theta = theta),
    join_by(id)
  ) %>% 
  mutate(
    Question_id = as.factor(id) %>% fct_reorder(researcher_theta),
    judge_group = fct_relevel(judge_group, "expert", "student")
  ) %>% 
  ggplot(aes(x = theta, y = Question_id)) +
  geom_point() +
  facet_grid(cols = vars(judge_group))



# ---- Compute the correlation----

rigour_scores %>% 
  select(judge_group, id, theta) %>% 
  filter(judge_group %in% c("student", "expert")) %>% 
  pivot_wider(names_from = judge_group, values_from = theta) %>% 
  select(-id) %>% 
  correlation::correlation(p_adjust = "none")

cor.test(
  rigour_scores %>% arrange(individual) %>% filter(judge_group == "student") %>% pull(theta),
  rigour_scores %>% arrange(individual) %>% filter(judge_group == "expert") %>% pull(theta)
)


# Raw Score
rigour_scores %>% 
  select(judge_group, id, propscore) %>% 
  pivot_wider(names_from = judge_group, values_from = propscore) %>% 
  select(-id) %>% 
  correlation::correlation(p_adjust = "none")



# In particular, the y4-researcher comparison has very similar results in each case:
rigour_scores %>% 
  filter(judge_group %in% c("expert", "student")) %>% 
  select(judge_group, id, propscore, theta) %>% 
  pivot_longer(cols = c(propscore, theta), names_to = "method") %>% 
  mutate(method = case_match(method, "theta" ~ "Bradley-Terry", "propscore" ~ "Raw score")) %>% 
  pivot_wider(names_from = judge_group, values_from = value) %>% 
  ggplot(aes(x = expert, y = student)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#999999", linewidth = 0.2) +
  geom_point(size = 2) +
  geom_text(aes(label = id), vjust = 1, hjust = 0, nudge_x = 0.01, nudge_y = -0.01, color = "black") +
  ggpubr::stat_cor() +
  facet_wrap(~ method, scales = "free")

# Compare correlation of raw scores vs theta scores
cor_test_raw_vs_theta <- cor.test(
  rigour_scores %>% filter(judge_group == "expert") %>% pull(propscore),
  rigour_scores %>% filter(judge_group == "expert") %>% pull(theta)
)
rigour_scores %>%
  filter(judge_group %in% c("expert", "student")) %>%
  select(judge_group, id, theta) %>%
  pivot_wider(names_from = judge_group, values_from = theta) %>%
  cor(method = "spearman")

btm_estimates %>%
  mutate(residual = student_theta - predict(lm(student_theta ~ expert_theta))) %>%
  arrange(desc(residual))  # top n student-overrated

# ----odd-----

# ---- even----

#-----Actual-----
# 读取数据
Actual_grade <- read_csv("ANON_2020-21diagtestSep.csv", na = c("", "NA", "-"))

# 只选择题目列
grade_questions <- Actual_grade %>% select(6:25)

# 强制转换为 numeric（排除 "-"）
grade_questions <- grade_questions %>%
  mutate(across(everything(), ~ as.numeric(.)))

# 计算总分、作答人数、平均分
average_scores <- grade_questions %>%
  summarise(across(
    everything(),
    list(
      total = ~ sum(., na.rm = TRUE),
      count = ~ sum(!is.na(.)),
      average = ~ mean(., na.rm = TRUE),
      sd = ~ sd(., na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(cols = everything(),
               names_to = c("Question", ".value"),
               names_sep = "_")
# 增加标准误差列
average_scores <- average_scores %>%
  mutate(Question = str_extract(Question, "\\d+")) %>%
  mutate(sem = sd / sqrt(count))

# 查看结果
print(average_scores)



# Step 1: 合并 expert 和 student 的 CJ 评分
cj_scores <- left_join(
  btm_estimates_student_all %>% select(individual, student_theta),
  btm_estimates_expert_all %>% select(individual, expert_theta),
  by = "individual"
) %>%
  mutate(Question = paste0(as.character(individual)))

# Step 2: 合并 CJ 评分和实际平均分
cj_vs_actual <- left_join(cj_scores, average_scores, by = "Question")

# Step 3: 查看合并结果
print(cj_vs_actual)

# Step 4: 可视化 Actual Grade vs Expert/Student Theta
library(ggpubr)

cj_vs_actual %>%
  pivot_longer(cols = c(student_theta, expert_theta), names_to = "Group", values_to = "theta") %>%
  ggplot(aes(x = average, y = theta, color = Group)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  ggpubr::stat_cor(method = "pearson") +
  labs(title = "Actual Average Grade vs Perceived Quality",
       x = "Actual Average Grade",
       y = "Perceived Quality (Theta)")


# 合并平均得分与 expert theta + 标准误
expert_vs_actual <- btm_estimates_experts %>%
  mutate(Question = paste0(individual)) %>%
  left_join(average_scores %>% mutate(Question = str_extract(Question, "\\d+")), by = "Question")

student_vs_actual <- btm_estimates_students %>%
  mutate(Question = paste0(individual)) %>%
  left_join(average_scores %>% mutate(Question = str_extract(Question, "\\d+")), by = "Question")



# Expert 图对象
plot_expert <- expert_vs_actual %>%
  ggplot(aes(x = average, y = expert_theta)) +
  geom_point(size = 2) +
  geom_text(aes(label = Question), hjust = -0.1, vjust = 0, size = 3) +
  geom_errorbar(aes(ymin = expert_theta - expert_se, ymax = expert_theta + expert_se), width = 0.05) +
  geom_errorbarh(aes(xmin = average - sem, xmax = average + sem), height = 0.05) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggpubr::stat_cor(method = "pearson") +
  labs(title = "Expert Theta vs Actual Grade",
       x = "Actual Average Grade",
       y = "Expert Perceived Quality (θ)")

# Student 图对象
plot_student <- student_vs_actual %>%
  ggplot(aes(x = average, y = student_theta)) +
  geom_point(size = 2) +
  geom_text(aes(label = Question), hjust = -0.1, vjust = 0, size = 3) +
  geom_errorbar(aes(ymin = student_theta - student_se, ymax = student_theta + student_se), width = 0.05) +
  geom_errorbarh(aes(xmin = average - sem, xmax = average + sem), height = 0.05) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  ggpubr::stat_cor(method = "pearson") +
  labs(title = "Student Theta vs Actual Grade",
       x = "Actual Average Grade",
       y = "Student Perceived Quality (θ)")

plot_expert/plot_student 


#----plot----
# 准备 expert
expert_plot_data <- btm_estimates_expert_all %>%
  select(id = individual, theta = expert_theta) %>%
  mutate(group = "Expert")

# 准备 student
student_plot_data <- btm_estimates_student_all %>%
  select(id = individual, theta = student_theta) %>%
  mutate(group = "Student")

# 准备 actual grade（使用 average_scores 表）
actual_plot_data <- average_scores %>%
  mutate(
    id = as.numeric(Question),
    theta = -average,
    group = "Actual Grade"
  ) %>%
  select(id, theta, group)

# 合并为一个大表
theta_all <- bind_rows(expert_plot_data, student_plot_data, actual_plot_data)



theta_all %>%
  mutate(
    id = as.factor(id),
    group = fct_relevel(group, "Actual Grade", "Expert", "Student"),
    proof = fct_reorder(id, theta)
  ) %>%
  ggplot(aes(x = theta, y = proof, color = group)) +
  geom_point(size = 2) +
  labs(
    title = "Comparison of Perceived Difficulty and Actual Grade",
    x = "Score (Theta or Average Grade)",
    y = "Proof",
    color = "Group"
  )


# Expert
plot_expert <- btm_estimates_expert_all %>%
  mutate(
    proof = as.factor(individual),
    rank_order = expert_theta
  ) %>%
  mutate(proof = fct_reorder(proof, rank_order)) %>%
  ggplot(aes(x = expert_theta, y = proof)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "Expert Perceived Quality",
    x = "Expert Theta", y = "Proof"
  )

# Student
plot_student <- btm_estimates_student_all %>%
  mutate(
    proof = as.factor(individual),
    rank_order = student_theta
  ) %>%
  mutate(proof = fct_reorder(proof, rank_order)) %>%
  ggplot(aes(x = student_theta, y = proof)) +
  geom_point(color = "darkgreen", size = 2) +
  labs(
    title = "Student Perceived Quality",
    x = "Student Theta", y = "Proof"
  )

# Actual Grade
plot_actual <- average_scores %>%
  mutate(
    proof = as.factor(Question),
    rank_order = -average
  ) %>%
  mutate(proof = fct_reorder(proof, rank_order)) %>%
  ggplot(aes(x = average, y = proof)) +
  geom_point(color = "black", size = 2) +
  labs(
    title = "Actual Average Grade",
    x = "Average Grade", y = "Proof"
  )

plot_expert + plot_student + plot_actual

#-----plot----

# 提取 student theta 排序顺序
student_order <- btm_estimates_students %>%
  mutate(proof = as.factor(individual)) %>%
  select(proof, proof_order = student_theta)

plot_expert <- btm_estimates_expert_all %>%
  mutate(proof = as.factor(individual)) %>%
  left_join(student_order, by = "proof") %>%
  mutate(proof = fct_reorder(proof, proof_order)) %>%
  ggplot(aes(x = expert_theta, y = proof)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "Expert Perceived Quality",
    x = "Expert Theta", y = "Proof (Ranked by Student Theta)"
  )
plot_student <- btm_estimates_student_all %>%
  mutate(proof = as.factor(individual)) %>%
  left_join(student_order, by = "proof") %>%
  mutate(proof = fct_reorder(proof, proof_order)) %>%
  ggplot(aes(x = student_theta, y = proof)) +
  geom_point(color = "darkgreen", size = 2) +
  labs(
    title = "Student Perceived Quality",
    x = "Student Theta", y = "Proof (Ranked by Student Theta)"
  )
plot_actual <- average_scores %>%
  mutate(proof = as.factor(Question)) %>%
  left_join(student_order, by = "proof") %>%
  mutate(proof = fct_reorder(proof, proof_order)) %>%
  ggplot(aes(x = -average, y = proof)) +
  geom_point(color = "black", size = 2) +
  labs(
    title = "Actual Average Grade",
    x = "Average Grade", y = "Proof (Ranked by Student Theta)"
  )
plot_expert + plot_student + plot_actual


student_vs_actual$student_theta
btm_estimates_student_all$student_theta
student_vs_actual$average
average_scores$average
# 准备数据
regression_data <- tibble(
  student_theta = btm_estimates_student_all$student_theta,
  average_score = average_scores$average,
  expert_theta =  btm_estimates_expert_all$expert_theta
)

# 执行线性回归
model <- lm(student_theta ~average , data = student_vs_actual)

# 查看回归结果
summary(model)

library(ggplot2)

ggplot(student_vs_actual, aes(x =student_theta , y = average)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Linear Regression: Average Score ~ Theta",
    x = "BTM Theta (student)",
    y = "Average Score"
  )

# ---SSR----
ssr_values <- tibble(
  judge_group = c("expert", "student"),
  SSR = c(
    btm_results_expert_all$sepG^2 / (1 + btm_results_expert_all$sepG^2),
    btm_results_student_all$sepG^2 / (1 + btm_results_student_all$sepG^2)
  )
)

# ----- SHR-----
# 给 expert_data 添加 judge_group = "expert"
experts_withsolutions<- experts_withsolutions %>%
  mutate(judge_group = "expert")

# 给 student_data 添加 judge_group = "student"
students_withsolutions <- students_withsolutions %>%
  mutate(judge_group = "student")

# 合并两者
withsolution_combined <- bind_rows(experts_withsolutions, students_withsolutions)
# copied from the lecture notes:
compute_split_half_irr <- function(decisions_data) {
  
  # Prepare to use sirt::btm - the third column indicates
  # which of the first two was the winner, with 1 = leftmost
  decisions <- decisions_data %>% 
    mutate(winning_column = 1) %>% 
    select(candidate_chosen, candidate_not_chosen, winning_column, judge)
  
  # Group 1: sample half of the judges at random
  judge_group1 <- decisions %>%
    select(judge) %>%
    distinct() %>%
    slice_sample(prop = 0.5)
  # Group 2: the remaining judges
  judge_group2 <- decisions %>%
    select(judge) %>%
    distinct() %>%
    anti_join(judge_group1, by = c("judge"))
  
  # Separate the judgements from each group
  judgements1 <- decisions %>% semi_join(judge_group1, by = "judge")
  judgements2 <- decisions %>% semi_join(judge_group2, by = "judge")
  
  # Fit the Bradley-Terry model for each group separately
  # (using purrr::quietly to suppress output from sirt::btm)
  btm1 <- purrr::quietly(sirt::btm)(
    judgements1 %>% data.frame,
    maxit = 400,
    fix.eta = 0,
    ignore.ties = TRUE
  )$result
  btm2 <- purrr::quietly(sirt::btm)(
    judgements2 %>% data.frame,
    maxit = 400,
    fix.eta = 0,
    ignore.ties = TRUE
  )$result
  
  # Combine the estimates from each group in a single table
  merged_effects <- merge(btm1$effects, btm2$effects, by = "individual")
  
  # Report the correlation between the two groups
  return(cor(merged_effects$theta.x,
             merged_effects$theta.y,
             method = "pearson"))
}

# now use the function to compute SHR:
set.seed(10108)
split_halves <- withsolution_combined %>% 
  nest(.by = judge_group) %>% 
  mutate(
    split_half_irr = purrr::map(
      data,
      \(x) replicate(n = 100, compute_split_half_irr(x))
    )
  ) %>% 
  unnest(split_half_irr)
shr_values <- split_halves %>% 
  summarise(SHR = median(split_half_irr), .by = judge_group)

# --- 合并 SSR 和 SHR 到同一张表 ---
summary_table <- left_join(ssr_values, shr_values, by = "judge_group")
kable(summary_table)


