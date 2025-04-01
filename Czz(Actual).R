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
