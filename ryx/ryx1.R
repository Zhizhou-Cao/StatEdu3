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
    proof = as.factor(id) %>% fct_reorder(researcher_theta),
    judge_group = fct_relevel(judge_group, "expert", "student")
  ) %>% 
  ggplot(aes(x = theta, y = proof)) +
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


#-----Actual-----







# ----odd-----

# ---- even----


