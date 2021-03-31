# Title     : AB Testing in R
# Objective : TODO
# Created by: Jjrex8988
# Created on: 30/3/2021

## Chapter 1: Mini case study in A/B Testing
## Introduction

## Goals of A/B testing
## (Q) The "hypothesis" for an A/B testing experiment refers to?
## (A) What you think will happen as a result of the experiment.


## Preliminary data exploration
# Load tidyverse
library(tidyverse)

# Read in data
click_data <- read_csv("click_data.csv")
click_data

# Find oldest and most recent date
min(click_data$visit_date)
max(click_data$visit_date)





## Baseline conversion rates
library(lubridate)

click_data_sum_month <-  click_data %>%
    group_by(month(visit_date)) %>%
    summarize(conversion_rate = mean(clicked_adopt_today))

ggplot(click_data_sum_month, aes(x = `month(visit_date)`, y = conversion_rate)) +
  geom_point() +
  geom_line()


##  Current conversion rate day of week
# Read in the data
click_data <- read_csv("click_data.csv")

# Calculate the mean conversion rate by day of the week
click_data %>%
  group_by(wday(visit_date)) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))


## Current conversion rate week
click_data <- read_csv("click_data.csv")

click_data %>%
  group_by(week(visit_date)) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))


## Plotting conversion rate seasonality
library(scales)

# Compute conversion rate by week of the year
click_data_sum_week <- click_data %>%
  group_by(week(visit_date)) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

# Build plot
ggplot(click_data_sum_week, aes(x = `week(visit_date)`,
                           y = conversion_rate)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1),
                     labels = percent)


## Experimental design, power analysis
## Randomized vs. sequential

## (Q) You're designing a new experiment and you have two
## conditions. What's the best method for comparing your two
## conditions?
## (A) Run control and test conditions simultaneously for two months.


## SSizeLogisticBin() documentation
library(powerMediation)

## (Q) What is another way to phrase what p2 signifies?
## (A) The probability when X = 1 (the test condition).


## Power analysis August

click_data_month2 <- click_data %>%
  group_by(month(visit_date)) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

total_sample_size_month <- SSizeLogisticBin(p1 = 0.54,
                                     p2 = 0.64,
                                     0.5,
                                     0.05,
                                     0.8
)

total_sample_size_month


## Power analysis August 5 percentage point increase
# Load powerMediation
library(powerMediation)

# Compute and look at sample size for experiment in August with a 5 percentage point increase
total_sample_size <- SSizeLogisticBin(p1 = 0.54,
                                      p2 = 0.59,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size




## Analyzing results
library(tidyverse)

experiment_data <- read_csv("experiment_data.csv")
experiment_data

experiment_data_sum <- experiment_data %>%
    group_by(condition, visit_date) %>%
    summarize(conversion_rate = mean(clicked_adopt_today))

ggplot(experiment_data_sum, aes(x = visit_date, y=conversion_rate,
                                color = condition,
                                group = condition
)) +
  geom_point() + geom_line()
## Wow! If we only expect a 5 percentage point
## conversion rate increase we need 3085 (about 1543 per group)
## to see a significant effect, much more than if we predicted a
## 10 percentage point increase.





## Chapter 2: Mini case study in A/B Testing Part 2
## Analyzing results
library(broom)


glm(clicked_adopt_today ~ condition,
    family = 'binomial',
    data = experiment_data) %>%
  tidy()


## Plotting results
experiment_data_clean <- read_csv("experiment_data.csv")


# Group and summarize data
experiment_data_clean_sum <- experiment_data_clean %>%
  group_by(condition, visit_date) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

# Make plot of conversion rates over time
ggplot(experiment_data_clean_sum,
       aes(x = visit_date,
           y = conversion_rate,
           color = condition,
           group = condition)) +
  geom_point() +
  geom_line()


experiment_data_clean %>%
  group_by(condition) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))


glm(clicked_adopt_today ~ condition,
    family = 'binomial',
    data = experiment_data_clean) %>%  tidy()


## (Q) what exactly the family argument is.
## (A) A description of the error distribution
## and link function to be used in the model.


## Practice with glm()
library(broom)

experiment_data_clean %>%
  group_by(condition) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

experiment_result <- glm(clicked_adopt_today ~ condition,
                         family = 'binomial',
                         data = experiment_data_clean
                         ) %>% tidy()
experiment_result



## Designing follow-up experiments
## Follow-up experiment 1 design
## (Q) You're now designing your follow-up experiments. What's the best path forward?
## (A) Build one experiment where your test condition is a kitten in a hat.
## If the experiment works, run a second experiment with a kitten in
## a hat as the control and two kittens in hats as the test


## Follow-up experiment 1 power analysis
# Load package for running power analysis
library(powerMediation)

# Run logistic regression power analysis
total_sample_size2 <- SSizeLogisticBin(p1 = 0.39,
                                      p2 = 0.59,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size2


## Follow-up experiment 1 analysis

# followup <- structure(list(visit_date = structure(c(17744, 17744, 17744,
# 17744, 17745, 17745, 17745, 17745, 17745, 17745, 17746, 17746,
# 17746, 17746, 17746, 17746, 17747, 17747, 17747, 17747, 17747,
# 17747, 17747, 17747, 17748, 17748, 17748, 17748, 17748, 17748,
# 17749, 17749, 17749, 17749, 17750, 17750, 17750, 17750, 17750,
# 17750, 17750, 17750, 17751, 17752, 17752, 17752, 17752, 17752,
# 17752, 17752, 17752, 17752, 17752, 17753, 17753, 17753, 17753,
# 17753, 17753, 17754, 17754, 17754, 17754, 17754, 17754, 17754,
# 17754, 17754, 17755, 17755, 17755, 17755, 17755, 17755, 17755,
# 17755, 17755, 17755, 17756, 17756, 17756, 17756, 17756, 17757,
# 17757, 17757, 17757, 17757, 17758, 17758, 17758, 17758, 17758,
# 17758, 17758, 17758, 17758, 17759, 17759, 17759, 17759, 17759,
# 17759, 17760, 17760, 17760, 17760, 17760, 17760, 17760, 17760,
# 17760, 17760, 17760, 17760, 17760, 17761, 17761, 17761, 17762,
# 17762, 17762, 17762, 17762, 17763, 17763, 17763, 17763, 17763,
# 17763, 17763, 17763, 17763, 17764, 17764, 17764, 17764, 17765,
# 17765, 17765, 17765, 17765, 17765, 17765, 17765, 17766, 17766,
# 17766, 17766, 17766, 17767, 17767, 17767, 17767, 17767, 17768,
# 17768, 17768, 17768, 17768, 17769, 17769, 17769, 17769, 17769,
# 17770, 17770, 17770, 17770, 17770, 17770, 17770, 17771, 17771,
# 17771, 17771, 17772, 17772, 17772, 17772, 17772, 17772, 17772,
# 17773, 17773, 17773, 17773, 17773, 17773, 17774, 17774, 17774,
# 17774, 17774), class = "Date"), condition = c("kitten_hat", "kitten_hat",
# "kitten_hat", "kitten_hat", "cat_hat", "cat_hat", "kitten_hat",
# "kitten_hat", "kitten_hat", "kitten_hat", "cat_hat", "kitten_hat",
# "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "cat_hat",
# "cat_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
# "kitten_hat", "kitten_hat", "cat_hat", "cat_hat", "cat_hat",
# "cat_hat", "kitten_hat", "kitten_hat", "cat_hat", "cat_hat",
# "cat_hat", "kitten_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
# "cat_hat", "kitten_hat", "kitten_hat", "kitten_hat", "cat_hat",
# "cat_hat", "cat_hat", "cat_hat", "cat_hat", "kitten_hat", "kitten_hat",
# "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "cat_hat",
# "cat_hat", "cat_hat", "kitten_hat", "kitten_hat", "kitten_hat",
# "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "kitten_hat",
# "kitten_hat", "kitten_hat", "kitten_hat", "cat_hat", "cat_hat",
# "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
# "kitten_hat", "kitten_hat", "cat_hat", "kitten_hat", "kitten_hat",
# "kitten_hat", "kitten_hat", "cat_hat", "cat_hat", "cat_hat",
# "kitten_hat", "kitten_hat", "cat_hat", "cat_hat", "cat_hat",
# "cat_hat", "cat_hat", "kitten_hat", "kitten_hat", "kitten_hat",
# "kitten_hat", "cat_hat", "cat_hat", "kitten_hat", "kitten_hat",
# "kitten_hat", "kitten_hat", "cat_hat", "cat_hat", "cat_hat",
# "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
# "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
# "kitten_hat", "kitten_hat", "cat_hat", "kitten_hat", "kitten_hat",
# "kitten_hat", "kitten_hat", "cat_hat", "cat_hat", "cat_hat",
# "cat_hat", "cat_hat", "kitten_hat", "kitten_hat", "kitten_hat",
# "kitten_hat", "cat_hat", "cat_hat", "cat_hat", "kitten_hat",
# "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
# "kitten_hat", "kitten_hat", "cat_hat", "cat_hat", "kitten_hat",
# "kitten_hat", "kitten_hat", "cat_hat", "cat_hat", "cat_hat",
# "kitten_hat", "kitten_hat", "cat_hat", "cat_hat", "cat_hat",
# "cat_hat", "kitten_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
# "cat_hat", "cat_hat", "cat_hat", "cat_hat", "kitten_hat", "kitten_hat",
# "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
# "kitten_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
# "kitten_hat", "kitten_hat", "cat_hat", "cat_hat", "kitten_hat",
# "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
# "kitten_hat", "kitten_hat", "kitten_hat"), clicked_adopt_today = c(1,
# 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1,
# 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
# 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1,
# 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1,
# 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0,
# 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
# 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1,
# 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1,
# 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1,
# 1, 1, 1, 1)), .Names = c("visit_date", "condition", "clicked_adopt_today"
# ), row.names = c(NA, -194L), class = c("tbl_df", "tbl", "data.frame"
# ))
#
# write.csv(followup, 'followup_experiment_data.csv', row.names = FALSE)

# Read in data for follow-up experiment
followup_experiment_data <- read_csv("followup_experiment_data.csv")

# View conversion rates by condition
followup_experiment_data %>%
  group_by(condition) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

# Run logistic regression
followup_experiment_results <- glm(clicked_adopt_today ~ condition,
                                   family = "binomial",
                                   data = followup_experiment_data) %>% tidy()
followup_experiment_results




## Pre-follow-up experiment assumptions
library(tidyverse)
eight_month_checkin_data <- read_csv("eight_month_checkin_data.csv")

# Plot 8 months data
# Compute monthly summary
eight_month_checkin_data_sum <- eight_month_checkin_data %>%
  mutate(month_text = month(visit_date, label = TRUE)) %>%
  group_by(month_text, condition) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

# Plot month-over-month results
ggplot(eight_month_checkin_data_sum,
       aes(x = month_text,
           y = conversion_rate,
           color = condition,
           group = condition)) +
  geom_point() +
  geom_line()


## Plot styling 1
# Plot monthly summary
ggplot(eight_month_checkin_data_sum,
       aes(x = month_text,
           y = conversion_rate,
           color = condition,
           group = condition)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1),
                     labels = percent) +
  labs(x = "Month",
       y = "Conversion Rate")


## Plot styling 2
# Plot monthly summary
ggplot(eight_month_checkin_data_sum,
       aes(x = month_text,
           y = conversion_rate,
           color = condition,
           group = condition)) +
  geom_point(size = 4) +
  geom_line(lwd = 1) +
  scale_y_continuous(limits = c(0, 1),
                     labels = percent) +
  labs(x = "Month",
       y = "Conversion Rate")




## Follow-up experiment assumptions
## Conversion rate between years

no_hat_data_sum <- structure(list(year = c(2017, 2017, 2017, 2017, 2017, 2017, 2017,
2017, 2017, 2017, 2017, 2017, 2018, 2018, 2018, 2018, 2018, 2018,
2018, 2018), month = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L,
8L, 9L, 10L, 11L, 12L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L), .Label = c("Jan",
"Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
"Nov", "Dec"), class = c("ordered", "factor")), conversion_rate = c(0.17741935483871,
0.167857142857143, 0.129032258064516, 0.143333333333333, 0.251612903225806,
0.29, 0.390322580645161, 0.506451612903226, 0.296666666666667,
0.2, 0.23, 0.445161290322581, 0.164516129032258, 0.225, 0.135483870967742,
0.136666666666667, 0.267741935483871, 0.306666666666667, 0.345161290322581,
0.580645161290323)), .Names = c("year", "month", "conversion_rate"
), row.names = c(NA, -20L), class = c("tbl_df", "tbl", "data.frame"
))

# Compute difference over time
no_hat_data_diff <- no_hat_data_sum %>%
  spread(year, conversion_rate) %>%
  mutate(year_diff = `2018` - `2017`)
no_hat_data_diff

# Compute summary statistics
mean(no_hat_data_diff$year_diff, na.rm = TRUE)
sd(no_hat_data_diff$year_diff, na.rm = TRUE)


# Load package for power analysis
library(powerMediation)

# Run power analysis for logistic regression
total_sample_size3 <- SSizeLogisticBin(0.49,
                                       0.64,
                                       0.5,
                                       0.05,
                                       0.8
)

total_sample_size3


## Re-run glm() for follow-up
followup_experiment_data_sep <- structure(list(visit_date = structure(c(17784, 17782, 17792,
17776, 17789, 17789, 17798, 17786, 17792, 17780, 17793, 17800,
17783, 17786, 17796, 17794, 17781, 17785, 17803, 17795, 17791,
17795, 17790, 17796, 17786, 17780, 17796, 17798, 17802, 17782,
17788, 17799, 17784, 17803, 17794, 17798, 17801, 17792, 17800,
17778, 17783, 17797, 17795, 17796, 17791, 17788, 17802, 17797,
17780, 17804, 17783, 17804, 17803, 17781, 17790, 17781, 17778,
17780, 17790, 17780, 17786, 17800, 17797, 17792, 17785, 17783,
17785, 17785, 17780, 17798, 17785, 17801, 17788, 17797, 17801,
17789, 17794, 17793, 17794, 17777, 17802, 17801, 17800, 17796,
17776, 17788, 17791, 17799, 17781, 17804, 17803, 17779, 17782,
17804, 17794, 17783, 17801, 17777, 17775, 17791, 17781, 17799,
17796, 17782, 17798, 17789, 17794, 17789, 17775, 17785, 17791,
17791, 17776, 17779, 17793, 17795, 17792, 17788, 17778, 17802,
17795, 17782, 17798, 17798, 17799, 17775, 17793, 17801, 17797,
17776, 17779, 17799, 17796, 17783, 17785, 17799, 17799, 17801,
17798, 17797, 17786, 17776, 17777, 17804, 17777, 17786, 17790,
17792, 17804, 17798, 17792, 17804, 17797, 17777, 17784, 17787,
17787, 17796, 17791, 17785, 17803, 17796, 17781, 17791, 17776,
17777, 17775, 17792, 17785, 17796, 17798, 17788, 17793, 17800,
17799, 17784, 17802, 17794, 17788, 17793, 17783, 17796, 17801,
17795, 17780, 17785, 17778, 17777, 17803, 17799, 17802, 17788,
17780, 17802, 17794, 17783, 17778, 17792, 17802, 17796, 17785,
17801, 17804, 17797, 17783, 17798, 17796, 17792, 17776, 17786,
17783, 17795, 17802, 17782, 17783, 17801, 17798, 17794, 17787,
17776, 17789, 17786, 17790, 17804, 17781, 17792, 17776, 17776,
17801, 17781, 17788, 17783, 17796, 17789, 17784, 17781, 17799,
17802, 17802, 17793, 17779, 17801, 17791, 17801, 17781, 17792,
17790, 17801, 17776, 17781, 17800, 17796, 17803, 17783, 17802,
17794, 17781, 17777, 17788, 17792, 17799, 17794, 17778, 17792,
17791, 17794, 17798, 17799, 17794, 17795, 17804, 17797, 17783,
17776, 17788, 17789, 17802, 17778, 17788, 17798, 17781, 17778,
17775, 17800, 17786, 17788, 17789, 17793, 17777, 17780, 17777,
17787, 17794, 17801, 17803, 17776, 17793, 17799, 17784, 17783,
17789, 17786, 17798, 17786, 17802, 17802, 17783, 17786, 17785,
17791, 17793, 17789, 17782, 17782, 17791, 17804, 17784, 17788,
17804, 17787, 17780, 17782, 17791, 17789, 17798, 17793, 17798,
17782, 17797, 17788, 17778, 17794, 17780, 17801, 17795, 17779,
17796, 17796, 17786, 17798, 17791, 17780), class = "Date"), condition = c("cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat", "cat_hat",
"cat_hat", "cat_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat", "kitten_hat",
"kitten_hat", "kitten_hat", "kitten_hat"), clicked_adopt_today = c(0,
0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1,
1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1,
0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0,
0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1,
0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0,
0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1,
1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1,
1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0,
0, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0,
1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0,
0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0,
1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0,
1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1,
1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0,
1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1,
1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1,
1, 0, 1, 1, 1)), .Names = c("visit_date", "condition", "clicked_adopt_today"
), row.names = c(NA, -342L), class = c("tbl_df", "tbl", "data.frame"
))


followup_experiment_data_sep %>%
  group_by(condition) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

followup_experiment_data_sep_results <- glm(clicked_adopt_today ~ condition,
                                            family = 'binomial',
                                            data = followup_experiment_data_sep
) %>%  tidy()

followup_experiment_data_sep_results
## Our follow-up experiment was successful! Now that we
## pulled the correct number of data points, we can see that
## there is a boost by using a kitten over a cat.

