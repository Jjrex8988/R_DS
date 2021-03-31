# Title     : AB Testing in R
# Objective : TODO
# Created by: Jjrex8988
# Created on: 31/3/2021


## Chapter 3: Experimental Design in A/B Testing
## A/B testing research questions
## Article click frequency monthly

library(tidyverse)
library(lubridate)

# write.csv(viz_2017, 'viz_website_2017.csv', row.names = FALSE)

# Compute summary by month
viz_website_2017 <- read_csv("viz_website_2017.csv")

viz_website_2017 %>%
  group_by(month(visit_date)) %>%
  summarize(article_conversion_rate = mean(clicked_article))

viz_website_2018 <- read_csv("data_viz_website_2018_04.csv")
str(viz_website_2018)

#
# viz_website_2018 %>%
#   group_by(month(visit_date)) %>%
#   summarize(article_conversion_rate = mean(clicked_article))


# 'Like' click frequency plot
library(scales)
# Compute 'like' click summary by month
viz_website_2017_like_sum <- viz_website_2017 %>%
  mutate(month = month(visit_date, label = TRUE)) %>%
  group_by(month) %>%
  summarize(like_conversion_rate = mean(clicked_like))

# Plot 'like' click summary by month
ggplot(viz_website_2017_like_sum,
       aes(x = month, y = like_conversion_rate, group = 1)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1), labels = percent)


# viz_website_2018_like_sum <- viz_website_2018 %>%
#   mutate(month = month(visit_date, label = TRUE)) %>%
#   group_by(month) %>%
#   summarize(like_conversion_rate = mean(clicked_like))
#

#
# ggplot(viz_website_2018_like_sum, aes(x = month, y = like_conversion_rate,
#                                       group =1
#                                       )) +
#   geom_point() + geom_line() + scale_y_continuous(limits = c(0, 1),
#                                                   labels = percent
# )


## 'Like' / 'Share' click frequency plot

# viz_website_2017_like_share_sum_test <- viz_website_2017 %>%
#   mutate(month = month(visit_date, label = TRUE)) %>%
#   group_by(month) %>%
#   summarize(conversion_rate = mean(clicked_like))

viz_website_2017_like_share_sum <- structure(list(month = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L,
8L, 9L, 10L, 11L, 12L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L,
11L, 12L), .Label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), class = c("ordered",
"factor")), action = c("like", "like", "like", "like", "like",
"like", "like", "like", "like", "like", "like", "like", "share",
"share", "share", "share", "share", "share", "share", "share",
"share", "share", "share", "share"), conversion_rate = c(0.197354838709677,
0.117571428571429, 0.147870967741935, 0.165733333333333, 0.211741935483871,
0.2974, 0.404, 0.124516129032258, 0.152533333333333, 0.201677419354839,
0.249333333333333, 0.293677419354839, 0.0516129032258065, 0.0112857142857143,
0.0191612903225806, 0.0296, 0.0501290322580645, 0.0701333333333333,
0.020258064516129, 0.0103870967741935, 0.0176666666666667, 0.0385161290322581,
0.0607333333333333, 0.0187741935483871)), .Names = c("month",
"action", "conversion_rate"), row.names = c(NA, -24L), class = c("tbl_df",
"tbl", "data.frame"))


# Plot comparison of 'like'ing and 'sharing'ing an article
ggplot(viz_website_2017_like_share_sum,
       aes(x = month, y = conversion_rate, color = action, group = action)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1), labels = percent)




## Assumptions and types of A/B testing
## Between vs. within
## (Q) Which of these experiments describes a within-participant experiment?
## (A) You have a group of email users and for two weeks randomly show one
## of two "Inbox" designs whenever a person logs in.


## Plotting A/A data
viz_website_2018_01  <- read_csv("viz_website_2018_01.csv")

# Compute conversion rates for A/A experiment
viz_website_2018_01_sum <- viz_website_2018_01 %>%
  group_by(condition) %>%
  summarize(like_conversion_rate = mean(clicked_like))

viz_website_2018_01_sum

# Plot conversion rates for two conditions
ggplot(viz_website_2018_01_sum,
       aes(x = condition, y = like_conversion_rate)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 1), labels = percent)


## Analyzing A/A data
# Load library to clean up model outputs
library(broom)

# Run logistic regression
aa_experiment_results <- glm(clicked_like ~ condition,
                             family = "binomial",
                             data = viz_website_2018_01) %>%
  tidy()
aa_experiment_results




## Confounding variables
## Examples of confounding variables
## (Q) You run an experiment where in one condition, the
## homepage is a tabby cat, and in another condition, it
## is a black cat. Both conditions are run at the same time.
## At the beginning of the experiment, a children's movie
## comes out starring a black cat. Which of these statements
## is true?
## (A) If the black cat condition results in a higher
## conversion rate it may be due to an external confounding
## variable.


## Confounding variable example analysis
viz_website_2018_02 <- read_csv("viz_website_2018_02.csv")

# Compute 'like' conversion rate by week and condition
viz_website_2018_02 %>%
  mutate(week = week(visit_date)) %>%
  group_by(week, condition) %>%
  summarize(like_conversion_rate = mean(clicked_like))

# Compute 'like' conversion rate by if article published and condition
viz_website_2018_02 %>%
  group_by(article_published, condition) %>%
  summarize(like_conversion_rate = mean(clicked_like))


## Confounding variable example plotting
viz_website_2018_02_sum <- viz_website_2018_02 %>%
  group_by(visit_date, condition, article_published) %>%
  summarize(like_conversion_rate = mean(clicked_like))


# Plot 'like' conversion rates by date for experiment
ggplot(viz_website_2018_02_sum,
       aes(x = visit_date,
           y = like_conversion_rate,
           color = condition,
           linetype = article_published,
           group = interaction(condition, article_published))) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = as.numeric(as.Date("2018-02-15"))) +
  scale_y_continuous(limits = c(0, 0.3), labels = percent)




## Side effects
## Confounding variable vs. side effect
## (Q) For our cat adoption website with an experiment comparing
## a tabby cat and a black cat, the photo of the tabby cat
## loads in 5 seconds and the black cat in 2 seconds.
## The tabby cat is 1 year old and the black cat is a 6 years old.
## Which of these statements is true?

## (A) The load time is a side effect, the age of the cats is a confounding variable


## Side effect load time plot
viz_website_2018_03 <- read_csv("viz_website_2018_03.csv")

# Compute 'like' conversion rate and mean pageload time by day
viz_website_2018_03_sum <- viz_website_2018_03 %>%
  group_by(visit_date, condition) %>%
  summarize(mean_pageload_time = mean(pageload_time),
            like_conversion_rate = mean(clicked_like))

# Plot effect of 'like' conversion rate by pageload time
ggplot(viz_website_2018_03_sum,
       aes(x = mean_pageload_time, y = like_conversion_rate, color = condition)) +
  geom_point()


viz_website_2018_03_sum <- read_csv("viz_website_2018_03_t.csv")



# Plot 'like' conversion rate by day
ggplot(viz_website_2018_03_sum,
       aes(x = visit_date,
           y = like_conversion_rate,
           color = condition,
           linetype = pageload_delay_added,
           group = interaction(condition, pageload_delay_added))) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-15"))) +
  scale_y_continuous(limits = c(0, 0.3), labels = percent)

## GOT PROBLEM WITH VIZ_WEBSITE_2018_03_SUM_T

## Great job! We can clearly see the
## effect that adding the delay had on the two conditions.




## Chapter 4: Statistical Analyses in A/B Testing
## Power analyses
## Logistic regression power analysis
# Load package to run power analysis
library(powerMediation)

# Run power analysis for logistic regression
total_sample_size <- SSizeLogisticBin(p1 = 0.17,
                                      p2 = 0.27,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size


## pwr.t.test() documentation
library(pwr)
# ? pwr.t.test

## (Q) I mentioned in the video that there are
## additional arguments that can be provided to
## pwr.t.test(). Take a look at the documentation
## to see what the difference is between type and
## alternative. Note, you'll need to load the pwr
## package.

## (A) type refers to number and type of samples;
## alternative to hypothesis.


## T-test power analysis
# Load package to run power analysis
library(pwr)

# Run power analysis for t-test
sample_size <- pwr.t.test(d = 0.3,
                          sig.level = 0.05,
                          power = 0.8)
sample_size




## Statistical tests
## Logistic regression

viz_website_2018_04 <- read_csv("data_viz_website_2018_04.csv")


# Load package to clean up model outputs
library(broom)

# Run logistic regression
ab_experiment_results <- glm(clicked_like ~ condition,
                             family = "binomial",
                             data = viz_website_2018_04) %>%
  tidy()
ab_experiment_results


## T-test
# Run t-test
ab_experiment_results <- t.test(time_spent_homepage_sec ~ condition,
                                data = viz_website_2018_04)
ab_experiment_results




## Stopping rules and sequential analysis
## What is a sequential analysis?
## (Q) What is a sequential analysis?
## (A) Sequential analysis is when you adjust your p-value to allow
## for looking at your data at various stopping points.


## Sequential analysis three looks
# Load package to run sequential analysis
library(gsDesign)

# Run sequential analysis

seq_analysis_3looks <- gsDesign(3,
                                1,
                                0.05,
                                0.2, sfu = "Pocock"
)

seq_analysis_3looks


## Sequential analysis sample sizes
# Fill in max number of points and compute points per group and find stopping points
max_n <- 3000
max_n_per_group <- max_n / 2
stopping_points <- max_n_per_group * seq_analysis_3looks$timing
stopping_points




## Multivariate testing
## Plotting time homepage in multivariate experiment
library(broom)

viz_website_2018_05 <- read_csv("viz_website_2018_05.csv")

multivar_results <- viz_website_2018_05 %>%
  mutate(word_one = factor(word_one, levels = c(
    "tips", "tools"
  ))) %>%
  mutate(word_two = factor(word_two, levels = c(
    "better", "amazing"
  ))) %>%
  lm(time_spent_homepage_sec ~ word_one * word_two, data = .) %>%
  tidy()

viz_website_2018_05_sum <- viz_website_2018_05 %>%
  group_by(word_one, word_two) %>%
  summarize(mean_time_spent_homepage_sec = mean(time_spent_homepage_sec))

ggplot(viz_website_2018_05_sum,
       aes(x = word_one, y = mean_time_spent_homepage_sec,
           fill = word_two)) +
  geom_bar(stat = 'identity', position = 'dodge')


## Plotting 'like' clicks in multivariate experiment

# Compute summary values for four conditions
viz_website_2018_05_like_sum <- viz_website_2018_05 %>%
  group_by(word_one, word_two) %>%
  summarize(like_conversion_rate = mean(clicked_like))

# Plot summary values for four conditions
ggplot(viz_website_2018_05_like_sum,
       aes(x = word_one, y = like_conversion_rate,
           fill = word_two)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_y_continuous(limits = c(0, 1), labels = percent)


## Multivariate design statistical test
# Load package for cleaning model output
library(broom)

# Organize variables and run logistic regression
viz_website_2018_05_like_results <- viz_website_2018_05 %>%
  mutate(word_one = factor(word_one,
                           levels = c("tips", "tools"))) %>%
  mutate(word_two = factor(word_two,
                           levels = c("better", "amazing"))) %>%
  glm(clicked_like ~ word_one * word_two,
                                    family = "binomial",
                                    data = .) %>%
  tidy()
viz_website_2018_05_like_results
## Once again we found a significant interaction.
## While there was no effect of word_two for the baseline of
## 'Tips', there likely was an effect for 'Tools'.




## A/B Testing Recap

