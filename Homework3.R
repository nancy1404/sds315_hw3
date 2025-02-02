# WHAT TO INSTALL BEFORE YOU START (just in case you don't run the code in order.)
library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
library(boot)

# <PROBLEM 1>

library(tidyverse)
library(ggplot2)
library(readr)

gasprices <- read.csv("gasprices.csv")

# Theories
# a) Gas stations charge more if they lack direct competition
ggplot(gasprices, aes( x= as.factor(Competitors), y = Price)) +
  geom_boxplot() +
  labs(
    title = "Gas Prices vs. Direct Competition Presence",
    caption = "* Y shows the gas stations with direct competition, while N shows those without direct competition.
    This boxplot shows the relationship between gas price for the presence of direct competition of gas stations.",
    x = "Competitors Present (Yes or No)",
    y = "Gas Price ($)"
  )

# mean difference calculations + confidence interval
competitors_mean <- gasprices |>
  group_by(Competitors) |>
  summarize(mean_price = mean(Price, na.rm = TRUE),
            conf_lower = confint(lm(Price ~ 1, data = cur_data()), level = 0.95) [1,1],
            conf_upper = confint(lm(Price ~ 1, data = cur_data()), level = 0.95) [1,2] )
competitors_mean

# b) The richer the area, the higher the gas prices

library(dplyr)

# group by Zip code (area) w/ price and income average
# mean calculations + confidence level
area_avg <- gasprices |>
  group_by(Zipcode) |>
  summarize(
    avg_price = mean(Price, na.rm = TRUE),
    avg_income = mean(Income, na.rm = TRUE),
    conf_lower = confint(lm(Price ~ 1, data = cur_data()), level = 0.95) [1, 1],
    conf_upper = confint(lm(Price ~ 1, data = cur_data()), level = 0.95) [1, 2]
  )
area_avg

richest_area <- area_avg |>
  arrange(desc(avg_income)) |>
  head(3) # 3 most richest areas

pooreast_area <- area_avg |>
  arrange(avg_income) |>
  head(3) # 3 most poorest areas

# separate here. show results only for things below.
selected_areas <- c(richest_area$Zipcode, pooreast_area$Zipcode)
area_selected <- area_avg |>
  filter(Zipcode %in% selected_areas) |>
  arrange(desc(avg_income))
area_selected

# scatterplot 
ggplot(area_avg, aes( x= avg_income, y = avg_price)) +
  geom_point()+
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_continuous(labels = scales::label_comma()) +
  labs(
    title = "Gas Price vs. Average Income by Area",
    caption = "* ",
    x = "Average Income ($)",
    y = "Average Gas Price ($)"
  )


# c) Gas stations at stoplights charge more
ggplot(gasprices, aes(x = as.factor(Stoplight), y = Price))+
  geom_boxplot()+
  labs(
    title = "Gas Prices vs. Stoplight Presence",
    caption = "* Y represents the gas stations with a stoplight in front, while N represents those without a stoplight.
    This boxplot shows the relationship between gas price for the presence of stoplight in front of gas stations.",
    x = "Stoplight Presence (Yes or No)",
    y = "Gas Price ($)"
  )

# mean difference calculations + confidence interval
stoplights_mean <- gasprices |>
  group_by(Stoplight) |>
  summarize(mean_price = mean(Price, na.rm = TRUE),
            conf_lower = confint(lm(Price ~1, data = cur_data()), level= 0.95) [1,1],
            conf_upper = confint(lm(Price ~1, data = cur_data()), level = 0.95) [1, 2])
stoplights_mean


# d) Gas stations with direct highway access charge more
ggplot(gasprices, aes(x = as.factor(Highway), y = Price)) +
  geom_boxplot() +
  labs(
    title = "Gas Prices vs. Direct Highway Access",
    caption = "* Y represents the gas stations with a direct highway access while N
    represents those without a direct highway access. This boxplot shows the relationship between gas price and the gas stations with/without 
    direct highway access.",
    x = "Direct Highway Access (Yes or No)",
    y = "Gas Price ($)"
  )

# mean difference calculations + confidence level
highway_mean <- gasprices |>
  group_by(Highway) |>
  summarize(mean_price = mean(Price, na.rm = TRUE),
            conf_lower = confint(lm(Price ~ 1, data = cur_data()), level = 0.95) [1,1],
            conf_upper = confint(lm(Price ~ 1, data = cur_data()), level = 0.95) [1,2] )
highway_mean


# e) Shell charges more than non-Shell brands
ggplot(gasprices, aes(x= Brand, y = Price)) +
  geom_boxplot() +
  labs(
    title = "Gas Price by Brand",
    caption = "* This boxplot shows the gas price per each brand, especially to 
    find out if Shell specifically charges more than non-Shell brands.",
    x = "Brand",
    y = "Gas Price ($)"
  )

# mean calculations + confidence level
shell_mean <- gasprices |>
  group_by(Brand)|>
  summarize(mean_price = mean(Price, na.rm = TRUE),
            conf_lower = confint(lm(Price ~ 1, data = cur_data()), level = 0.95) [1,1],
            conf_upper = confint(lm(Price ~ 1, data = cur_data()), level = 0.95) [1,2] )
shell_mean


# PROBLEM 2

# PART A : bootstrap for avg mileage of 2011 S-class 63 AMGs

library(boot)
sclass <- read_csv("sclass.csv")

sclass_filtered <- sclass |> #filter for 2011 63 AMG
  filter(year == 2011, trim == "63 AMG")
# 116 cars found.

boot_mean <-function(data, indices) {
  return(mean(data[indices], na.rm = TRUE))
}

set.seed(111) # reproducibility check

boot_results <- boot(data = sclass_filtered$mileage,
                     statistic = boot_mean,
                     R = 10000) # 10000 samples bootstrap resampling
boot_results

confidence_a <- boot.ci(boot_results, type = "perc")
confidence_a

# PART B: bootstrap for 2014 S-class 550s painted black

sclass_filtered2 <- sclass |>
  filter(year == 2014, trim == "550")
# sclass_filtered2 #2889 cars found

sclass_filtered2 <- sclass |>
  filter(year == 2014, trim == "550") |>
  mutate(isBlack = ifelse(color == "Black", TRUE, FALSE))

boot_proportion <- function(data, indices) {
  return(mean(data[indices], na.rm = TRUE))
}

set.seed(111) #reproducibility check

# bootstrap resample
boot_results2 <- boot(data=sclass_filtered2$isBlack,
                      statistic = boot_proportion,
                      R = 10000) # 10000 bootstrap samples
boot_results2

confidence_b <- boot.ci(boot_results2, type = "perc")
confidence_b


# PROBLEM 3: TV Network Survey

# PART A: Who Makes People Happier, Ed or Earl?

tvsurvey <- read.csv('SDS 315/nbc_pilotsurvey.csv')

filtered_tvsurvey <- tvsurvey |>
  filter(Show %in% c("Living with Ed", "My Name is Earl"))

ed_happy <- filtered_tvsurvey |>
  filter(Show == "Living with Ed") |>
  pull(Q1_Happy)

earl_happy <- filtered_tvsurvey |>
  filter(Show == "My Name is Earl") |>
  pull(Q1_Happy)

# bootstrap function for mean difference
boot_diff_mean <- function(data, indices) {
  sampled_data <- data[indices, ]
  ed_mean <- mean(sampled_data[sampled_data$Show == "Living with Ed", "Q1_Happy"], na.rm = TRUE)
  earl_mean <- mean(sampled_data[sampled_data$Show == "My Name is Earl", "Q1_Happy"], na.rm = TRUE)
  return(ed_mean - earl_mean)
}

# combined final data
final_data <- filtered_tvsurvey[, c("Show", "Q1_Happy")]

# reproducibility check
set.seed(111)

bootstrap_results <- boot(data = final_data, statistic = boot_diff_mean, R = 10000)
bootstrap_results

confidence_A <- boot.ci(bootstrap_results, type = "perc")
confidence_A


# PART B: The Big Loser vs. The Apprentice: Los Angeles

# Which one made people feel more annoyed?

annoyed_data <- tvsurvey |>
  filter(Show %in% c("The Biggest Loser", "The Apprentice: Los Angeles")) |>
  select(Show, Q1_Annoyed)

# bootstrap function defined.
boot_diff_mean_annoyed <- function(data, indices) {
  sampled_data <- data[indices, ]
  
  mean_biggest_loser <- mean(sampled_data$Q1_Annoyed[sampled_data$Show == "The Biggest Loser"], na.rm = TRUE)
  mean_apprentice <- mean(sampled_data$Q1_Annoyed[sampled_data$Show == "The Apprentice: Los Angeles"], na.rm = TRUE)
  
  return(mean_biggest_loser - mean_apprentice)
}

set.seed(111) # reproducibility check

# bootstrapping
bootstrap_results2 <- boot(data = annoyed_data,
                           statistic = boot_diff_mean_annoyed, R = 10000)

bootstrap_results2

# 95% confidence interval
confidence_B <- boot.ci(bootstrap_results2, type = "perc")
confidence_B


# PART C: Dancing with the Stars, Confusing or Not?

# filter data for Dancing with the Stars show only
dancing_data <- tvsurvey |>
  filter(Show == "Dancing with the Stars") |>
  select(Q2_Confusing)

# confusing response (4 or 5)

dancing_data$Confusing <- ifelse(dancing_data$Q2_Confusing >= 4, 1, 0)

# function for bootstrapping
boot_confusing <- function(data, indices) {
  sampled_data <- data[indices, ]
  return(mean(sampled_data$Confusing, na.rm = TRUE))
}

set.seed(111) #reproducibility check

# bootstrap resampling
bootstrap_results3 <- boot(data = dancing_data,
                           statistic = boot_confusing, R = 10000)
bootstrap_results3

# 95% confidence interval
confidence_C <- boot.ci(bootstrap_results3, type = "perc")
confidence_C


# PROBLEM 4: EBAY

ebay <- read.csv("SDS 315/ebay.csv")

# ration of each DMA
ebay <- ebay |>
  mutate(revenue_ratio = rev_after / rev_before)

# function calculating diff means of rev ratio for treatment vs. control
boot_diff_rev <- function(data, indices) {
  sampled_data <- data[indices, ]
  
  # calculate mean rev ratio for treatment and control
  mean_treatment <- mean(sampled_data$revenue_ratio[sampled_data$adwords_pause == 1])
  mean_control <- mean(sampled_data$revenue_ratio[sampled_data$adwords_pause == 0])
  
  return(mean_treatment - mean_control)
}

# bootstrap resampling
set.seed(111) # reproducibility 
bootstrap_results4 <- boot(data = ebay, statistic = boot_diff_rev, R = 10000)
bootstrap_results4

confidence_D <-boot.ci(bootstrap_results4, type = "perc")
confidence_D



