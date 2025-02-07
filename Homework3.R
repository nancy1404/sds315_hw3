# WHAT TO INSTALL BEFORE YOU START (just in case you don't run the code in order.)
library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
library(boot)
library(mosaic)

# <PROBLEM 1>

library(tidyverse)
library(ggplot2)
library(mosaic)
library(readr)

# load gas prices dataset
gasprices <- read.csv("gasprices.csv")

# visualize relationship btw gas prices and direct competition 
# Theories
# a) Gas stations charge more if they lack direct competition
ggplot(gasprices, aes( x= Competitors, y = Price)) +
  geom_boxplot() +
  labs(
    title = "Gas Prices vs. Direct Competition Presence",
    caption = "* Y shows the gas stations with direct competition, while N shows those without direct competition.
    This boxplot shows the relationship between gas price for the presence of direct competition of gas stations.",
    x = "Competitors Present (Yes or No)",
    y = "Gas Price ($)"
  )

# mean diff btw gas prices for stations with and without competition
mean_price_competition <- mean(Price ~ Competitors, data = gasprices)

# display mean prices for each category (Y and N)
mean_price_competition

# bootstrap 
boot_price_competition <- do(10000) * diffmean(Price ~ Competitors, data = resample(gasprices))

# confidence interval for diff in means
confint(boot_price_competition, level = 0.95)


# b) The richer the area, the higher the gas prices

# scatterplot for gas prices vs income
ggplot(gasprices, aes(x = Income, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_continuous(labels = scales::label_comma()) +
  labs(
    title = "Gas Prices vs. Income Levels",
    caption = "* This scatterplot shows the relationship between gas prices and average income levels across areas.",
    x = "Income ($)",
    y = "Gas Price ($)"
  )

cor_price_income <- cor(Price ~ Income, data = gasprices)
cor_price_income

set.seed(111) # for reproducibility
# Bootstrap for price differences by income
boot_price_income <- do(10000) * cor(Price ~ Income, data = resample(gasprices))

# Confidence interval
confint(boot_price_income, level = 0.95)


# c) Gas stations at stoplights charge more

ggplot(gasprices, aes(x= as.factor(Stoplight), y = Price )) +
  geom_boxplot()+
  labs(
    title = "Gas Prices vs. Stoplight Presence",
    caption = "* Y represents the gas stations with a stoplight in front, while N represents those without a stoplight.
    This boxplot shows the relationship between gas price for the presence of stoplight in front of gas stations.",
    x = "Stoplight Presence (Yes or No)",
    y = "Gas Price ($)"
  )

# mean price diff btw gas stations
mean_price_stoplight <- mean(Price ~ Stoplight, data = gasprices)
mean_price_stoplight

boot_price_stoplight <- do(10000) * diffmean(Price~Stoplight, data = resample(gasprices))

confint(boot_price_stoplight, level = 0.95)

# d) Gas stations with direct highway access charge more

ggplot(gasprices, aes(x = as.factor(Highway), y = Price)) +
  geom_boxplot() +
  labs(
    title = "Gas Prices vs. Direct Highway Access",
    x = "Direct Highway Access (Yes or No)",
    y = "Gas Price ($)",
    caption = "Y represents (Y) the gas stations with a direct highway access while (N) represents those without a direct highway access. 
    This boxplot shows the relationship between gas price and the gas stations with/without direct highway access."
  )

# mean diff btw gas prices for highway presence
mean_price_highway <- mean(Price ~ Highway, data = gasprices)
mean_price_highway

# bootstrap 
boot_price_highway <- do(10000) * diffmean(Price ~ Highway, data = resample(gasprices))

# confidence interval 
confint(boot_price_highway, level = 0.95)

# e) Shell charges more than non-Shell brands

filter data set
gasprices_filtered <- gasprices |>
  mutate(ShellBrand = ifelse(Brand == "Shell", "Shell", "Non-Shell"))

# Plot boxplot comparing Shell vs. Non-Shell
ggplot(gasprices_filtered, aes ( x= ShellBrand, y = Price)) +
  geom_boxplot()+
  labs(
    title = "Gas Price by Brand (Shell or Non-Shell)",
    caption = "* This boxplot shows the gas price per each brand, especially to 
    find out if Shell specifically charges more than non-Shell brands.",
    x = "Brand (Shell or Non-Shell)",
    y = "Gas Price ($)"
  )

# Bootstrap for difference in means
boot_price_brand <- do(10000) * diffmean(Price ~ ShellBrand, data = resample(gasprices_filtered))

# confidence interval for difference in means
confint(boot_price_brand, level = 0.95)

mean_price_brand <- gasprices_filtered |>
  group_by(ShellBrand) |>
  summarize(mean_price = mean(Price, na.rm = TRUE))
mean_price_brand


# PROBLEM 2

# PART A : bootstrap for avg mileage of 2011 S-class 63 AMGs

library(boot)
sclass <- read_csv("sclass.csv")

sclass_filtered <- sclass |> #filter for 2011 63 AMG
  filter(year == 2011, trim == "63 AMG")
# 116 cars found.

# set seed for reproducibility
set.seed(111)

# boostrap resampling for avg mileage
boot_mileage <- do(10000)*mean(~mileage, data = resample(sclass_filtered))

# Histogram of bootstrap distribution
ggplot(boot_mileage) +
  geom_histogram(aes(x=mean)) +
  labs(
    title = "Bootstrap Distribution of Average Mileage for 2011 S-Class 63 AMG",
    x = "Average Mileage",
    y = "Frequency"
  )

# calculate standard error (spread of sampling distribution)
sd(~mean, data = boot_mileage)

# 95% confidence level
confint(boot_mileage, level = 0.95)


# PART B: bootstrap for 2014 S-class 550s painted black

sclass_filtered2 <- sclass |>
  filter(year == 2014, trim == "550") |>
  mutate(isBlack = color == "Black")
# sclass_filtered2 #2889 cars found

set.seed(111) #reproducibility check

# Bootstrap resampling for proportion of black cars
boot_proportion <- do(10000)*mean(~isBlack, data = resample(sclass_filtered2))

# histogram of bootstrap distribution
ggplot(boot_proportion) +
  geom_histogram(aes(x=mean)) +
  labs(
    title = "Bootstrap Distribution of Proportion of Black 2014 S-Class 550 Cars",
    x = "Proportion of Black Cars",
    y = "Frequency"
  )

# standard error (spread of sampling distribution)
sd(~mean, data = boot_proportion)

# 95% confidence level
confint(boot_proportion, level = 0.95)



# PROBLEM 3: TV Network Survey

# PART A: Who Makes People Happier, Ed or Earl?

tvsurvey <- read.csv('nbc_pilotsurvey.csv')

# filter for shows "Living with Ed" and "My Name is Earl"
filtered_tvsurvey <- tvsurvey |>
  filter(Show %in% c("Living with Ed", "My Name is Earl"))

set.seed(111) # seed for reproducibility

# bootstrap resampling for difference in happiness mean btw two shows
boot_diff_mean = do(10000) * diffmean(Q1_Happy ~ Show, data = resample(filtered_tvsurvey))

# bootstrap sampling distribution
ggplot(boot_diff_mean) +
  geom_histogram(aes(x=diffmean)) +
  labs(
    title = "Bootstrap Distribution of Difference in Mean Happiness Ratings",
    caption = "* including Living with Ed and My Name is Earl",
    x = "Difference in Mean Happiness Ratings",
    y = "Frequency"
  )

# calculate standard error
sd(~diffmean, data = boot_diff_mean)

# 95% confidence interval for diff in means
confint(boot_diff_mean, level = 0.95)


# PART B: The Big Loser vs. The Apprentice: Los Angeles
# filter for two shows only
filtered_tvsurvey2 <- tvsurvey |>
  filter(Show %in% c("The Biggest Loser", "The Apprentice: Los Angeles"))

set.seed(111) # reproducibility
# bootstrap resampling for difference in annoyance mean btw two shows
boot_diff_mean_annoyed = do(10000) * diffmean(Q1_Annoyed ~ Show, data = resample(filtered_tvsurvey2)) 

# Bootstrap sampling distribution
ggplot(boot_diff_mean_annoyed) +
  geom_histogram(aes(x=diffmean)) +
  labs(
    title = "Bootstrap Distribution of Difference in Mean Annoyance Ratings",
    caption = "* including The Biggest Loser and The Apprentice: Los Angeles",
    x = "Difference in Mean Annoyance Ratings",
    y = "Frequency"
  )

# calculate sd error
sd(~diffmean, data = boot_diff_mean_annoyed)

# 95% confidence interval for diff in means
confint(boot_diff_mean_annoyed, level = 0.95)



# PART C: Dancing with the Stars, Confusing or Not?

# filter data for Dancing with the Stars show only
dancing_data <- tvsurvey |>
  filter(Show == "Dancing with the Stars") |>
  select(Q2_Confusing)

# create binary variable for confusing response 

dancing_data$Confusing <- ifelse(dancing_data$Q2_Confusing >= 4, 1, 0)

set.seed(111) #reproducibility 

# Bootstrap resampling for the proportion of confusing responses
boot_confusing <- do(10000) * mean(~Confusing, data = resample(dancing_data))

# ggplot
ggplot(boot_confusing) +
  geom_histogram(aes(x=mean), binwidth = 0.01) +
  labs(
    title = "Bootstrap distribution of Proportion of Confusing Responses",
    x = "Proportion of Confusing Responses",
    y = "Frequency"
  )


# calculate standard error (spread of sampling distribution)
sd(~mean, data = boot_confusing)

# 95% confidence interval
# quantile(boot_confusing$X1, probs= c(0.025, 0.975))
confint(boot_confusing, level = 0.95)

# PROBLEM 4: EBAY

ebay <- read.csv("ebay.csv")

# ration of each DMA
ebay <- ebay |>
  mutate(revenue_ratio = rev_after / rev_before)

set.seed(111) # for reproducibility

# Bootstrap resampling for diff in mean revenue ratio btw treatment and control groups
boot_diff_rev <- do(10000) * diffmean(revenue_ratio ~ adwords_pause, data = resample(ebay))

# distribution has any varaibility?
ggplot(boot_diff_rev) +
  geom_histogram(aes(x=diffmean), binwidth = 0.01)+
  labs(
    title = "Bootstrap Distribution of Revenue Ratio Difference",
    x = "Difference in Mean Revenue Ratio",
    y = "Frequency"
  )

# calculate sd error
sd(~diffmean, data = boot_diff_rev)

# 95% confidence interval 
confint(boot_diff_rev, level = 0.95)




