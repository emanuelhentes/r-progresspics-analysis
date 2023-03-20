# Load packages
library(tidyverse)
library(gtsummary)
library(qdapRegex)
library(webshot2)

# Load data
data <- read_csv("https://raw.githubusercontent.com/emanuelhentes/r-progresspics-analysis/main/progresspics-database.csv")

# Data preparation
data$feet <-substr(data$height2, 1, 1) 
data$inches <- rm_between(data$height2, "ft", "in", extract=TRUE)
data$feet <- as.numeric(data$feet)
data$inches <- as.numeric(data$inches)

# Create new variables
data <- data %>%
  mutate(height_inches = feet * 12 + inches) %>%
  mutate(bmi0 = weight0 / (height_inches)^2 * 703) %>%
  mutate(bmi1 = weight1 / (height_inches)^2 * 703) %>%
  mutate(bmiDiff = bmi0 - bmi1) %>%
  mutate(bmiDiff_perc = ((bmi0 - bmi1) / bmi0) * 100) %>%
  mutate(weightDiff_perc = ((weight0 - weight1) / weight0) * 100) %>%
  mutate(waistDiff_perc = ((waist0 - waist1) / waist0) * 100)


# Descriptive statistics table
## Age and height t able
table0 <- data %>% 
  subset(select = c(age, height_inches, gender)) %>%
  tbl_summary(by = gender,
              label = list(age = "Age",
                           height_inches = "Height (in)"),
              statistic = ~ "{mean} ({sd})  \n{median} ({min}–{max})",
              missing = "no",
              digits = all_continuous() ~ 1) %>%
  bold_labels()

## Weight table
table1 <- data %>% 
  subset(select = c(weight0, weight1, weightDiff, weightDiff_perc, gender)) %>%
  tbl_summary(by = gender,
              label = list(weight0 = "Before",
                           weight1 = "After",
                           weightDiff = "Weight difference",
                           weightDiff.perc = "Weight difference (%)"),
              statistic = ~ "{mean} ({sd})  \n{median} ({min}–{max})",
              missing = "no",
              digits = all_continuous() ~ 1)

## Waist size table
table2 <- data %>% 
  subset(select = c(waist0, waist1, waistDiff, waistDiff_perc, gender)) %>%
  tbl_summary(by = gender,
              label = list(waist0 = "Before",
                           waist1 = "After",
                           waistDiff = "Waist size difference",
                           waistDiff.perc = "Waist size difference (%)"),
              statistic = ~ "{mean} ({sd})  \n{median} ({min}–{max})",
              missing = "no",
              digits = all_continuous() ~ 1)

## BMI table
table3 <- data %>% 
  subset(select = c(bmi0, bmi1, bmiDiff, bmiDiff_perc, gender)) %>%
  tbl_summary(by = gender,
              label = list(bmi0 = "Before",
                           bmi1 = "After",
                           bmiDiff = "BMI difference",
                           bmiDiff.perc = "BMI difference (%)"),
              statistic = ~ "{mean} ({sd})  \n{median} ({min}–{max})",
              missing = "no",
              digits = all_continuous() ~ 1)

## Stack all tables
descriptive <-
  tbl_stack(list(table0, table1, table2, table3), group_header = c("", "Weight (lbs)", "Waist (in)", "BMI")) %>%
  as_gt() %>%
  gt::fmt_markdown(columns = everything()) %>%
  gt::tab_header(title = "Table 1. Descriptive statistics") %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups(groups = everything()))

# Plot
plot <- ggplot(data, aes(x = weightDiff, y = waistDiff, shape = gender, color = gender)) +
  geom_point(size = 4) + 
  geom_smooth(method = lm, se = FALSE) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(title = "Plot of waist size lost by weight lost",
       x  = "Weight lost (lbs)", y = "Waist size lost (in)",
       shape = "Gender", color = "Gender")
plot

# Regression table
## (1) Weight lost (lbs) / inch table
w1 <- lm(weightDiff ~ waistDiff, data, subset = gender == "Male") %>% 
  tbl_regression(label = waistDiff ~ "(1) Weight lost (lbs) / inch") %>%
  modify_caption("Table 2. Results of linear regressions for weight loss")
w2 <- lm(weightDiff ~ waistDiff, data, subset = gender == "Female") %>%
  tbl_regression(label = waistDiff ~ "(1) Weight lost (lbs) / inch")

wregression <- tbl_merge(tbls = list(w1, w2),
                         tab_spanner = c("**Males**", "**Females**"))

## (2) Weight percentage lost (lbs) / inch table
wp1 <- lm(weightDiff_perc ~ waistDiff, data, subset = gender == "Male") %>% 
  tbl_regression(label = waistDiff ~ "(2) Weight percentage lost (lbs) / inch")
wp2 <- lm(weightDiff_perc ~ waistDiff, data, subset = gender == "Female") %>%
  tbl_regression(label = waistDiff ~ "(2) Weight percentage lost (lbs) / inch")

wpregression <- tbl_merge(tbls = list(wp1, wp2),
                          tab_spanner = c("**Males**", "**Females**"))

## (3) BMI lost / inch table
b1 <- lm(bmiDiff ~ waistDiff, data, subset = gender == "Male") %>% 
  tbl_regression(label = waistDiff ~ "(3) BMI lost / inch")
b2 <- lm(bmiDiff ~ waistDiff, data, subset = gender == "Female") %>%
  tbl_regression(label = waistDiff ~ "(3) BMI lost / inch")

bregression <- tbl_merge(tbls = list(b1, b2),
                         tab_spanner = c("**Males**", "**Females**"))

## (4) BMI percentage lost / inch table
bp1 <- lm(bmiDiff_perc ~ waistDiff, data, subset = gender == "Male") %>% 
  tbl_regression(label = waistDiff ~ "(4) BMI percentage lost / inch")
bp2 <- lm(bmiDiff_perc ~ waistDiff, data, subset = gender == "Female") %>%
  tbl_regression(label = waistDiff ~ "(4) BMI percentage lost / inch")

bpregression <- tbl_merge(tbls = list(bp1, bp2),
                          tab_spanner = c("**Males**", "**Females**"))

## Stack all previous tables
regression <- tbl_stack(tbls = list(wregression, wpregression, bregression, bpregression)) %>%
  modify_header(label = "**Regression**")
