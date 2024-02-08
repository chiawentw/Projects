# Set Up
## packages
library(haven)
library(dplyr)
library(readr)
library(ggplot2)
library(lme4)
library(tibble)
library(ggcorrplot)
library(DHARMa)
library(tibble)
library(geepack)
library(car)
## working folder path
folder_path <- "C:/Users/cwcheng/Dropbox (University of
Michigan)/Chia/687/datasets/SP/rounds 5-8"
## get a list of all .dta files in the folder
file_list <- list.files(path = folder_path, pattern = ".dta",
                        full.names = TRUE)
## initialize an empty data frame to use later on
combined_HCAll = NULL
## select variables we need and iterate through each data set file to combine the data set
for (file in file_list) {
  tryCatch({
    data <- read_dta(file)
    ## extract the year number from the dresid column
    dresid_column <- grep("dresid$", names(data), value = TRUE)
    HCAll <- data %>%
      select(spid, ends_with(c("dgender",
                               "dracehisp",
                               "d2intvrage",
                               "hlkpfrwrk"))) %>%
      mutate(year_number = dresid_column[1]) %>%
      data.frame()
    names(HCAll) = c(1:ncol(HCAll))
    # combine with the existing data
    combined_HCAll <- bind_rows(combined_HCAll, HCAll)
  }, error = function(e) {
    cat("Error in processing file:", file, "\n")
  })
}
# Data Cleaning
## change column names
names(combined_HCAll) = c("id",
                          "gender",
                          "race",
                          "age",
                          "keep_from_work",
                          "year")
## mutate "year" column using for longitudinal analysis
final <- combined_HCAll %>%
  mutate(year = as.numeric(sub("^r(\\d+)dresid$", "\\1", year))) %>%
  filter(year %in% c(5:8))
## save the data frame to an Rdata in our shared folder
save(final, file = "C:/Users/cwcheng/Dropbox (University of
Michigan)/Chia/687/project/final1.Rdata") # 27,469 obs.
## drop invalid data in columns from our data frame
finalkfw <- final %>%
  subset(select = c(1:6),
         subset = rowSums(final > 0,
                          na.rm = TRUE) == ncol(final)) %>%
  ## sort out missing values
  subset(race != 5) %>% ## sort out don't know or no primary selection
  subset(race != 6) %>% ## sort out don't know
  mutate(keep_from_work = ifelse(keep_from_work == 2,
                                 0,
                                 ifelse(keep_from_work == 1,
                                        1,
                                        NA))) %>%
  ## change not being kept from work to 0 and being kept from work to 1
  mutate(gender = ifelse(gender == 2,
                       0,
                       ifelse(gender == 1,
                              1,
                              NA)))
## change female to 0 while male stays 1
## save this dataframe as an Rdata for my groupmates again
save(finalkfw, file = "C:/Users/cwcheng/Dropbox (University of
Michigan)/Chia/687/project/finalkfw1.Rdata") # 23,984 obs.
# Pre-model Building Descriptive Analysis
## a. for independent variables
## race
## create a dataset for the plot later on
race_summary <- finalkfw %>%
  group_by(year, keep_from_work, race) %>%
  summarise(percentage = n()) %>%
  ungroup() %>%
  group_by(year, keep_from_work) %>%
  mutate(Cumpercentage = percentage/sum(percentage)) %>%
  ungroup() %>%
  mutate(keep_from_work = factor(ifelse(keep_from_work == 0, "0
(No)", "1 (Yes)"))) %>%
  mutate(race = case_when(
    race == 1 ~ "White, non-hispanic",
    race == 2 ~ "Black, non-hispanic",
    race == 3 ~ "Other",
    race == 4 ~ "Hispanic"
  ),
  year = case_when(
    year == 5~2015,
    year == 6~2016,
    year == 7~2017,
    year == 8~2018,
  ))
## create a percentage bar plot
ggplot(race_summary, aes(x = factor(year),
                         y = Cumpercentage, fill = keep_from_work)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Percentage of Work Cessation Across the Four Years
Based on Respondents' Races",
       x = "Year",
       y = "Percentage") +
  theme_minimal() +
  guides(fill = guide_legend(title = "Work Cessation")) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0),
        plot.title = element_text(size = 10)) +
  facet_wrap(~ factor(race))
## gender
## create a dataset for the plot later on
gender_summary <- finalkfw %>%
  group_by(year, keep_from_work, gender) %>%
  summarise(percentage = n()) %>%
  ungroup() %>%
  group_by(year, keep_from_work) %>%
  mutate(Cumpercentage = percentage/sum(percentage)) %>%
  ungroup() %>%
  mutate(keep_from_work = factor(ifelse(keep_from_work == 0, "0
(No)", "1 (Yes)"))) %>%
  mutate(gender = case_when(
    gender == 0 ~ "Female",
    gender == 1 ~ "Male"
  ),
  year = case_when(
    year == 5~2015,
    year == 6~2016,
    year == 7~2017,
    year == 8~2018,
  ))
## create a percentage bar plot
ggplot(gender_summary, aes(x = factor(year),
                           y = Cumpercentage, fill = keep_from_work)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Percentage of Work Cessation Across the Four Years
Based on Respondents' Genders",
       x = "Year",
       y = "Percentage") +
  theme_minimal() +
  guides(fill = guide_legend(title = "Work Cessation")) +
  theme(axis.text.x = element_text(angle = -25, hjust = 0),
        plot.title = element_text(size = 10)) +
  facet_wrap(~ factor(gender))
## age
## create a dataset for the plot later on
age_summary <- finalkfw %>%
  group_by(year, keep_from_work, age) %>%
  summarise(percentage = n()) %>%
  ungroup() %>%
  group_by(year, keep_from_work) %>%
  mutate(Cumpercentage = percentage/sum(percentage)) %>%
  ungroup() %>%
  mutate(keep_from_work = factor(ifelse(keep_from_work == 0, "0
(No)", "1 (Yes)"))) %>%
  mutate(age = case_when(
    age == 1 ~ "65-69",
    age == 2 ~ "70-74",
    age == 3 ~ "75-79",
    age == 4 ~ "80-84",
    age == 5 ~ "85-89",
    age == 6 ~ "90+",
  ),
  year = case_when(
    year == 5~2015,
    year == 6~2016,
    year == 7~2017,
    year == 8~2018,
  ))
## create a percentage bar plot
ggplot(age_summary, aes(x = factor(year),
                        y = Cumpercentage, fill = keep_from_work))
+
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Percentage of Work Cessation Across the Four Years
Based on Respondents' Ages",
       x = "Year",
       y = "Percentage") +
  theme_minimal() +
  guides(fill = guide_legend(title = "Work Cessation")) +
  theme(axis.text.x = element_text(angle = -25, hjust = 0),
        plot.title = element_text(size = 10)) +
  facet_wrap(~ factor(age))
# Model Fitting
## linear regression
full_model <- lm(keep_from_work ~ factor(gender) + factor(race) +
                   factor(age),
                 data = finalkfw)
## find the best model by stepwise backward elimination
backward_model <- step(full_model, direction = "backward")
summary(backward_model)
## get the gvif of the remaining covariates in the best model
vif(backward_model)
## drop variables with colinearity and get the BIC score
backward_model2 = lm(keep_from_work ~ factor(race) + factor(age),
                     data = finalkfw)
BIC(backward_model2)
# trouble-shooting the dataset because we have the same QIC scores
finalkfw$id <- as.character(finalkfw$id)
class(finalkfw$id)
finalkfw_id <- finalkfw %>%
  group_by(id) %>%
  mutate(id2 = cur_group_id()) %>%
  mutate(size = n()) %>%
  filter(size == 4) %>%
  ungroup() %>%
  arrange(id2, year)
## marginal model
## independent var-cov matrix
mod_indep = geeglm(keep_from_work ~ factor(race) + factor(age),
                   id = id2,
                   waves = year,
                   family = binomial("logit"),
                   data = finalkfw_id,
                   corstr = 'independence')
summary(mod_indep)
QIC(mod_indep)
## exchangeable var-cov matrix
mod_exch = geeglm(keep_from_work ~ factor(race) + factor(age),
                  id = id2,
                  waves = year,
                  family = binomial("logit"),
                  data = finalkfw_id,
                  corstr = 'exchangeable')
summary(mod_exch)
QIC(mod_exch)
## AR(1) var-cov matrix
mod_ar1 = geeglm(keep_from_work ~ factor(race) + factor(age),
                 id = id,
                 waves = year,
                 family = binomial("logit"),
                 data = finalkfw,
                 corstr = 'ar1')
summary(mod_ar1)
QIC(mod_ar1)
## create a table for the three QICs
qic_tab <- data.frame("gee model-independence" = QIC(mod_indep), "gee
model-exchangeable" = QIC(mod_exch), "gee model-AR(1)" =
                        QIC(mod_ar1))
qic_tab
# generating a contingency table
final_distinct <- finalkfw %>%
  distinct(id, .keep_all = TRUE)

counts <- finalkfw %>%
  group_by(race, age, keep_from_work, year) %>%
  summarise(count = n()) %>%
  tibble()
counts[, 1] <- ifelse(counts[, 1] == 1, "1 (White, non-Hispanic)",
                      ifelse(counts[, 1] == 2, "2 (Black, non-Hispanic)",
                             ifelse(counts[, 1] == 3, "3 (Other)", "4 (Hispanic)")))
counts[, 2] <- ifelse(counts[, 2] == 1, "1 (65-69)", 
                      ifelse(counts[, 2] == 2, "2 (70-74)", 
                             ifelse(counts[, 2] == 3, "3 (75-79)", 
                             ifelse(counts[, 2] == 4, "4 (80-84)", 
                             ifelse(counts[, 2] == 5, "5
(85-89)", "6 (90+)")))))
counts[, 3] <- ifelse(counts[, 3] == 0, "0 (not kept from work)", "1
(kept from work)")
write_csv(counts, "C:/Users/cwcheng/Dropbox (University of
Michigan)/Chia/687/project/counts1.csv")
