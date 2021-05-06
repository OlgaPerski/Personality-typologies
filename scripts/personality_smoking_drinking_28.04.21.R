library(foreign)
library(car)
library(tidyverse)
library(arsenal)
library(ggplot2)
library(interactions)
library(gtsummary)
library(here)
library(flextable)

source(here("scripts", "variable_names.r"))

if(!file.exists(here("data","personality_data.rds"))) {
  data <- read.spss(here("data","Personality BBC data file.sav"), to.data.frame=TRUE)
  data<-data %>%
    select(!!! variables)
  
  write_rds(data, path=here("data","personality_data.rds"))
}

data_raw <- read_rds(here("data","personality_data.rds"))

# recode country, age and education variables --------------------------------------------

include_country <- c("United Kingdom", "United States", "Ireland", "Canada", "Australia", "India", "New Zealand",
                     "The Netherlands")

names(include_country) <- c("GB", "US", "IE", "CA", "AU", "IN", "NZ", "NL")

data <- data_raw %>%
  mutate(country = case_when(country %in% names(include_country) ~ as.character(country),
                             country == "  " ~ "NA",
                             TRUE ~ "Other"),
         country = recode(country, !!!include_country)) %>%
  mutate(country = as.factor(country))

data <- data %>%
  mutate(age_clean = as.factor(case_when(
    is.na(age) ~ 0,
    age >=16 & age<=100 ~ 1,
    TRUE ~ 0)))

data <- data %>%
  mutate(education_clean = as.factor(case_when(
    is.na(edu)&is.na(c_edu) ~ 0,
    edu==1 ~ 1,
    edu==2 ~ 2,
    edu==3 ~ 3,
    edu==4 ~ 4,
    edu==5 ~ 5,
    edu==6 ~ 6,
    edu==7&is.na(c_edu) ~ 0,
    edu==7&c_edu==1 ~ 1,
    edu==7&c_edu==2 ~ 2,
    edu==7&c_edu==3 ~ 3,
    edu==7&c_edu==4 ~ 4,
    edu==7&c_edu==5 ~ 5,
    edu==7&c_edu==6 ~ 6)),
    education_clean = forcats::fct_collapse(education_clean, "NA" = "0", "No post-16 qualifications" = c("1", "2"), 
                                            "Post-16 qualifications" = c("3", "4"), "Higher education" = c("5", "6")))

# recode smoking variable -------------------------------------------------

smok_labels = c("No", "Yes")

data <- data %>%
  mutate(smok1 = as.factor(smok1)) %>%
  mutate(smok2 = as.factor(smok2)) %>%
  mutate(smok1 = recode_factor(smok1, "1" = 0, "0" = 1)) %>% #original coding: 0 = yes; 1 = no
  mutate(smok2 = recode_factor(smok2, "0" = 0, "1" = 1, "2" = 1, "3" = 1, "4" = 1, "5" = 1, "6" = 1)) %>%
  mutate(smoking = factor(case_when(
    smok1=="0"&is.na(smok2) ~ 0, #never smoker
    smok1=="0"&smok2=="0" ~ 0, #never smoker
    smok1=="1"&smok2=="0" ~ 0, #ex-smoker
    smok1=="1"&smok2=="1" ~ 1), #current smoker
    labels = smok_labels))

# recode alcohol variable -------------------------------------------------

data <- data %>%
  mutate(alcohol = as.factor(alcohol)) %>%
  mutate(drinking = forcats::fct_collapse(alcohol, "No" = c("0", "1"), "Yes" = c("2", "3", "4", "5", "6")))

# create combined smoking/drinking variable --------------------------------

comb_labels = c("No", "Yes")

data <- data %>%
  mutate(combined = factor(case_when(
    smoking=="No"&drinking=="No" ~ 0,
    smoking=="Yes"&drinking=="No" ~ 0,
    smoking=="No"&drinking=="Yes" ~ 0,
    smoking=="Yes"&drinking=="Yes" ~ 1),
    labels = comb_labels))

# recode explanatory variables --------------------------------------------

#primary analysis - median split

data <- data %>%
  mutate(extra_median = as_factor(case_when(
    extraversion >= median(extraversion, na.rm = T) ~ "high",
    extraversion < median(extraversion, na.rm = T) ~ "low")),
    consci_median = as_factor(case_when(
      conscientiousness >= median(conscientiousness, na.rm = T) ~ "high", 
      conscientiousness < median(conscientiousness, na.rm = T) ~ "low")),
    neuroti_median = as_factor(case_when(
      neuroticism >= median(neuroticism, na.rm = T) ~ "high",
      neuroticism < median(neuroticism, na.rm = T) ~ "low")))

#sensitivity analysis - 1 SD above the mean vs everyone else

data <- data %>%
  mutate(extra_sd = as_factor(case_when(
    extraversion >= (mean(extraversion, na.rm = T) + sd(extraversion, na.rm = T)) ~ "high", 
    extraversion < (mean(extraversion, na.rm = T) + sd(extraversion, na.rm = T)) ~ "low")),
    consci_sd = as_factor(case_when(
      conscientiousness <= (mean(conscientiousness, na.rm = T) - sd(conscientiousness, na.rm = T)) ~ "low",
      conscientiousness > (mean(conscientiousness, na.rm = T) - sd(conscientiousness, na.rm = T)) ~ "high")),
    neuroti_sd = as_factor(case_when(
      neuroticism >= (mean(neuroticism, na.rm = T) + sd(neuroticism, na.rm = T)) ~ "high",
      neuroticism < (mean(neuroticism, na.rm = T) + sd(neuroticism, na.rm = T)) ~ "low")))

#sensitivity analysis - 1 SD above the mean vs 1 SD below the mean

data <- data %>%
  mutate(extra_xtremes = as_factor(case_when(
    extraversion >= (mean(extraversion, na.rm = T) + sd(extraversion, na.rm = T)) ~ "high", 
    extraversion < (mean(extraversion, na.rm = T) - sd(extraversion, na.rm = T)) ~ "low")),
    consci_xtremes = as_factor(case_when(
      conscientiousness <= (mean(conscientiousness, na.rm = T) - sd(conscientiousness, na.rm = T)) ~ "low",
      conscientiousness > (mean(conscientiousness, na.rm = T) + sd(conscientiousness, na.rm = T)) ~ "high")),
    neuroti_xtremes = as_factor(case_when(
      neuroticism >= (mean(neuroticism, na.rm = T) + sd(neuroticism, na.rm = T)) ~ "high",
      neuroticism < (mean(neuroticism, na.rm = T) - sd(neuroticism, na.rm = T)) ~ "low")))

# end of data cleaning ----------------------------------------------------

data <- data %>%
  select(-c("edu", "c_edu", "extraversion", "conscientiousness", "neuroticism", "smok1", "smok2", "alcohol"))

data <- data %>%
  mutate(eligible = as_factor(case_when(
    country == "NA" | is.na(sex) | age_clean == 0 | education_clean == "NA" | is.na(smoking) | is.na(drinking) | is.na(extra_median) | is.na(consci_median) |
      is.na(neuroti_median) ~ "exclude",
    TRUE ~ "include")))

#create table 1

t1 <- tbl_summary(data, by = eligible) %>%
  bold_labels() %>%
  add_overall() %>%
  as_flex_table() %>%
  save_as_docx(path = here("output","table1.docx"))

# main analyses -----------------------------------------------------------

data <- data %>%
  filter(eligible == "include")

data$eligible <- droplevels(data$eligible)
data$education_clean <- droplevels(data$education_clean)
data$country <- droplevels(data$country)

#set correct referents for outcome variables, explanatory variables and covariates

data$smoking <- relevel(data$smoking, ref="No")
data$drinking <- relevel(data$drinking, ref="No")
data$combined <- relevel(data$combined, ref="No")

data$extra_median <- relevel(data$extra_median, ref="low")
data$consci_median <- relevel(data$consci_median, ref="high")
data$neuroti_median <- relevel(data$neuroti_median, ref="low")
data$extra_sd <- relevel(data$extra_sd, ref="low")
data$consci_sd <- relevel(data$consci_sd, ref="high")
data$neuroti_sd <- relevel(data$neuroti_sd, ref="low")
data$extra_xtremes <- relevel(data$extra_xtremes, ref="low")
data$consci_xtremes <- relevel(data$consci_xtremes, ref="high")
data$neuroti_xtremes <- relevel(data$neuroti_xtremes, ref="low")

data$sex <- relevel(data$sex, ref="male")
data$education_clean <- relevel(data$education_clean, ref="No post-16 qualifications")
data$country <- relevel(data$country, ref="United Kingdom")

#smoking

m1<-glm(smoking~extra_median+consci_median+neuroti_median+extra_median*consci_median+extra_median*neuroti_median+consci_median*neuroti_median+
          extra_median*consci_median*neuroti_median,family=binomial(link="logit"),data=data,na.action=na.omit)

t1 <- tbl_regression(m1, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

m1a<-glm(smoking~extra_median+consci_median+neuroti_median+extra_median*consci_median+extra_median*neuroti_median+consci_median*neuroti_median+
           extra_median*consci_median*neuroti_median+age+sex+education_clean+country,family=binomial(link="logit"),data=data,na.action=na.omit)

t1_a <- tbl_regression(m1a, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

#drinking

m2<-glm(drinking~extra_median+consci_median+neuroti_median+extra_median*consci_median+extra_median*neuroti_median+consci_median*neuroti_median+
          extra_median*consci_median*neuroti_median,family=binomial(link="logit"),data=data,na.action=na.omit)

t2 <- tbl_regression(m2, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

m2a<-glm(drinking~extra_median+consci_median+neuroti_median+extra_median*consci_median+extra_median*neuroti_median+consci_median*neuroti_median+
           extra_median*consci_median*neuroti_median+age+sex+education_clean+country,family=binomial(link="logit"),data=data,na.action=na.omit)

t2_a <- tbl_regression(m2a, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

#combined

m3<-glm(combined~extra_median+consci_median+neuroti_median+extra_median*consci_median+extra_median*neuroti_median+consci_median*neuroti_median+
          extra_median*consci_median*neuroti_median,family=binomial(link="logit"),data=data,na.action=na.omit)

t3 <- tbl_regression(m3, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

m3a<-glm(combined~extra_median+consci_median+neuroti_median+extra_median*consci_median+extra_median*neuroti_median+consci_median*neuroti_median+
           extra_median*consci_median*neuroti_median+age+sex+education_clean+country,family=binomial(link="logit"),data=data,na.action=na.omit)

t3_a <- tbl_regression(m3a, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

# create table 2 ----------------------------------------------------------

tbl_merge_1 <-
  tbl_merge(
    tbls = list(t1, t2, t3),
    tab_spanner = c("**Smoking**", "**Alcohol**", "**Combined**"))

tbl_merge_2 <-
  tbl_merge(
    tbls = list(t1_a, t2_a, t3_a),
    tab_spanner = c("**Smoking**", "**Alcohol**", "**Combined**"))

tbl_stack <- tbl_stack(list(tbl_merge_1,tbl_merge_2)) %>%
  as_flex_table() %>%
  save_as_docx(path = here("output","table2.docx"))

# nrs with outcome for table 2 --------------------------------------------

#smoking

xtabs(~smoking+extra_median,data=data)
xtabs(~smoking+consci_median,data=data)
xtabs(~smoking+neuroti_median,data=data)

xtabs(~smoking+extra_median+consci_median,data=data)
xtabs(~smoking+extra_median+neuroti_median,data=data)
xtabs(~smoking+consci_median+neuroti_median,data=data)

xtabs(~smoking+extra_median+consci_median+neuroti_median,data=data)

#drinking

xtabs(~drinking+extra_median,data=data)
xtabs(~drinking+consci_median,data=data)
xtabs(~drinking+neuroti_median,data=data)

xtabs(~drinking+extra_median+consci_median,data=data)
xtabs(~drinking+extra_median+neuroti_median,data=data)
xtabs(~drinking+consci_median+neuroti_median,data=data)

xtabs(~drinking+extra_median+consci_median+neuroti_median,data=data)

#combined

xtabs(~combined+extra_median,data=data)
xtabs(~combined+consci_median,data=data)
xtabs(~combined+neuroti_median,data=data)

xtabs(~combined+extra_median+consci_median,data=data)
xtabs(~combined+extra_median+neuroti_median,data=data)
xtabs(~combined+consci_median+neuroti_median,data=data)

xtabs(~combined+extra_median+consci_median+neuroti_median,data=data)

# stratified results for significant interactions -------------------------

#low conscientiousness

stratified <- data %>%
  filter(consci_median=="low")

e1 <- glm(smoking~extra_median+age+sex+education_clean+country,family=binomial(link="logit"),data=stratified,na.action=na.omit)

t_e1 <- tbl_regression(e1, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

e2 <- glm(drinking~extra_median+age+sex+education_clean+country,family=binomial(link="logit"),data=stratified,na.action=na.omit)

t_e2 <- tbl_regression(e2, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

e3 <- glm(combined~extra_median+age+sex+education_clean+country,family=binomial(link="logit"),data=stratified,na.action=na.omit)

t_e3 <- tbl_regression(e3, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

tbl_merge_strt_1 <-
  tbl_merge(
    tbls = list(t_e1, t_e2, t_e3),
    tab_spanner = c("**Smoking**", "**Alcohol**", "**Combined**"))

#high conscientiousness

stratified_h <- data %>%
  filter(consci_median=="high")

e1_h <- glm(smoking~extra_median+age+sex+education_clean+country,family=binomial(link="logit"),data=stratified_h,na.action=na.omit)

t_e1_h <- tbl_regression(e1_h, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

e2_h <- glm(drinking~extra_median+age+sex+education_clean+country,family=binomial(link="logit"),data=stratified_h,na.action=na.omit)

t_e2_h <- tbl_regression(e2_h, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

e3_h <- glm(combined~extra_median+age+sex+education_clean+country,family=binomial(link="logit"),data=stratified_h,na.action=na.omit)

t_e3_h <- tbl_regression(e3_h, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

tbl_merge_strt_2 <-
  tbl_merge(
    tbls = list(t_e1_h, t_e2_h, t_e3_h),
    tab_spanner = c("**Smoking**", "**Alcohol**", "**Combined**")) 

tbl_stack_stratified <- tbl_stack(list(tbl_merge_strt_1,tbl_merge_strt_2)) %>%
  as_flex_table() %>%
  save_as_docx(path = here("output","stratified_table.docx"))

# plots for two-way interactions -----------------------------------

plot_colors <- c("darkgreen", "darkblue")

smok_plot <- cat_plot(m1a, pred=extra_median, modx=consci_median, data=data, geom="line", colors = plot_colors, outcome.scale="response",
                      interval = TRUE, x.label = 'Extraversion', y.label = 'Smoking',
                      legend.main = 'Conscientiousness', pred.labels = c("Low", "High"), modx.labels = c("High", "Low"))

drink_plot <- cat_plot(m2a, pred=extra_median, modx=consci_median, data=data, geom="line", colors = plot_colors, outcome.scale="response",
                       interval = TRUE, x.label = 'Extraversion', y.label = 'Excessive drinking',
                       legend.main = 'Conscientiousness', pred.labels = c("Low", "High"), modx.labels = c("High", "Low"))

comb_plot <- cat_plot(m3a, pred=extra_median, modx=consci_median, data=data, geom="line", colors = plot_colors, outcome.scale="response",
                      interval = TRUE, x.label = 'Extraversion', y.label = 'Combined',
                      legend.main = 'Conscientiousness', pred.labels = c("Low", "High"), modx.labels = c("High", "Low"))

# sensitivity analyses with different operationalisations of personality --------

#1 SD above the mean compared with everyone else

#smoking

s1_1 <-glm(smoking~extra_sd+consci_sd+neuroti_sd+extra_sd*consci_sd+extra_sd*neuroti_sd+consci_sd*neuroti_sd+
           extra_sd*consci_sd*neuroti_sd,family=binomial(link="logit"),data=data,na.action=na.omit)

t_s1_1 <- tbl_regression(s1_1, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

s1_1a<-glm(smoking~extra_sd+consci_sd+neuroti_sd+extra_sd*consci_sd+extra_sd*neuroti_sd+consci_sd*neuroti_sd+
            extra_sd*consci_sd*neuroti_sd+age+sex+education_clean+country,family=binomial(link="logit"),data=data,na.action=na.omit)

t_s1_1a <- tbl_regression(s1_1a, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

#drinking

s1_2<-glm(drinking~extra_sd+consci_sd+neuroti_sd+extra_sd*consci_sd+extra_sd*neuroti_sd+consci_sd*neuroti_sd+
           extra_sd*consci_sd*neuroti_sd,family=binomial(link="logit"),data=data,na.action=na.omit)

t_s1_2 <- tbl_regression(s1_2, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

s1_2a<-glm(drinking~extra_sd+consci_sd+neuroti_sd+extra_sd*consci_sd+extra_sd*neuroti_sd+consci_sd*neuroti_sd+
            extra_sd*consci_sd*neuroti_sd+age+sex+education_clean+country,family=binomial(link="logit"),data=data,na.action=na.omit)

t_s1_2a <- tbl_regression(s1_2a, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

#combined

s1_3<-glm(combined~extra_sd+consci_sd+neuroti_sd+extra_sd*consci_sd+extra_sd*neuroti_sd+consci_sd*neuroti_sd+
           extra_sd*consci_sd*neuroti_sd,family=binomial(link="logit"),data=data,na.action=na.omit)

t_s1_3 <- tbl_regression(s1_3, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

s1_3a<-glm(combined~extra_sd+consci_sd+neuroti_sd+extra_sd*consci_sd+extra_sd*neuroti_sd+consci_sd*neuroti_sd+
            extra_sd*consci_sd*neuroti_sd+age+sex+education_clean+country,family=binomial(link="logit"),data=data,na.action=na.omit)

t_s1_3a <- tbl_regression(s1_3a, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

# create supplementary table 1 --------------------------------------------

tbl_merge_s1_1 <-
  tbl_merge(
    tbls = list(t_s1_1, t_s1_2, t_s1_3),
    tab_spanner = c("**Smoking**", "**Alcohol**", "**Combined**"))

tbl_merge_s1_2 <-
  tbl_merge(
    tbls = list(t_s1_1a, t_s1_2a, t_s1_3a),
    tab_spanner = c("**Smoking**", "**Alcohol**", "**Combined**"))

tbl_stack_s1 <- tbl_stack(list(tbl_merge_s1_1,tbl_merge_s1_2)) %>%
  as_flex_table() %>%
  save_as_docx(path = here("output","supp_table1.docx"))

#1 SD above the mean compared with 1 SD below the mean

#smoking

s2_1<-glm(smoking~extra_xtremes+consci_xtremes+neuroti_xtremes+extra_xtremes*consci_xtremes+extra_xtremes*neuroti_xtremes+consci_xtremes*neuroti_xtremes+
           extra_xtremes*consci_xtremes*neuroti_xtremes,family=binomial(link="logit"),data=data,na.action=na.omit)

t_s2_1 <- tbl_regression(s2_1, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

s2_1a<-glm(smoking~extra_xtremes+consci_xtremes+neuroti_xtremes+extra_xtremes*consci_xtremes+extra_xtremes*neuroti_xtremes+consci_xtremes*neuroti_xtremes+
            extra_xtremes*consci_xtremes*neuroti_xtremes+age+sex+education_clean+country,family=binomial(link="logit"),data=data,na.action=na.omit)

t_s2_1a <- tbl_regression(s2_1a, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

#drinking

s2_2<-glm(drinking~extra_xtremes+consci_xtremes+neuroti_xtremes+extra_xtremes*consci_xtremes+extra_xtremes*neuroti_xtremes+consci_xtremes*neuroti_xtremes+
           extra_xtremes*consci_xtremes*neuroti_xtremes,family=binomial(link="logit"),data=data,na.action=na.omit)

t_s2_2 <- tbl_regression(s2_2, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

s2_2a<-glm(drinking~extra_xtremes+consci_xtremes+neuroti_xtremes+extra_xtremes*consci_xtremes+extra_xtremes*neuroti_xtremes+consci_xtremes*neuroti_xtremes+
            extra_xtremes*consci_xtremes*neuroti_xtremes+age+sex+education_clean+country,family=binomial(link="logit"),data=data,na.action=na.omit)

t_s2_2a <- tbl_regression(s2_2a, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

#combined

s2_3<-glm(combined~extra_xtremes+consci_xtremes+neuroti_xtremes+extra_xtremes*consci_xtremes+extra_xtremes*neuroti_xtremes+consci_xtremes*neuroti_xtremes+
           extra_xtremes*consci_xtremes*neuroti_xtremes,family=binomial(link="logit"),data=data,na.action=na.omit)

t_s2_3 <- tbl_regression(s2_3, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

s2_3a<-glm(combined~extra_xtremes+consci_xtremes+neuroti_xtremes+extra_xtremes*consci_xtremes+extra_xtremes*neuroti_xtremes+consci_xtremes*neuroti_xtremes+
            extra_xtremes*consci_xtremes*neuroti_xtremes+age+sex+education_clean+country,family=binomial(link="logit"),data=data,na.action=na.omit)

t_s2_3a <- tbl_regression(s2_3a, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

# create supplementary table 2 ----------------------------------------------

tbl_merge_s2_1 <-
  tbl_merge(
    tbls = list(t_s2_1, t_s2_2, t_s2_3),
    tab_spanner = c("**Smoking**", "**Alcohol**", "**Combined**"))

tbl_merge_s2_2 <-
  tbl_merge(
    tbls = list(t_s2_1a, t_s2_2a, t_s2_3a),
    tab_spanner = c("**Smoking**", "**Alcohol**", "**Combined**"))

tbl_stack_s2 <- tbl_stack(list(tbl_merge_s2_1,tbl_merge_s2_2)) %>%
  as_flex_table() %>%
  save_as_docx(path = here("output","supp_table2.docx"))

# exploring sig. three-way interaction for excessive drinking ----------------------

#high neuroticism

data1 <- data %>%
  filter(neuroti_xtremes=="high")

s1 <- glm(drinking~extra_xtremes+consci_xtremes+extra_xtremes*consci_xtremes+age+sex+education_clean+country,family=binomial(link="logit"),data=data1,na.action=na.omit)

t_s1 <- tbl_regression(s1, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

#high neuroticism and high extraversion

data1a <- data %>%
  filter(neuroti_xtremes=="high"&extra_xtremes=="high")

s1a <- glm(drinking~consci_xtremes+age+sex+education_clean+country,family=binomial(link="logit"),data=data1a,na.action=na.omit)

t_s1a <- tbl_regression(s1a, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

#high neuroticism and low extraversion

data1b <- data %>%
  filter(neuroti_xtremes=="high"&extra_xtremes=="low")

s1b <- glm(drinking~consci_xtremes+age+sex+education_clean+country,family=binomial(link="logit"),data=data1b,na.action=na.omit)

t_s1b <- tbl_regression(s1b, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

#low neuroticism

data2 <- data %>%
  filter(neuroti_xtremes=="low")

s2 <- glm(drinking~extra_xtremes+consci_xtremes+extra_xtremes*consci_xtremes+age+sex+education_clean+country,family=binomial(link="logit"),data=data2,na.action=na.omit)

t_s2 <- tbl_regression(s2, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

#low neuroticism and high extraversion

data1c <- data %>%
  filter(neuroti_xtremes=="low"&extra_xtremes=="high")

s1c <- glm(drinking~consci_xtremes+age+sex+education_clean+country,family=binomial(link="logit"),data=data1c,na.action=na.omit)

t_s1c <- tbl_regression(s1c, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

#low neuroticism and low extraversion

data1d <- data %>%
  filter(neuroti_xtremes=="low"&extra_xtremes=="low")

s1d <- glm(drinking~consci_xtremes+age+sex+education_clean+country,family=binomial(link="logit"),data=data1d,na.action=na.omit)

t_s1d <- tbl_regression(s1d, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

tbl_merge_sa <-
  tbl_merge(
    tbls = list(t_s1a, t_s1b, t_s1c, t_s1d),
    tab_spanner = c("**high/high**", "**high/low**", "**low/high**", "**low/low**"))