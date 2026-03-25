library(tidyverse)
library(ggplot2)
library(finalfit)
library(forcats)
library(readr)

#TODO:
#CREATE VARIABLE FOR DIFFERENCE IN SELF HELP ATTENDANCE ON ADMISSION AND DISCHARGE
#CREATE VARIABLE FOR YEARS OF USAGE

tedsd_puf_2023 <- read_csv("TEDSD 2017 to 2023/tedsd_puf_2023.csv")

data_main = tedsd_puf_2023 |>
  mutate(SUCCESS = ifelse(REASON == 1, 1,
                          ifelse(REASON %in% c(2,3), 0, NA)))

filter_cols <- c("SUB1", "SUB2", "SUB3", "REASON", "HERFLG", "METHFLG", "OPSYNFLG")
demo_cols <- c("AGE", "SEX", "RACE", "ETHNIC", "EDUC", "EMPLOY", "LIVARAG", "PRIMINC", "DIVISION")
usage_cols <- c("NOPRIOR", "ROUTE1", "FREQ1", "FRSTUSE1", "ALCDRUG", "DSMCRIT", "PSYPROB", "FREQ_ATND_SELF_HELP")
treatment_cols <- c("SERVICES", "METHUSE", "LOS", "FREQ_ATND_SELF_HELP_D")
other_cols <- c("SUCCESS", "CASEID")
all_target_cols <- c(filter_cols,
                     demo_cols,
                     usage_cols,
                     treatment_cols,
                     other_cols)

data_cleaned = data_main |>
  select(any_of(all_target_cols)) |>
  filter(
    SUB1 %in% c(5,6,7),
    #SUB2 %in% c(5,6,7,1),
    #SUB3 %in% c(5,6,7,1),
    !is.na(SUCCESS),
  ) |>
  relocate(CASEID, SUCCESS)

rename_columns <- function(data_cleaned) {
  data_cleaned_renamed <- data_cleaned |>
    mutate(
      #Filtering Columns----
      SUB1 = factor(SUB1, levels = c(5, 6, 7), labels = c("Heroin", "Non-prescription methadone", "Other opiates and synthetics")),
      SUBS_USED = ifelse(SUB2 %in% 2:19 & SUB3 %in% 2:19, 3, ifelse(
        SUB2 %in% 2:19 | SUB3 %in% 2:19, 2,
        1
      )),
      SUBS_USED = as.factor(SUBS_USED),
      REASON = factor(REASON, levels = c(1, 2, 3, 4, 5, 6, 7), labels = c("Treatment Completed", "Dropped Out Of Treatment", "Terminated By Facility", "Transferred To Another Treatment Program/Facility", "Incarcerated", "Death", "Other")),
      HERFLG = factor(HERFLG, levels = c(0,1), labels = c("No", "Yes")),
      METHFLG = factor(METHFLG, levels = c(0,1), labels = c("No", "Yes")),
      OPSYNFLG = factor(OPSYNFLG, levels = c(0,1), labels = c("No", "Yes")),
      #Demographic Columns----
      AGE = factor(AGE,levels = 1:12,
                   labels = c("12–14 years old", 
                              "15–17 years old", 
                              "18–20 years old", 
                              "21–24 years old", 
                              "25–29 years old", 
                              "30–34 years old", 
                              "35–39 years old", 
                              "40–44 years old", 
                              "45–49 years old", 
                              "50–54 years old", 
                              "55–64 years old", 
                              "65 years and older")),
      SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
      RACE = factor(RACE, 
                    levels = 1:9, 
                    labels = c("Alaska Native (Aleut, Eskimo)", 
                               "American Indian (other than Alaska Native)", 
                               "Asian or Pacific Islander", 
                               "Black or African American", 
                               "White", 
                               "Asian", 
                               "Other single race", 
                               "Two or more races", 
                               "Native Hawaiian or Other Pacific Islander")),
      ETHNIC = factor(ETHNIC, 
                      levels = c(1, 2, 3, 5, 4), 
                      labels = c("Hispanic/Latino",
                                 "Hispanic/Latino",
                                 "Hispanic/Latino",
                                 "Hispanic/Latino",
                                 "Not Hispanic/Latino")),
      EDUC = factor(EDUC, 
                    levels = 1:5, 
                    labels = c("8th grade or less", 
                               "Grades 9 to 11", 
                               "Grade 12 (or GED)", 
                               "1–3 years of college/vocational", 
                               "4 years of college (BA/BS) or more")),
      EMPLOY = factor(EMPLOY, 
                      levels = 1:4, 
                      labels = c("Full-time", 
                                 "Part-time", 
                                 "Unemployed", 
                                 "Not in labor force")),
      LIVARAG = factor(LIVARAG, 
                       levels = 1:3, 
                       labels = c("Experiencing homelessness", 
                                  "Dependent living", 
                                  "Independent living")),
      PRIMINC = factor(PRIMINC, 
                       levels = 1:5, 
                       labels = c("Wages/salary", 
                                  "Public assistance", 
                                  "Retirement/pension, disability", 
                                  "Other", 
                                  "None")),
      DIVISION = factor(DIVISION,
                        levels = 0:9,
                        labels = c("U.S. territories",
                                   "New England",
                                   "Middle Atlantic",
                                   "East North Central",
                                   "West North Central",
                                   "South Atlantic",
                                   "East South Central",
                                   "West South Central",
                                   "Mountain",
                                   "Pacific")),
      #Usage Columns----
      NOPRIOR = factor(NOPRIOR, levels = 0:1, labels = c("No prior treatment episode", "One or more prior treatment episodes")),
      ROUTE1 = factor(ROUTE1, 
                      levels = 1:5, 
                      labels = c("Oral", 
                                 "Smoking", 
                                 "Inhalation", 
                                 "Injection", 
                                 "Other")),
      FREQ1 = factor(FREQ1, 
                     levels = 1:3, 
                     labels = c("No use in the past month", 
                                "Some use", 
                                "Daily use")),
      FRSTUSE1 = factor(FRSTUSE1, 
                        levels = 1:7, 
                        labels = c("11 years and under", 
                                   "12–14 years", 
                                   "15–17 years", 
                                   "18–20 years", 
                                   "21–24 years", 
                                   "25–29 years", 
                                   "30 years and older")),
      ALCDRUG = factor(ALCDRUG, 
                       levels = 0:3, 
                       labels = c("None", 
                                  "Alcohol only", 
                                  "Other drugs only", 
                                  "Alcohol and other drugs")),
      DSMCRIT = factor(DSMCRIT, 
                       levels = 1:19, 
                       labels = c("Alcohol-induced disorder", 
                                  "Substance-induced disorder", 
                                  "Alcohol intoxication", 
                                  "Alcohol dependence", 
                                  "Opioid dependence", 
                                  "Cocaine dependence", 
                                  "Cannabis dependence", 
                                  "Other substance dependence", 
                                  "Alcohol abuse", 
                                  "Cannabis abuse", 
                                  "Other substance abuse", 
                                  "Opioid abuse", 
                                  "Cocaine abuse", 
                                  "Anxiety disorders", 
                                  "Depressive disorders", 
                                  "Schizophrenia/other psychotic disorders", 
                                  "Bipolar disorders", 
                                  "Attention deficit/disruptive behavior disorders", 
                                  "Other mental health condition")),
      PSYPROB = factor(PSYPROB, levels = 1:2, labels = c("Yes", "No")),
      #Treatment columns----
      SERVICES = factor(SERVICES, 
                        levels = 1:8, 
                        labels = c("Detox, 24-hr, hospital inpatient", 
                                   "Detox, 24-hr, free-standing residential", 
                                   "Rehab/residential, hospital (non-detox)", 
                                   "Rehab/residential, short term (<= 30 days)", 
                                   "Rehab/residential, long term (> 30 days)", 
                                   "Ambulatory, intensive outpatient", 
                                   "Ambulatory, non-intensive outpatient",
                                   "Ambulatory, detox")),
      METHUSE = factor(METHUSE, levels = 1:2, labels = c("Yes", "No")),
      LOS = factor(LOS, 
                   levels = 1:37, 
                   labels = c(as.character(1:30), 
                              "31 to 45 days", 
                              "46 to 60 days", 
                              "61 to 90 days", 
                              "91 to 120 days", 
                              "121 to 180 days", 
                              "181 to 365 days", 
                              "More than a year")),
      FREQ_ATND_SELF_HELP = factor(FREQ_ATND_SELF_HELP, 
                                   levels = 1:5, 
                                   labels = c("No attendance", 
                                              "1–3 times in the past month", 
                                              "4–7 times in the past month", 
                                              "8–30 times in the past month", 
                                              "Some attendance, frequency unknown")),
      FREQ_ATND_SELF_HELP_D = factor(FREQ_ATND_SELF_HELP_D, 
                                     levels = 1:5, 
                                     labels = c("No attendance", 
                                                "1–3 times in the past month", 
                                                "4–7 times in the past month", 
                                                "8–30 times in the past month", 
                                                "Some attendance, frequency unknown")),
    ) |>
    #Other modifications----
  select(-SUB2, -SUB3) |>
    relocate(SUBS_USED, .after = SUB1)
  
  
  #remove all NA values----
  data_cleaned_renamed <- data_cleaned_renamed |>
    mutate(across(where(is.factor), ~fct_na_value_to_level(.x, level = "NA")))
  
  return(data_cleaned_renamed)
}

data_cleaned_renamed <- rename_columns(data_cleaned)

#create GLM----
str(data_cleaned_renamed)

circumstances_glm <-
  glm(SUCCESS ~ SUB1 + SUBS_USED + AGE + SEX + RACE + ETHNIC + EDUC +
        EMPLOY + LIVARAG + PRIMINC + DIVISION + NOPRIOR + ROUTE1 + FREQ1 +
        FRSTUSE1 + ALCDRUG + DSMCRIT + PSYPROB + FREQ_ATND_SELF_HELP,
      data = data_cleaned_renamed,
      family = "binomial")

summary(circumstances_glm)

clean_data_with_regression <- data_cleaned_renamed |>
  mutate(
    EXP_SUCCESS = predict(circumstances_glm, type = "resp"),
    SUCCESS_OE = SUCCESS - EXP_SUCCESS
  )



