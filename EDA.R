library(tidyverse)
library(ggplot2)
library(finalfit)

#REASON: Reason for Discharge from Clinic
#SUB1: primary substance admitted for
#METHUSE: opioid therapy used in treatment

opioid_therapy_success_by_substance <- tedsd_puf_2023 |>
  filter(
    METHUSE == 1,
    SUB1 != -9
  ) |>
  mutate(
    POS_OUTCOME = ifelse(REASON == 1, 1,
                         ifelse(REASON %in% c(2,3), 0, NA))
  ) |>
  group_by(SUB1) |>
  summarize(
    opioid_therapy_success_rate = mean(POS_OUTCOME, na.rm = TRUE),
    n = n()
  )

opiod_therapy_on_off <- tedsd_puf_2023 |>
  filter(
    SUB1 %in% c(5,7),
    METHUSE != -9
  ) |>
  mutate(
    POS_OUTCOME = ifelse(REASON == 1, 1,
                         ifelse(REASON %in% c(2,3), 0, NA))
  ) |>
  group_by(SUB1, METHUSE) |>
  summarize (
    success_rate = mean(POS_OUTCOME, na.rm = TRUE),
    n = n()
  )

