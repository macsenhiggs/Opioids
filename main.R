library(tidyverse)
library(ggplot2)
library(finalfit)

#REASON: Reason for Discharge from Clinic
#SUB1: primary substance admitted for
#METHUSE: opioid therapy used in treatment

data_main = tedsd_puf_2023 |>
  mutate(POS_OUTCOME = ifelse(REASON == 1, 1,
                              ifelse(REASON %in% c(2,3), 0, NA)))

prop.table(table(data_main$POS_OUTCOME, useNA = "ifany"))

data_cleaned = 







