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

filter_cols <- c("SUB1", "SUB2", "SUB3", "REASON", "HERFLG", "METHFLG", "OPSYNFLG")
demo_cols <- c("AGE", "SEX", "RACE", "ETHNIC", "MARSTAT", "EDUC", "EMPLOY", "DETNLF", "PREG", "LIVARAG", "PRIMINC", "STFIPS")
usage_cols <- c("NOPRIOR", "ROUTE1", "FREQ1", "FRSTUSE1", "ALCDRUG", "DSMCRIT", "PSYPROB")
treatment_cols <- c("SERVICES", "METHUSE", "LOS", "FREQ_ATND_SELF_HELP_D", "FREQ_ATND_SELF_HELP")
other_cols <- c("POS_OUTCOME", "CASEID")
all_target_cols <- c(filter_cols, demo_cols, usage_cols, treatment_cols, other_cols)

data_cleaned = data_main |>
  select(any_of(all_target_cols)) |>
  filter(
    SUB1 %in% c(5,6,7),
    #SUB2 == 1, #no second substance?
    #SUB3 == 1, #no third substance?
    !is.na(POS_OUTCOME),
  ) |>
  mutate(
    
  )

table(data_cleaned$HERFLG, data_cleaned$METHFLG, data_cleaned$OPSYNFLG)





