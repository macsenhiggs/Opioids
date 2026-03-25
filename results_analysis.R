library(tidyverse)
library(ggplot2)

#treatment_cols <- c("SERVICES", "METHUSE", "LOS", "FREQ_ATND_SELF_HELP_D")

SERVICES_success <- clean_data_with_regression |>
  group_by(SERVICES) |>
  summarize(
    SUCCESS_RATE = mean(SUCCESS),
    EXPECTED_SUCCESS_RATE = mean(EXP_SUCCESS),
    SUCCESS_RATE_OE = mean(SUCCESS_OE),
    n = n()
  )

METHUSE_success <- clean_data_with_regression |>
  group_by(METHUSE) |>
  summarize(
    SUCCESS_RATE = mean(SUCCESS),
    EXPECTED_SUCCESS_RATE = mean(EXP_SUCCESS),
    SUCCESS_RATE_OE = mean(SUCCESS_OE),
    n = n()
  )

LOS_success <- clean_data_with_regression |>
  group_by(LOS) |>
  summarize(
    SUCCESS_RATE = mean(SUCCESS),
    EXPECTED_SUCCESS_RATE = mean(EXP_SUCCESS),
    SUCCESS_RATE_OE = mean(SUCCESS_OE),
    n = n()
  ) |>
  mutate(LOS_INDEX = row_number())
ggplot(LOS_success, aes(x = LOS_INDEX, y = SUCCESS_RATE_OE)) +
  geom_path() +
  geom_vline(xintercept = 30, linetype = "dashed") +
  geom_point(aes(size = n)) +
  theme_minimal()


FREQ_ATND_SELF_HELP_D_success <- clean_data_with_regression |>
  group_by(FREQ_ATND_SELF_HELP_D) |>
  summarize(
    SUCCESS_RATE = mean(SUCCESS),
    EXPECTED_SUCCESS_RATE = mean(EXP_SUCCESS),
    SUCCESS_RATE_OE = mean(SUCCESS_OE),
    n = n()
  )

ALL_success <- clean_data_with_regression |>
  group_by(SERVICES, METHUSE, LOS, FREQ_ATND_SELF_HELP_D) |>
  summarize(
    SUCCESS_RATE = mean(SUCCESS),
    EXPECTED_SUCCESS_RATE = mean(EXP_SUCCESS),
    SUCCESS_RATE_OE = mean(SUCCESS_OE),
    n = n()
  ) |>
  filter(n > 100)