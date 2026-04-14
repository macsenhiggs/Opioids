library(ggeffects)
library(broom)
library(ggplot2)
library(dplyr)
library(purrr)

if (!exists("circumstances_glm")) {
  stop("ERROR: GET GLM FROM glm_creation.R BEFORE PERFORMING CI ANALYSIS")
}

# Calculate adjusted probabilities for SUB1
prob_cols <- ggpredict(circumstances_glm)

all_probs_df <- prob_cols %>%
  map_df(~ as.data.frame(.x), .id = "predictor_name") |>
  filter (x != "NA")

mean_vals <- all_probs_df |>
  group_by(group) |>
  summarize(
    mean_predicted = mean(predicted, na.rm = TRUE)
  )

all_probs_mean_diff <- all_probs_df |>
  left_join(mean_vals, by = "group") |>
  mutate(group_mean_diff = predicted - mean_predicted) |>
  #format(group_mean_diff = group_mean_diff, scientific = FALSE) |>
  select(-c(std.error, conf.low, conf.high, group)) |>
  arrange(desc(group_mean_diff))

head(all_probs_mean_diff, 5)
tail(all_probs_mean_diff, 5)

fwrite(all_probs_mean_diff, "misc-files/all_probs_mean_diff.csv")


rm(list = setdiff(ls(), c("circumstances_glm", "clean_data_with_regression", "tedsd_puf_join", "all_probs_mean_diff")))

