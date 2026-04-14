library(ggeffects)
library(broom)
library(ggplot2)
library(dplyr)
library(purrr)




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
  format(group_mean_diff = group_mean_diff, scientific = FALSE) |>
  select(-c(std.error, conf.low, conf.high, group))

fwrite(all_probs_mean_diff, "misc-files/all_probs_mean_diff.csv")





#EXPERIMENT: EXTRACT ODDS RATIOS
# Extract and calculate Odds Ratios
impact_table <- tidy(
  circumstances_glm,
  exponentiate = TRUE,
  conf.int = TRUE,
  conf.method = "Wald"
  ) %>%
  select(term, estimate, p.value, conf.low, conf.high) %>%
  rename(odds_ratio = estimate) %>%
  arrange(desc(odds_ratio)) # Highest impact at the top

# View the Top 5 Positive Impacts
head(impact_table, 5)

# View the Top 5 Negative Impacts
tail(impact_table, 5)