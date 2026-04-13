library(tidyverse)
library(ggplot2)


if (!exists("clean_data_with_regression")) {
  stop("ERROR: GET DATA WITH REGRESSION FROM glm_creation.R BEFORE PERFORMING ANALYSIS")
}


#INVESTIGATION 0: EXPLORING EXPECTED SUCCESS----

investigation0_data <- clean_data_with_regression |>
  filter(LIVARAG != "NA")

investigation0_plot <- ggplot(investigation0_data, aes(x = EXP_SUCCESS, fill = SERVICES)
) +
  geom_histogram(binwidth = 0.01,
                 #position = "dodge"
  ) +
  labs(
    title = "Histogram of Expected Case Success by Service Type",
    subtitle = "Binwidth = 0.01",
    x = "Expected Success",
    caption = "Data: TEDS 2022-2023"
  ) +
  theme_minimal()

ggsave("investigation0_plot.png", investigation0_plot, path = "plots/",
       units = "px", width = 750, height = 600, scale = 4)


#INVESTIGATION 1: IMPACT OF METHUSE BY DIFFICULTY OF PATIENT CASE----
investigation1_data <- clean_data_with_regression |>
  mutate(EXP_SUCCESS_BIN = cut(EXP_SUCCESS,
                               breaks = seq(0, 1, by = 0.05),
                               include.lowest = TRUE)
  ) |>
  arrange(EXP_SUCCESS_BIN) |>
  group_by(#SUB1,
    EXP_SUCCESS_BIN,
    METHUSE) |>
  summarize(
    SUCCESS_RATE = mean(SUCCESS),
    EXPECTED_SUCCESS_RATE = mean(EXP_SUCCESS),
    SUCCESS_RATE_OE = mean(SUCCESS_OE),
    n = n(),
    .groups = "drop"
  )
investigation1_plot <- investigation1_data |>
  filter(METHUSE != "NA") |>
  ggplot(aes(x = EXP_SUCCESS_BIN, y = SUCCESS_RATE_OE, color = METHUSE, group = METHUSE)) +
  geom_point(aes(size = n), alpha = 0.6) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #facet_wrap(~ SUB1, scales = "free_y") +
  labs(
    title = "Impact of METHUSE by Predicted Likelihood of Success",
    subtitle = "Among rehab patients primarily dependent on heroin, methadones, or other opiates (n = 213388)",
    size = "Number of Cases",
    x = "Projected Success Probability (binned by 0.05)",
    y = "Success Rate over Expected",
    caption = "Data: TEDS 2022-2023"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#print(investigation1_plot)
ggsave("investigation1_plot.png", investigation1_plot, path = "plots/",
       units = "px", width = 750, height = 600, scale = 4)


# 
# #ALT IDEA: Update the data processing to include SUB1
# investigation1_data_alt <- clean_data_with_regression |>
#   filter(!is.na(SUB1), !is.na(METHUSE)) |> # Filter NAs early for a cleaner plot
#   mutate(EXP_SUCCESS_BIN = cut(EXP_SUCCESS,
#                                breaks = seq(0, 1, by = 0.05),
#                                include.lowest = TRUE)
#   ) |>
#   group_by(SUB1, EXP_SUCCESS_BIN, METHUSE) |> # Added SUB1 here
#   summarize(
#     SUCCESS_RATE = mean(SUCCESS),
#     EXPECTED_SUCCESS_RATE = mean(EXP_SUCCESS),
#     SUCCESS_RATE_OE = mean(SUCCESS_OE),
#     n = n(),
#     .groups = "drop"
#   )
# 
# # 2. Update the plot with faceting
# investigation1_plot_alt <- investigation1_data_alt |>
#   filter(METHUSE != "NA") |>
#   ggplot(aes(x = EXP_SUCCESS_BIN, y = SUCCESS_RATE_OE, color = METHUSE, group = METHUSE)) +
#   geom_point(aes(size = n), alpha = 0.6) + # Added alpha to help with overlapping points
#   geom_line() +
#   geom_hline(yintercept = 0, linetype = "dashed",) +
#   facet_wrap(~ SUB1, scales = "free_y") + # This creates the multiple charts
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotates x labels for readability
#   labs(title = "Impact of METHUSE by Case Difficulty and Substance",
#        x = "Expected Success Bin",
#        y = "O/E Success Rate")
# 
# print(investigation1_plot_alt)



#INVESTIGATION 2: IMPACT OF METHUSE BY DRUG, ROUTE, AND FREQUENCY----
investigation2_data <- clean_data_with_regression |>
  group_by(
    SUB1,
    ROUTE1,
    FREQ1
  ) |>
  filter(
    METHUSE == "Yes",
    ROUTE1 != "NA",
    FREQ1 != "NA"
  ) |>
  summarize(
    SUCCESS_RATE = mean(SUCCESS),
    EXPECTED_SUCCESS_RATE = mean(EXP_SUCCESS),
    SUCCESS_RATE_OE = mean(SUCCESS_OE),
    n = n(),
    .groups = "drop"
  )

library(shadowtext)

max_n <- max(investigation2_data$n, na.rm = TRUE)
plot_data <- investigation2_data %>%
  mutate(size_scale = (n / max_n) ^ 0.2)

investigation2_plot <- ggplot(plot_data, aes(x = ROUTE1, y = FREQ1, fill = SUCCESS_RATE_OE)) +
  
  geom_tile(aes(width = size_scale, height = size_scale), color = "white") +
  
  geom_shadowtext(
    aes(label = sprintf("%.2f\n(n=%d)", SUCCESS_RATE_OE, n)),
    color = "white",
    bg.color = "black",
    bg.r = 0.15,
    size = 3,
    lineheight = 0.9
  ) +
  
  scale_fill_gradient2(
    low = "red", 
    mid = "snow2", 
    high = "darkgreen", 
    midpoint = 0,
    limits = c(-0.2, 0.2),
    oob = scales::squish,
    name = "Success OE"
  ) +
  
  facet_wrap(~SUB1, ncol = 1) + 
  
  labs(
    title = "Success Rate: Observed vs. Expected (OE Ratio)",
    subtitle = "Tile size represents sample size (n); Color represents performance relative to GLM prediction",
    x = "Route of Administration",
    y = "Frequency of Use",
    caption = "Data: TEDS 2022-2023"
    ) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("investigation2_plot.png", investigation2_plot, path = "plots/",
       units = "px", width = 750, height = 600, scale = 4)





#INVESTIGATION 3: IMPACT OF LENGTH OF STAY ON SUCCESS OVER EXPECTED----
investigation3_data <- clean_data_with_regression |>
  group_by(LOS) |>
  summarize(
    SUCCESS_RATE = mean(SUCCESS),
    EXPECTED_SUCCESS_RATE = mean(EXP_SUCCESS),
    SUCCESS_RATE_OE = mean(SUCCESS_OE),
    n = n()
  ) |>
  mutate(LOS_INDEX = row_number())
investigation3_plot <- ggplot(investigation3_data, aes(x = LOS_INDEX, y = SUCCESS_RATE_OE)) +
  geom_path() +
  geom_vline(xintercept = 30, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(size = n)) +
  labs(
    title = "Relative Success Rate of Opioid Rehab Cases By Length of Stay",
    subtitle = "LOS is grouped for stays > 30 days (n = 439300)",
    x = "Length of Stay",
    y = "Success Rate Over Expected",
    caption = "Data: TEDS 2022-2023"
  ) +
  theme_minimal()
#print(investigation3_plot)
ggsave("investigation3_plot.png", investigation3_plot, path = "plots/",
       units = "px", width = 750, height = 600, scale = 4)



#INVESTIGATION 4: IMPACT OF METHUSE BY AGE----
investigation4_data <- clean_data_with_regression |>
  group_by(
    AGE,
    METHUSE) |>
  filter(
    METHUSE != "NA"
  ) |>
  summarize(
    SUCCESS_RATE = mean(SUCCESS),
    EXPECTED_SUCCESS_RATE = mean(EXP_SUCCESS),
    SUCCESS_RATE_OE = mean(SUCCESS_OE),
    n = n(),
    .groups = "drop"
  )
min_y_val <- min(investigation4_data$SUCCESS_RATE_OE) * 1.1 #place label 10% lower than min value

investigation4_plot <- ggplot(investigation4_data, aes(x = factor(AGE), y = SUCCESS_RATE_OE, fill = METHUSE)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.5) +
  #scale_fill_brewer(palette = "Paired") +
  geom_text(
    aes(
      label = paste0("n = ", n),
      vjust = ifelse(SUCCESS_RATE_OE >= 0, -0.5, 1.5)
    ),
    position = position_dodge(0.5), 
    color = "black",
    size = 2.5,
    fontface = "bold"
  ) +
  labs(
    title = "Bar Chart of Opioid Patient Success over Expected by Opioid Therapy Status and Primary Substance",
    subtitle = "Expected success determined by GLM based on demographic and abuse data",
    y = "Success Rate over Expected",
    x = "Age Group",
    fill = "Given\nOpioid\nTherapy",
    caption = "Data: TEDS 2022-2023"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#print(investigation4_plot)
ggsave("investigation4_plot.png", investigation4_plot, path = "plots/",
       units = "px", width = 750, height = 600, scale = 4)


rm(list = ls(pattern = "investigation[0-9]|plot_data"))


