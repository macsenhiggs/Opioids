library(tidyverse)
library(ggplot2)
library(fmsb)
library(shadowtext)


if (!exists("clean_data_with_regression")) {
  stop("ERROR: GET DATA WITH REGRESSION FROM glm_creation.R BEFORE PERFORMING ANALYSIS")
}

#GLOBAL PLOT SETTINGS----
path = "plots/"
units = "px"
width = 750
height = 600
scale = 3


#INVESTIGATION 0: EXPLORING EXPECTED SUCCESS----

investigation0_data <- clean_data_with_regression

investigation0_plot <- ggplot(investigation0_data, aes(x = EXP_SUCCESS, fill = SERVICES)
) +
  geom_histogram(binwidth = 0.01,
                 #position = "dodge"
  ) +
  labs(
    title = "Histogram of Expected Case Success by Services Type",
    subtitle = "Binwidth = 0.01 (n = 213388)",
    x = "Expected Success",
    caption = "Data: TEDS-D 2022-2023"
  ) +
  scale_fill_brewer(type = "seq", direction = -1, palette = "Spectral") +
  theme_minimal() +
  theme(
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    text = element_text(face = "bold")
  )

ggsave("investigation0_plot.png", investigation0_plot,
       path = path, units = units, width = width, height = height, scale = scale)


#INVESTIGATION 0.1: EXPLORING EXPECTED SUCCESS ACCUR----

investigation0.1_data <- clean_data_with_regression |>
  group_by(SUB1, SUBS_USED, SERVICES,
           #AGE, SEX,
           #EDUC, EMPLOY, 
           #LIVARAG,PRIMINC,
           #DIVISION,
           ROUTE1, FREQ1, 
           #FRSTUSE1
           ) |>
  summarize(
    EXP_SUCCESS_RATE = mean(EXP_SUCCESS),
    SUCCESS_RATE = mean(SUCCESS),
    SUCCESS_RATE_OE = mean(SUCCESS_OE),
    n = n(),
    .groups = "drop"
  ) |>
  filter(
    n >= 100
  ) |>
  relocate(SERVICES)

investigation0.1_plot <- ggplot(investigation0.1_data, aes(x = EXP_SUCCESS_RATE, y = SUCCESS_RATE, color = SERVICES)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  geom_point(aes(size = n), alpha = 0.5) +
  scale_color_brewer(type = "seq", direction = -1, palette = "Spectral") +
  labs(
    title = "Success Rate vs. Expected Success Rate for Unique Patient Profiles",
    subtitle = "Each point represents a unique combination of SERVICES, SUB1, SUBS_USED, ROUTE1, & FREQ1
(n >= 100)",
    x = "Expected Success Rate",
    y = "Actual Success Rate",
    caption = "Data: TEDS-D 2022-2023"
  ) +
  theme_minimal() +
  theme(
    #legend.position = c(1, 1),
    #legend.justification = c(1, 1),
    text = element_text(face = "bold")
  )

#print(investigation0.1_plot)
ggsave("investigation0-1_plot.png", investigation0.1_plot,
       path = path, units = units, width = width, height = height, scale = scale) 

# investigation0.1_plot_alt <- ggplot(investigation0.1_data, aes(x = EXP_SUCCESS_RATE, y = SUCCESS_RATE_OE, color = SERVICES)) +
#   geom_abline(intercept = 0, slope = 0, color = "black", linetype = "dashed") +
#   geom_point(aes(size = n), alpha = 0.5) +
#   scale_color_brewer(type = "seq", direction = -1, palette = "Spectral") +
#   labs(
#     title = "Success Rate OE vs. Expected Success Rate for Unique Patient Profiles",
#     subtitle = "Each point represents a unique combination of SERVICES, SUB1, SUBS_USED, ROUTE1, & FREQ1
# (n >= 100)",
#     x = "Expected Success Rate",
#     y = "Success Rate OE",
#     caption = "Data: TEDS-D 2022-2023"
#   ) +
#   theme_minimal()
# 
# print(investigation0.1_plot_alt)
# ggsave("investigation0-1_plot_alt.png", investigation0.1_plot_alt,
#        path = path, units = units, width = width, height = height, scale = scale)
  


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
    title = "Impact of METHUSE on Success over Expected by Case Difficulty",
    subtitle = "Expected Success Binned by 0.05 (n = 213388)",
    size = "Number of Cases",
    x = "Expected Success Probability",
    y = "Success Rate over Expected",
    caption = "Data: TEDS-D 2022-2023"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    text = element_text(face = "bold")
    )

#print(investigation1_plot)
ggsave("investigation1_plot.png", investigation1_plot,
       path = path, units = units, width = width, height = height, scale = scale)


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
    title = "Success Rate Over Expected of Patients Given Opioid Therapy by Drug Usage Patterns",
    subtitle = "Tile size represents sample size (n); Color represents performance relative to GLM prediction",
    x = "Route of Administration",
    y = "Frequency of Use",
    caption = "Data: TEDS-D 2022-2023"
    ) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(face = "bold")
  )

ggsave("investigation2_plot.png", investigation2_plot,
       path = path, units = units, width = width, height = height, scale = scale)





#INVESTIGATION 3: IMPACT OF METHUSE BY SERVICES TYPE----
investigation3_data <- clean_data_with_regression |>
  filter(
    METHUSE != "NA"
  ) |>
  group_by(SERVICES, METHUSE) |>
  summarize(
    SUCCESS_RATE = mean(SUCCESS),
    EXPECTED_SUCCESS_RATE = mean(EXP_SUCCESS),
    SUCCESS_RATE_OE = mean(SUCCESS_OE),
    n = n(),
    .groups = "drop"
  )

radar_prep <- investigation3_data |>
  select(SERVICES, METHUSE, SUCCESS_RATE_OE) |>
  pivot_wider(names_from = SERVICES, values_from = SUCCESS_RATE_OE) |>
  column_to_rownames("METHUSE")

max_val <- max(radar_prep, na.rm = TRUE)
min_val <- min(radar_prep, na.rm = TRUE)

radar_final <- rbind(rep(max_val, ncol(radar_prep)), 
                     rep(min_val, ncol(radar_prep)), 
                     radar_prep)

colnames(radar_final) <- gsub(", ?", ",\n", colnames(radar_final))

# Define colors (e.g., Blue for No, Red for Yes)
colors_border = c(rgb(0.97, 0.46, 0.43, 0.9), rgb(0, 0.75, 0.77, 0.9))
colors_in = c(rgb(0.97, 0.46, 0.43, 0.4), rgb(0, 0.75, 0.77, 0.4))


#PLOT STARTS HERE

png("plots/investigation3_plot.png", 
    width = 10, height = 8, units = "in", res = 200)

plot.new()


#default: c(5, 4, 4, 2) + 0.1
#par(mar = c(7, 6, 6, 4))
par(plt = c(0.15, 0.85, 0.15, 0.85), font.lab = 2, font.axis = 2)

radarchart(
  radar_final,
  axistype = 1,
  # Customize the polygon (the shapes)
  pcol = colors_border, 
  pfcol = colors_in, 
  plwd = 4, 
  plty = 1,
  # Customize the grid
  cglcol = "grey", 
  cglty = 1, 
  axislabcol = "grey25", 
  caxislabels = round(seq(min_val, max_val, length.out = 5), 2), 
  cglwd = 0.8,
  # Labels
  vlcex = 0.8 
)

title(
  main = "Success Rate OE for METHUSE Values by Service Type", 
      col.main = "black", 
      cex.main = 1.5
  )



# legend("top", 
#        legend = rownames(radar_final[-c(1,2),]), 
#        col = colors_border,
#        pch = 20, 
#        bty = "n", 
#        pt.cex = 2, 
#        cex = 1.2, 
#        x.intersp = 0.5,
#        text.col = "black", 
#        horiz = T , 
#        inset = c(0.5, -0.1))

legend(
  x = "bottom",           # Position relative to the plot
  inset = c(-0.1, -0.2),    # Push it DOWN into the margin (negative value)
  title = "METHUSE",      # Your legend title
  legend = rownames(radar_final[-c(1,2),]),
  pch = 20,
  col = colors_border,
  text.col = "black",
  cex = 1.1,
  pt.cex = 2,
  bty = "o",              # "o" creates the box (border)
  bg = "white",           # Background color of the box
  horiz = TRUE,           # Lay Yes/No side-by-side
  xpd = TRUE,              # CRITICAL: Allows drawing outside the plot area
  y.intersp = 0.8,    # Squeezes YES and NO closer together
  x.intersp = 0.5,    # Squeezes the dot closer to the text
)

mtext(
  side = 1,
  text = "data: TEDS-D 2022-2023", 
  line = 4,
  adj = 1,
  cex = 0.8,
  #font = 3
)



# investigation3_plot <- recordPlot()
# print(investigation3_plot)

#dev.copy(png, filename = "plots/investigation3_plot.png", width = width, height = height) [10.2]
dev.off() #[10.2]




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
    title = "Impact of METHUSE on Success over Expected by Patient Age",
    subtitle = "n = 213388",
    y = "Success Rate over Expected",
    x = "Age Group",
    fill = "Given\nOpioid\nTherapy",
    caption = "Data: TEDS-D 2022-2023"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(face = "bold")
    )
#print(investigation4_plot)
ggsave("investigation4_plot.png", investigation4_plot,
       path = path, units = units, width = width, height = height, scale = scale)


#FINAL CLEANUP----
rm(list = setdiff(ls(), c("circumstances_glm", "clean_data_with_regression",
                          "TEDS-Dd_puf_join")))


