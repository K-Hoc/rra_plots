source("support_functions.R")

f_MAE <- function(p_errorSet) {
  l_sum <- 0
  l_n <- nrow(p_errorSet)
  for (i in 1:l_n) { # Calc: Mean Absolute Error (MAE)
    l_sum <- abs(p_errorSet[i,]$sev_err) + l_sum 
  }
  r_MAE <- l_sum / l_n
  return(r_MAE)
}
# --------------------------------------------------------------------
library(tidyverse)
library(paletteer)
library(ggpubr)
library(patchwork)
library(flextable)

#### Table 2 - Performance metrics ####
dfF1 <- read_csv(file = "output/table2.csv") %>%
  mutate(across(where(is.numeric), \(x) round(x, digits = 3))) %>% 
  select(
    Task = class,
    Dataset = set,
    Response = class,
    Precision,
    Recall,
    Accuracy,
    `F1-Score` = F1
  ) %>% 
  mutate(
    Task = if_else(
      condition = str_detect(Task, "disturbed"),
      true = "A: Disturbance detection",
      false = "B: Classifying reorganization pathway"
    ),
    Response = if_else(
      condition = str_detect(Response, "disturbed"),
      true = "Disturbed / undisturbed",
      false = if_else(
        condition = str_detect(Response, "Class:"),
        true = str_remove(Response, "Class: "),
        false = Response
      )
    ),
    Dataset = if_else(
      condition = str_detect(Dataset, "test_set"),
      true = "Plot level test set",
      false = "Patch level test set"
    )
  )

ft <- dfF1 %>%
  flextable() %>%
  theme_booktabs() %>%
  flextable::color(i = seq(2, nrow(dfF1), 2), color = "black", part = "body") %>%
  flextable::bg(i = seq(2, nrow(dfF1), 2), bg = "#F9F9F9", part = "body") %>% 
  flextable::hline(i = c(1,2,6)) %>% 
  flextable::align(align = "center", part = "all") %>%
  flextable::bold(part = "header") %>%
  autofit()
ft

ft = fit_to_width(ft, max_width = 6.5) # for Word file
save_as_docx(ft, path = "output/table2.docx")
# --------------------------------------------------------------------


#### Figure 3 - Observed vs predicted severity ####
df_fig3 <- read_csv(file = "output/data_fig3.csv")
p1_plt <- ggplot(
  data = df_fig3,
  aes(
    x = severity,
    y = prd_sev
  )
) +
  geom_abline(colour = "grey") +
  geom_point(
    aes(colour = species)
  ) +
  labs(
    title = "Plot level",
    x = "Observed severity (%)",
    y = "Predicted severity (%)",
    col = "Species"
  ) +
  ylim(c(0,100)) +
  scale_color_paletteer_d("MetBrewer::Kandinsky") +
  theme_pubr() +
  theme(aspect.ratio = 1)

df_fig3_patch <- read_csv(file = "output/data_fig3_patch.csv")
p2_patch <- ggplot(
  data = df_fig3_patch,
  aes(x = severity, y = prd_sev)
) +
  geom_abline(colour = "grey") +
  geom_point(aes(colour = species)) +
  labs(
    title = "Patch level",
    x = "Observed severity (%)",
    y = "Predicted severity (%)",
    col = "Species"
  ) +
  ylim(c(0,100)) +
  scale_color_paletteer_d("MetBrewer::Kandinsky") +
  theme_pubr() +
  theme(aspect.ratio = 1)

(p1_plt + p2_patch) +
  plot_annotation(tag_levels = "a") +
  patchwork::plot_layout(guides = "collect") & theme(legend.position = "bottom")

# Save to file
ggsave(
  filename = "output/fig3_2.png",
  scale = 4,
  width = 600,
  height = 350,
  units = "px"
)

# summary(lm(prd_sev ~ severity, data = df_fig3))
cat("Plot level metrics\n")
cat("Summary of prediction error(sev_err):\n")
summary(df_fig3$sev_err)
cat("Standard deviation: ", sd(df_fig3$sev_err), "\n")
cat("Root Mean Squared Error(RMSE): ", f_RMSE(df_fig3$sev_err), "\n")
cat("Mean Absolute Error(MAE): ", f_MAE(df_fig3), "\n")

cat("Patch level metrics\n")
cat("summary of prediction error (sev_err):\n")
summary(df_fig3_patch$sev_err)
cat("Standard deviation: ", sd(df_fig3_patch$sev_err), "\n")
cat("Root Mean Squared Error(RMSE): ", f_RMSE(df_fig3_patch$sev_err), "\n")
cat("Mean Absolute Error(MAE): ", f_MAE(df_fig3_patch), "\n")

# --------------------------------------------------------------------


#### Figure 4 - Development trajectory ####
df4plt <- read_csv(file = "output/data_fig4_plot.csv")
df4patch <- read_csv(file = "output/data_fig4_patch.csv")

df4plt$R_direction <- as.factor(df4plt$R_direction)
df4plt$PrdTrd <- as.factor(df4plt$PrdTrd)
df4plt$R_direction <- factor(df4plt$R_direction, levels = c("Replacement", "Restructuring", "Reassembly", "Resilience"))
df4plt$PrdTrd <- factor(df4plt$PrdTrd, levels = c("Replacement", "Restructuring", "Reassembly", "Resilience"))

df4patch$R_direction <- as.factor(df4patch$R_direction)
df4patch$PrdTrd <- as.factor(df4patch$PrdTrd)
df4patch$R_direction <- factor(df4patch$R_direction, levels = c("Replacement", "Restructuring", "Reassembly", "Resilience"))
df4patch$PrdTrd <- factor(df4patch$PrdTrd, levels = c("Replacement", "Restructuring", "Reassembly", "Resilience"))

# Plot lvl plot
p1 <- ggplot(
  data = df4plt %>%
    select(R_direction, PrdTrd, species) %>%
    pivot_longer(
      cols = c(R_direction, PrdTrd),
      names_to = "param",
      values_to = "val"
    )
  ) +
  geom_bar(
    aes(
      x = interaction(param, species),
      fill = factor(val)
    ),
    position = "fill"
  ) +
  facet_grid(~species, scales = "free_x") +
  scale_x_discrete(
    "", labels = c("Predicted", "Observed"), guide = guide_axis(angle = 45)
  ) +
  labs(
    y = "Percent",
    fill = "Reorganization pathway:",
    title = "Plot level trajectory"
  ) +
  scale_fill_manual(values = c(
    "Reassembly" = "#fed976",
    "Replacement" = "#bd0026",
    "Resilience" = "#a6bddb",
    "Restructuring" = "#fd8d3c"
  )) +
  scale_y_continuous(labels = c("0", "25%", "50%", "75%", "100%")) +
  theme_pubr()

# Patch lvl plot
p2 <- ggplot(
  data = df4patch %>%
    select(R_direction, PrdTrd, species) %>%
    pivot_longer(
      cols = c(R_direction, PrdTrd),
      names_to = "param",
      values_to = "val"
    )
  ) +
  geom_bar(
    aes(
      x = interaction(param, species),
      fill = factor(val)
    ),
    position = "fill"
  ) +
  facet_grid(~species, scales = "free_x") +
  scale_x_discrete(
    "", labels = c("Predicted", "Observed"), guide = guide_axis(angle = 45)
  ) +
  labs(
    y = "Percent",
    fill = "Reorganization pathway:",
    title = "Patch level trajectory"
  ) +
  scale_fill_manual(values = c(
    "Reassembly" = "#fed976",
    "Replacement" = "#bd0026",
    "Resilience" = "#a6bddb",
    "Restructuring" = "#fd8d3c"
  )) +
  scale_y_continuous(labels = c("0", "25%", "50%", "75%", "100%")) +
  theme_pubr()

pleg <- p1 + theme(legend.position = "bottom")
pleg <- get_legend(pleg)
pleg <- ggpubr::as_ggplot(pleg)
p1 <- p1 + theme(legend.position = "none")
p2 <- p2 + theme(legend.position = "none")

(p1 / p2) +
  plot_annotation(tag_levels = "a") +
  patchwork::plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave(
  filename = "output/fig4.png",
  scale = 4,
  height = 1000,
  width = 600,
  units = "px"
)
# --------------------------------------------------------------------
