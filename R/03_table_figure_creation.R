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
df_fig3$manag <- as.factor(df_fig3$manag)
# df_fig3$manag <- factor(df_fig3$manag, labels = c("Managed", "Unmanaged"))
df_fig3_patch <- read_csv(file = "output/data_fig3_patch.csv")
df_fig3_patch$manag <- as.factor(df_fig3_patch$manag)
# df_fig3_patch$manag <- factor(df_fig3_patch$manag, labels = c("Managed", "Unmanaged"))

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
    y = "Predicted severity (%)"
  ) +
  ylim(c(0,100)) +
  scale_color_paletteer_d(
    palette = "MetBrewer::Kandinsky",
    name = "Forest type",
    labels = c(
      beech = "Beech",
      oak = "Oak",
      pine = "Pine",
      spruce = "Spruce"
    )
  ) +
  theme_pubr() +
  theme(aspect.ratio = 1)# +
  # facet_wrap(~manag)

p2_patch <- ggplot(
  data = df_fig3_patch,
  aes(x = severity, y = prd_sev)
) +
  geom_abline(colour = "grey") +
  geom_point(aes(colour = species)) +
  labs(
    title = "Patch level",
    x = "Observed severity (%)",
    y = "Predicted severity (%)"
  ) +
  ylim(c(0,100)) +
  scale_color_paletteer_d(
    "MetBrewer::Kandinsky",
    name = "Forest type",
    labels = c(
      beech = "Beech",
      oak = "Oak",
      pine = "Pine",
      spruce = "Spruce"
    )
  ) +
  theme_pubr() +
  theme(aspect.ratio = 1)# +
  # facet_wrap(~manag)

(p1_plt + p2_patch) +
  plot_annotation(tag_levels = "a") +
  patchwork::plot_layout(guides = "collect") & theme(legend.position = "bottom")

# Save to file
ggsave(
  filename = "output/Figure3.tiff", #"output/fig3_2.png",
  scale = 3, #4,
  # width = 600,
  # height = 350, # 600,
  # units = "px",
  width = 90,
  height = 45,
  units = "mm",
  dpi = 500
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
df4plt$R_direction <- factor(
  df4plt$R_direction,
  levels = c("Replacement", "Restructuring", "Reassembly", "Resilience")
)
df4plt$PrdTrd <- factor(
  df4plt$PrdTrd,
  levels = c("Replacement", "Restructuring", "Reassembly", "Resilience")
)
df4plt$manag <- as.factor(df4plt$manag)
# df4plt$manag <- factor(df4plt$manag, labels = c("Managed", "Unmanaged")) #c("Cleared", "Dead"))

df4patch$R_direction <- as.factor(df4patch$R_direction)
df4patch$PrdTrd <- as.factor(df4patch$PrdTrd)
df4patch$R_direction <- factor(
  df4patch$R_direction,
  levels = c("Replacement", "Restructuring", "Reassembly", "Resilience")
)
df4patch$PrdTrd <- factor(
  df4patch$PrdTrd,
  levels = c("Replacement", "Restructuring", "Reassembly", "Resilience")
)
df4patch$manag <- as.factor(df4patch$manag)
# df4patch$manag <- factor(df4patch$manag, labels = c("Managed", "Unmanaged")) #c("Cleared", "Dead"))

# Plot lvl plot
p1 <- ggplot(
  data = df4plt %>%
    select(R_direction, PrdTrd, species, manag) %>%
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
  geom_text(
    stat = "count",
    aes(
      x = interaction(param, species),
      label = ifelse(after_stat(count), after_stat(count), ""),
      group = val
    ),
    position = position_fill(vjust = 0.5),
    size = 3
  ) +
  facet_grid(~species, scales = "free_x") +
  # facet_grid(manag~species, scales = "free_x") +
  scale_x_discrete(
    "", labels = c("Predicted", "Observed"), guide = guide_axis(angle = 45)
  ) +
  labs(
    y = "Percent",
    fill = "Reorganization pathway:",
    title = "Plot level"
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
    select(R_direction, PrdTrd, species, manag) %>%
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
  geom_text(
    stat = "count",
    aes(
      x = interaction(param, species),
      label = ifelse(after_stat(count), after_stat(count), ""),
      group = val
    ),
    position = position_fill(vjust = 0.5),
    size = 3
  ) +
  # facet_grid(manag~species, scales = "free_x") +
  facet_grid(~species, scales = "free_x") +
  scale_x_discrete(
    "", labels = c("Predicted", "Observed"), guide = guide_axis(angle = 45)
  ) +
  labs(
    y = "Percent",
    fill = "Reorganization pathway:",
    title = "Patch level"
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

# ggsave(
#   filename = "output/fig4_mngmnt.png",
#   scale = 4,
#   height = 600, # 1000
#   width = 600,
#   units = "px"
# )
ggsave(
  filename = "output/Figure4.tiff",
  scale = 3,
  width = 90,
  height = 90,
  units = "mm",
  dpi = 500 
)
# --------------------------------------------------------------------
