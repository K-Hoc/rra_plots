# Supplement figures script ####
# Creation of supplemental figures and tables
library(tidyverse)
library(patchwork)

# Table S1 - Parameters of neural networks ####
# Gathered from website (keras.io/api/applications)

# Table S2 - Metrics of neural networks during hold-out test ####
# -> Created in 01_Visualize_train_val.Rmd

# Table S3 - Average result metrics of the 10-fold cross validation ####
# -> Created in 01_Visualize_train_val.Rmd

# Table S4 - Performance of random forest models including management ####
# -> Created in 02_Analysis.Rmd

# Figure S2 - Predicted vs. observed disturbance severity ####
df_figS2 <- read_csv(file = "output/data_fig3.csv")

df_figS2$manag <- as.factor(df_fig3$manag)
df_figS2_patch <- read_csv(file = "output/data_fig3_patch.csv")
df_figS2_patch$manag <- as.factor(df_fig3_patch$manag)

f_figS2_plot <- function(data, lvl) {
  r_plt <- ggplot(
    data = data,
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
      title = lvl,
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
    theme(aspect.ratio = 1) +
    facet_wrap(~manag)

  return(r_plt)
}

p1_plt <- f_figS2_plot(df_figS2, "Plot level")
p2_plt <- f_figS2_plot(df_figS2_patch, "Patch level")

(p1_plt + p2_patch) +
  plot_annotation(tag_levels = "a") +
  patchwork::plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave(
  filename = "output/FigureS2.tiff",
  scale = 4,
  width = 600,
  height = 350,
  units = "px",
  dpi = 500
)

# Figure S3 - Observed vs. predicted post disturbance reorganization pathways ####
df_S3 <- read_csv(file = "output/data_fig4_plot.csv")
df_S3_patch <- read_csv(file = "output/data_fig4_patch.csv")

df_S3$R_direction <- as.factor(df_S3$R_direction)
df_S3$PrdTrd <- as.factor(df_S3$PrdTrd)
df_S3$R_direction <- factor(
  df_S3$R_direction,
  levels = c("Replacement", "Restructuring", "Reassembly", "Resilience")
)
df_S3$PrdTrd <- factor(
  df_S3$PrdTrd,
  levels = c("Replacement", "Restructuring", "Reassembly", "Resilience")
)
df_S3$manag <- as.factor(df_S3$manag)

df_S3_patch$R_direction <- as.factor(df_S3_patch$R_direction)
df_S3_patch$PrdTrd <- as.factor(df_S3_patch$PrdTrd)
df_S3_patch$R_direction <- factor(
  df_S3_patch$R_direction,
  levels = c("Replacement", "Restructuring", "Reassembly", "Resilience")
)
df_S3_patch$PrdTrd <- factor(
  df_S3_patch$PrdTrd,
  levels = c("Replacement", "Restructuring", "Reassembly", "Resilience")
)
df_S3_patch$manag <- as.factor(df_S3_patch$manag)

f_figS3_plot <- function(data, lvl) {
  r_plt <- ggplot(
    data = data %>%
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
    facet_grid(manag~species, scales = "free_x") +
    scale_x_discrete(
      "", labels = c("Predicted", "Observed"), guide = guide_axis(angle = 45)
    ) +
    labs(
      y = "Percent",
      fill = "Reorganization pathway:",
      title = lvl
    ) +
    scale_fill_manual(values = c(
      "Reassembly" = "#fed976",
      "Replacement" = "#bd0026",
      "Resilience" = "#a6bddb",
      "Restructuring" = "#fd8d3c"
    )) +
    scale_y_continuous(labels = c("0", "25%", "50%", "75%", "100%")) +
    theme_pubr()

  return(r_plt)
}

p1 <- f_figS3_plot(df_S3, "Plot level")
p2 <- f_figS3_plot(df_S3_patch, "Patch level")

pleg <- p1 + theme(legend.position = "bottom")
pleg <- get_legend(pleg)
pleg <- ggpubr::as_ggplot(pleg)
p1 <- p1 + theme(legend.position = "none")
p2 <- p2 + theme(legend.position = "none")

(p1 / p2) +
  plot_annotation(tag_levels = "a") +
  patchwork::plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave(
  filename = "output/figS3.tiff",
  scale = 4,
  height = 600,
  width = 600,
  units = "px",
  dpi = 500
)

# Figure S4 - Structural complexity and groundcover across forests and management ####
# Created in 02_Analysis.Rmd

# Figure S5 - Relative change in structural and groundcover indicators ####
# Created in 02_Analysis.Rmd

# Figure S6 - Image labels validation ####
# Crated in 00_label_plausibility.R