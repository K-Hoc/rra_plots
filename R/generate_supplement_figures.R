# ============================================================
# CREATE SUPPLEMENT FIGURES AND TABLES
# ============================================================

library(tidyverse)
library(ggpubr)
library(patchwork)
library(paletteer)
library(gt)
source("support_functions.R")

# ============================================================
# SETTINGS
# ============================================================

setwd("/home/khoch/edfm/private/Paper_1/2_work/R/")
analysis_name <- "labels_random_meanpatch" #"labels_oos_meanpatch"

# ============================================================
# LOAD ANALYSIS
# ============================================================

res <- readRDS(
  file.path(
    "output",
    "analyses",
    paste0(analysis_name, ".rds")
  )
)

out_dir <- file.path(
  "output",
  "supplement",
  analysis_name
)

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# TABLE S1-S3
# ============================================================
s4_tbl_plt <- res$predictions$plot |> 
  group_by(manag) |> 
  summarise(
    TP = sum(pred_dist == "disturbed"   & disturbed == "disturbed"),
    TN = sum(pred_dist == "undisturbed" & disturbed == "undisturbed"),
    FP = sum(pred_dist == "disturbed"   & disturbed == "undisturbed"),
    FN = sum(pred_dist == "undisturbed" & disturbed == "disturbed"),
    .groups = "drop"
  ) |> 
  mutate(
    N = paste0(
      TP + TN, "/", TP + TN + FP + FN
    ),
    Accuracy     = round((TP + TN) / (TP + TN + FP + FN), 3),
    Sensitivity  = round(TP / (TP + FN), 3),
    Specificity  = round(TN / (TN + FP), 3),
    Precision    = round(TP / (TP + FP), 3),
    Recall       = round(TP / (TP + FN), 3),
    F1_score     = round(2 * TP / (2 * TP + FP + FN), 3)
  )

s4_tbl_ptch <- res$predictions$patch |> 
  group_by(manag) |> 
  summarise(
    TP = sum(pred_dist == "disturbed"   & disturbed == "disturbed"),
    TN = sum(pred_dist == "undisturbed" & disturbed == "undisturbed"),
    FP = sum(pred_dist == "disturbed"   & disturbed == "undisturbed"),
    FN = sum(pred_dist == "undisturbed" & disturbed == "disturbed"),
    .groups = "drop"
  ) |> 
  mutate(
    N = paste0(
      TP + TN, "/", TP + TN + FP + FN
    ),
    Accuracy     = round((TP + TN) / (TP + TN + FP + FN), 3),
    Sensitivity  = round(TP / (TP + FN), 3),
    Specificity  = round(TN / (TN + FP), 3),
    Precision    = round(TP / (TP + FP), 3),
    Recall       = round(TP / (TP + FN), 3),
    F1_score     = round(2 * TP / (2 * TP + FP + FN), 3)
  )

s4_tbl <- bind_rows(
  s4_tbl_plt |>
    mutate(
      Dataset = "Plot level test set",
      Task = "A: Disturbance detection",
      Response = "Disturbed / undisturbed"
    ),
  s4_tbl_ptch |>
    mutate(
      Dataset = "Patch level test set",
      Task = "A: Disturbance detection",
      Response = "Disturbed / undisturbed"
    )
) |>
  select(
    Task, Dataset, Response, manag, N,
    Precision, Recall, Accuracy, `F1-Score` = F1_score
  )
s4_tbl
  
supp_tables <- f_generate_supplement_tables(
  res$metrics$summary,
  table_prefix = "Table S"
)

gtsave(
  supp_tables$disturbance_detection,
  file.path(
    out_dir,
    "TableS1_disturbance_detection.html"
  )
)

gtsave(
  supp_tables$severity_regression,
  file.path(
    out_dir,
    "TableS2_severity_regression.html"
  )
)

gtsave(
  supp_tables$multiclass_classification,
  file.path(
    out_dir,
    "TableS3_multiclass_classification.html"
  )
)

# ============================================================
# FIGURE S2
# Predicted vs Observed Severity
# ============================================================

df_figS2 <- res$predictions$plot %>%
  mutate(
    prd_sev = pred_sev
  )

df_figS2_patch <- res$predictions$patch %>%
  mutate(
    prd_sev = pred_sev
  )

f_figS2_plot <- function(data, lvl) {

  ggplot(
    data,
    aes(
      x = severity,
      y = prd_sev
    )
  ) +
    geom_abline(
      slope = 1,
      intercept = 0,
      colour = "grey50"
    ) +
    geom_point(
      aes(colour = species)
    ) +
    facet_wrap(~manag) +
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
    coord_equal() +
    xlim(0, 100) +
    ylim(0, 100) +
    labs(
      title = lvl,
      x = "Observed severity (%)",
      y = "Predicted severity (%)"
    ) +
    theme_pubr() +
    theme(
      aspect.ratio = 1
    )

}

pS2_plot <- f_figS2_plot(
  df_figS2,
  "Plot level"
)

pS2_patch <- f_figS2_plot(
  df_figS2_patch,
  "Patch level"
)

figS2 <- (pS2_plot / pS2_patch) +
  plot_annotation(
    tag_levels = "a"
  ) &
  theme(
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(
    out_dir,
    "FigureS2.pdf"
  ),
  plot = figS2,
  width = 45,
  height = 45,
  units = "mm"
)

ggsave(
  filename = file.path(
    out_dir,
    "FigureS2.tiff"
  ),
  plot = figS2,
  width = 45,
  height = 45,
  units = "mm",
  dpi = 500
)

# ============================================================
# FIGURE S3
# Observed vs Predicted Reorganization Pathways
# ============================================================

trajectory_levels <- c(
  "Replacement",
  "Restructuring",
  "Reassembly",
  "Resilience"
)

df_S3 <- res$predictions$plot %>%
  rename(
    PrdTrd = R_dir_pred
  ) %>%
  mutate(
    R_direction = factor(
      R_direction,
      levels = trajectory_levels
    ),
    PrdTrd = factor(
      PrdTrd,
      levels = trajectory_levels
    )
  )

df_S3_patch <- res$predictions$patch %>%
  rename(
    PrdTrd = R_dir_pred
  ) %>%
  mutate(
    R_direction = factor(
      R_direction,
      levels = trajectory_levels
    ),
    PrdTrd = factor(
      PrdTrd,
      levels = trajectory_levels
    )
  )

f_figS3_plot <- function(data, lvl) {

  ggplot(
    data %>%
      select(
        R_direction,
        PrdTrd,
        species,
        manag
      ) %>%
      pivot_longer(
        cols = c(
          R_direction,
          PrdTrd
        ),
        names_to = "param",
        values_to = "val"
      )
  ) +

    geom_bar(
      aes(
        x = interaction(param, species),
        fill = val
      ),
      position = "fill"
    ) +

    geom_text(
      stat = "count",
      aes(
        x = interaction(param, species),
        label = ifelse(
          after_stat(count),
          after_stat(count),
          ""
        ),
        group = val
      ),
      position = position_fill(
        vjust = 0.5
      ),
      size = 3
    ) +

    facet_grid(
      manag ~ species,
      scales = "free_x"
    ) +

    scale_x_discrete(
      "",
      labels = c(
        "Observed",
        "Predicted"
      )
    ) +

    scale_fill_manual(
      values = c(
        Reassembly = "#fed976",
        Replacement = "#bd0026",
        Resilience = "#a6bddb",
        Restructuring = "#fd8d3c"
      )
    ) +

    scale_y_continuous(
      labels = scales::percent
    ) +

    labs(
      title = lvl,
      y = "Percent",
      fill = "Reorganization pathway"
    ) +

    theme_pubr()

}

pS3_plot <- f_figS3_plot(
  df_S3,
  "Plot level"
)

pS3_patch <- f_figS3_plot(
  df_S3_patch,
  "Patch level"
)

figS3 <- (pS3_plot / pS3_patch) +
  plot_annotation(
    tag_levels = "a"
  ) &
  theme(
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(
    out_dir,
    "FigureS3.pdf"
  ),
  plot = figS3,
  width = 45,
  height = 45,
  units = "mm"
)

ggsave(
  filename = file.path(
    out_dir,
    "FigureS3.tiff"
  ),
  plot = figS3,
  width = 45,
  height = 45,
  units = "mm",
  dpi = 500
)

# ============================================================
# DONE
# ============================================================

message(
  "Supplement figures and tables written to: ",
  out_dir
)