library(tidyverse)
library(patchwork)
library(ggpubr)
library(paletteer)

# ============================================================
# LOAD ANALYSES ##############################################
# ============================================================

analysis_files <- list.files(
  #path = "/mnt/private/Paper_1/2_work/R/output/analyses",
  path = "~/edfm/private/Paper_1/2_work/R/output/analyses/",
  pattern = "\\.rds$",
  full.names = TRUE
)

analyses <- analysis_files |>
  set_names(
    tools::file_path_sans_ext(
      basename(analysis_files)
    )
  ) |>
  map(readRDS)

# ============================================================
# USER SETTINGS ##############################################
# ============================================================

setwd("/mnt/privat/Paper_1/2_work/R")
analysis_name <- "labels_random_meanpatch" #"labels_oos_meanpatch"

res <- analyses[[analysis_name]]

# output directory
dir.create(
  file.path("output", "figures", analysis_name),
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# FIGURE 3 - Observed vs Predicted Severity ##################
# ============================================================

df_fig3 <- res$predictions$plot |> dplyr::filter(manag != "living")
df_fig3_patch <- res$predictions$patch |> dplyr::filter(manag != "living")

p_plot <- ggplot(
  df_fig3,
  aes(
    x = severity,
    y = pred_sev
  )
) +
  geom_abline(
    slope = 1,
    intercept = 0,
    colour = "grey50"
  ) +
  geom_point(
    aes(colour = species),
    alpha = 0.75
  ) +
  coord_equal() +
  xlim(0,100) +
  ylim(0,100) +
  theme_pubr() +
  scale_color_paletteer_d(
    "MetBrewer::Kandinsky",
    name = "Species",
    labels = c("Beech", "Oak", "Pine", "Spruce")
  ) +
  labs(
    title = "Plot level",
    x = "Observed severity (%)",
    y = "Predicted severity (%)"
  )

p_patch <- ggplot(
  df_fig3_patch,
  aes(
    x = severity,
    y = pred_sev
  )
) +
  geom_abline(
    slope = 1,
    intercept = 0,
    colour = "grey50"
  ) +
  geom_point(
    aes(colour = species),
    alpha = 0.75
  ) +
  coord_equal() +
  xlim(0,100) +
  ylim(0,100) +
  theme_pubr() +
  scale_color_paletteer_d(
    "MetBrewer::Kandinsky",
    name = "Species",
    labels = c("Beech", "Oak", "Pine", "Spruce")
  ) +
  labs(
    title = "Patch level",
    x = "Observed severity (%)",
    y = "Predicted severity (%)"
  )

fig3 <- (p_plot + p_patch) +
  plot_annotation(tag_levels = "a") +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom"
  )

fig3

ggsave(
  filename = file.path(
    "output",
    "figures",
    analysis_name,
    "Figure3_severity.pdf"
  ),
  plot = fig3,
  scale = 3,
  width = 90,
  height = 45,
  units = "mm",
  dpi = 300
)

ggsave(
  filename = file.path(
    "output",
    "figures",
    analysis_name,
    "Figure3_severity.tiff"
  ),
  plot = fig3,
  scale = 3,
  width = 90,
  height = 45,
  units = "mm",
  dpi = 500
)

# ============================================================
# FIGURE 4 - Reorganization Pathway ##########################
# ============================================================

df4plt <- res$predictions$plot |>
  rename(
    PrdTrd = R_dir_pred
  ) |> 
  filter(manag != "living")

df4patch <- res$predictions$patch |>
  rename(
    PrdTrd = R_dir_pred
  ) |> 
  filter(manag != "living")

trajectory_cols <- c(
  Reassembly = "#fed976",
  Replacement = "#bd0026",
  Resilience = "#a6bddb",
  Restructuring = "#fd8d3c"
)

make_traj_plot <- function(df, title) {
  ggplot(
    df |>
      select(
        R_direction,
        PrdTrd,
        species
      ) |>
      pivot_longer(
        c(R_direction, PrdTrd),
        names_to = "type",
        values_to = "value"
      )
  ) +
    geom_bar(
      aes(
        x = interaction(type, species),
        fill = value
      ),
      position = "fill"
    ) +
    geom_text(
      stat = "count",
      aes(
        x = interaction(type, species),
        label = ifelse(after_stat(count), after_stat(count), ""),
        group = value
      ),
      position = position_fill(vjust = 0.5),
      size = 3
    ) +
    facet_grid(
      ~species,
      scales = "free_x",
      labeller = labeller(
        species = c(
          beech = "Beech",
          oak = "Oak",
          pine = "Pine",
          spruce = "Spruce"
        )
      )
    ) +
    scale_fill_manual(
      values = trajectory_cols
    ) +
    scale_x_discrete(
      labels = c(
        "Observed",
        "Predicted"
      )
    ) +
    labs(
      title = title,
      x = "",
      y = "Proportion",
      fill = "Pathway"
    ) +
    theme_pubr() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

p1 <- make_traj_plot(
  df4plt,
  "Plot level"
)

p2 <- make_traj_plot(
  df4patch,
  "Patch level"
)

fig4 <- (p1 / p2) +
  plot_annotation(
    tag_levels = "a"
  ) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom"
  )

fig4

ggsave(
  file.path(
    "output",
    "figures",
    analysis_name,
    "Figure4_pathway.pdf"
  ),
  fig4,
  scale = 3,
  width = 90,
  height = 90,
  units = "mm"
)

ggsave(
  file.path(
    "output",
    "figures",
    analysis_name,
    "Figure4_pathway.tiff"
  ),
  fig4,
  scale = 3,
  width = 90,
  height = 90,
  units = "mm",
  dpi = 500
)

##############################################################################
##############################################################################
##############################################################################
# Comparison figures #########################################################

# ============================================================
# COLLECT ALL METRICS
# ============================================================

all_metrics <- bind_rows(
  lapply(
    analyses,
    function(x) {

      x$metrics$summary |>
        mutate(
          src = x$metadata$src,
          split_method = x$metadata$split_method,
          patch_method = x$metadata$patch_method
        )

    }
  )
)

# Calculate mean for random_plot case
all_metrics <- bind_rows(
  all_metrics |>
    mutate(
      src = if_else(
        condition = src %in% c(
          "labels_oos_randompatch_I","labels_oos_randompatch_II",
          "labels_oos_randompatch_III","labels_oos_randompatch_IV"
        ),
        true = "labels_oos_randompatch",
        false = src
      )
    ) |>
    filter(patch_method == "random_plot") |> 
    group_by(q, lvl, class, src, split_method, patch_method) |> 
    summarise(
      across(where(is.numeric), ~mean(.x, na.rm = FALSE)),
      .groups = "drop"
    ),

  all_metrics |> 
    filter(patch_method != "random_plot")
)

# Comparison 1 - Disturbance detection
q1_comparison <- all_metrics |>
  filter(
    class == "disturbed/undisturbed"
  )

q1_plt <- ggplot(
  q1_comparison,
  aes(
    src,
    F1,
    fill = lvl
  )
) +
  geom_col(
    position = position_dodge()
  ) +
  coord_flip() +
  theme_pubr() +
  labs(
    x = "",
    y = "F1 Score",
    fill = "Level"
  )

ggsave(
  filename = "output/figures/q1_comparison.pdf"
)
ggsave(filename = "output/figures/q1_comparison.png")

# Comparison 2 - severity estimation
q2_comparison <- all_metrics |>
  filter(
    !is.na(MAE)
  )

q2_plt <- ggplot(
  q2_comparison,
  aes(
    src,
    RMSE,
    fill = lvl
  )
) +
  geom_col(
    position = position_dodge()
  ) +
  coord_flip() +
  theme_pubr() +
  labs(
    y = "RMSE",
    x = "",
    fill = "Level"
  )

ggsave(
  filename = "output/figures/q2_comparison.pdf"
)
ggsave(
  filename = "output/figures/q2_comparison.png"
)

# Comparison 3 - Pathway classification
q3_comparison <- all_metrics |>
  filter(
    class %in% c(
      "Reassembly",
      "Replacement",
      "Resilience",
      "Restructuring"
    )
  )

q3_plt1 <- ggplot(
  q3_comparison,
  aes(
    class,
    F1,
    fill = src
  )
) +
  geom_col(
    position = position_dodge()
  ) +
  facet_wrap(
    ~lvl
  ) +
  theme_pubr() +
  labs(
    x = "",
    y = "F1 Score",
    fill = "Analysis"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

ggsave(
  filename = "output/figures/q3_comparison.pdf"
)
ggsave(filename = "output/figures/q3_comparison.png")

# Macro F1
macro_f1 <- all_metrics |>
  filter(
    !is.na(Macro_F1)
  )

q3_mcr_plt <- ggplot(
  macro_f1,
  aes(
    src,
    Macro_F1,
    colour = lvl
  )
) +
  geom_point(
    size = 4
  ) +
  geom_errorbar(
    aes(
      ymin = Macro_F1_low,
      ymax = Macro_F1_high
    ),
    width = 0.15
  ) +
  coord_flip() +
  theme_pubr() +
  labs(
    x = "",
    y = "Macro F1",
    colour = "Level"
  )

ggsave(filename = "output/figures/q3_comparison_macrof1.pdf")
ggsave(filename = "output/figures/q3_comparison_macrof1.png")

write_csv(q1_comparison, file = "output/q1_comparison.csv")
write_csv(q2_comparison, file = "output/q2_comparison.csv")
write_csv(q3_comparison, file = "output/q3_comparison.csv")
write_csv(macro_f1, file = "output/q3_comp_macrF1.csv")

((q1_plt + q2_plt + q3_mcr_plt + plot_layout(guides = "collect", axes = "collect")) / q3_plt1) +
  plot_annotation(tag_levels = "a")
ggsave(
  filename = "output/figures/comparison_combined.png",
  width = 14,
  height = 7
)

##############################################################
# ============================================================
# Table 2 - creation #########################################
# ============================================================
library(flextable)
library(officer)

# Prepare data
df_table2 <- res$metrics$summary |>
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 3)
    )
  ) |>
  filter(
    q %in% c("q1", "q3")
  ) |>
  select(
    q,
    lvl,
    class,
    Precision,
    Recall,
    F1,
    Overall_accuracy,
    `Balanced Accuracy`
  ) |>
  mutate(
    Task = case_when(
      q == "q1" ~ "A: Disturbance detection",
      q == "q3" ~ "B: Reorganization pathway"
    ),
    
    Dataset = case_when(
      lvl == "plot" ~ "Plot level",
      lvl == "patch" ~ "Patch level"
    ),
    
    Response = case_when(
      class == "disturbed/undisturbed" ~
        "Disturbed / undisturbed",

      TRUE ~ class
    ),
    Overall_accuracy = if_else(
      condition = (q == "q3"),
      true = `Balanced Accuracy`,
      false = Overall_accuracy
    )
  ) |>
  select(
    Task,
    Dataset,
    Response,
    Precision,
    Recall,
    F1,
    Accuracy = Overall_accuracy
  ) |> 
  na.omit()

# Create flextable
ft <- df_table2 |>
  flextable() |>
  flextable::theme_booktabs() |>
  flextable::color(i = seq(2, nrow(df_table2), 2), color = "black", part = "body") |> 
  flextable::bg(i = seq(2, nrow(df_table2), 2), bg = "#F9F9F9", part = "body") |> 
  flextable::hline(i = c(1,2,6)) |> 
  flextable::align(align = "center",part = "all") |>
  flextable::bold(part = "header") |>
  flextable::autofit()
ft

ft <- fit_to_width(ft, max_width = 6.5)

# Save as word table
save_as_docx(
  ft,
  path = file.path(
    "output",
    "figures",
    analysis_name,
    "Table2.docx"
  )
)
