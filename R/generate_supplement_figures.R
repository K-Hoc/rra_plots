# ============================================================
# CREATE SUPPLEMENT FIGURES AND TABLES (S1-S6)
# ============================================================

library(tidyverse)
library(ggpubr)
library(patchwork)
library(paletteer)
library(gt)
library(flextable)
library(caret)

# ============================================================
# SETTINGS
# ============================================================

analysis_name <- Sys.getenv("ANALYSIS_NAME", unset = "labels_random_meanpatch")
save_png_preview <- TRUE

# ============================================================
# PATHS AND HELPERS
# ============================================================

f_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_flag <- "--file="
  idx <- grep(file_flag, args)

  if (length(idx) > 0) {
    return(dirname(normalizePath(sub(file_flag, "", args[idx[1]]))))
  }

  return(normalizePath(getwd()))
}

script_dir <- file.path(f_script_dir(), "R")
project_dir <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)
paper_dir <- normalizePath(file.path(script_dir, "..", ".."), mustWork = FALSE)

source(file.path(script_dir, "support_functions.R"))

f_assert_columns <- function(df, required_cols, df_name) {
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "Missing required columns in ",
        df_name,
        ": ",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
}

f_save_plot_bundle <- function(
    plt,
    out_dir,
    stem,
    width_mm = 45,
    height_mm = 45,
    dpi = 500,
    save_png = TRUE
) {
  ggsave(
    filename = file.path(out_dir, paste0(stem, ".pdf")),
    plot = plt,
    width = width_mm,
    height = height_mm,
    units = "mm"
  )

  ggsave(
    filename = file.path(out_dir, paste0(stem, ".tiff")),
    plot = plt,
    width = width_mm,
    height = height_mm,
    units = "mm",
    dpi = dpi
  )

  if (isTRUE(save_png)) {
    ggsave(
      filename = file.path(out_dir, paste0(stem, ".png")),
      plot = plt,
      width = width_mm,
      height = height_mm,
      units = "mm",
      dpi = 300
    )
  }
}

f_rel_change <- function(initial_value, final_value) {
  ifelse(
    is.na(initial_value) | is.na(final_value) | initial_value == 0,
    NA_real_,
    (final_value - initial_value) / initial_value
  )
}

analysis_path <- file.path(
  script_dir,
  "output",
  "analyses",
  paste0(analysis_name, ".rds")
)

if (!file.exists(analysis_path)) {
  stop(
    paste0(
      "Analysis object not found: ",
      analysis_path,
      ". Run analysis.R first or adjust analysis_name."
    )
  )
}

# ============================================================
# LOAD ANALYSIS
# ============================================================

res <- readRDS(analysis_path)

out_dir <- file.path(
  script_dir,
  "output",
  "supplement",
  analysis_name
)

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

f_assert_columns(
  res$predictions$plot,
  c(
    "severity",
    "pred_sev",
    "disturbed",
    "pred_dist",
    "R_direction",
    "R_dir_pred",
    "manag",
    "species"
  ),
  "res$predictions$plot"
)

f_assert_columns(
  res$predictions$patch,
  c(
    "severity",
    "pred_sev",
    "disturbed",
    "pred_dist",
    "R_direction",
    "R_dir_pred",
    "manag",
    "species"
  ),
  "res$predictions$patch"
)

# ============================================================
# TABLE S2&S3
# ============================================================

s2_tbl <- read_csv(file = "output/TableS2_DNNs_holdout_metrics.csv")
s3_tbl <- read_csv(file = "output/TableS3_DNNs_10fold_results.csv")

tbl_s2 <- s2_tbl |> 
  flextable() |> 
  theme_booktabs() |> 
  flextable::color(i = seq(2, nrow(s2_tbl), 2), color = "black", part = "body") |> 
  flextable::bg(i = seq(2, nrow(s2_tbl), 2), bg = "#F9F9F9", part = "body") |> 
  flextable::align(align = "center", part = "all") |> 
  flextable::hline(i = c(5,9,10,15,19)) |> 
  flextable::autofit() |> 
  flextable::bold(part = "header") |> 
  flextable::fit_to_width(max_width = 6.5)
tbl_s3 <- s3_tbl |>
  flextable() |> 
  theme_booktabs() |> 
  flextable::color(i = seq(2, nrow(s3_tbl), 2), color = "black", part = "body") |> 
  flextable::bg(i = seq(2, nrow(s3_tbl), 2), bg = "#F9F9F9", part = "body") |> 
  flextable::hline(i = c(5,9,10,15,19)) |> 
  flextable::align(align = "center", part = "all") |> 
  flextable::autofit() |> 
  flextable::bold(part = "header") |> 
  flextable::fit_to_width(max_width = 6.5)

save_as_docx(
  tbl_s2,
  path = file.path(out_dir, "TableS2_DNNs_holdout_metrics.docx")
)
save_as_docx(
  tbl_s3,
  path = file.path(out_dir, "TableS3_DNNs_10fold_results.docx")
)

message("Wrote Table S2 and S3 to: ", out_dir)

# ============================================================
# TABLE S4 (DOCX)
# ============================================================

trajectory_levels <- c(
  "Replacement",
  "Restructuring",
  "Reassembly",
  "Resilience"
)

s4_tbl_plt <- res$predictions$plot |>
  group_by(manag) |>
  summarise(
    TP = sum(pred_dist == "disturbed" & disturbed == "disturbed", na.rm = TRUE),
    TN = sum(pred_dist == "undisturbed" & disturbed == "undisturbed", na.rm = TRUE),
    FP = sum(pred_dist == "disturbed" & disturbed == "undisturbed", na.rm = TRUE),
    FN = sum(pred_dist == "undisturbed" & disturbed == "disturbed", na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Accuracy = round((TP + TN) / (TP + TN + FP + FN), 3),
    Precision = round(TP / (TP + FP), 3),
    Recall = round(TP / (TP + FN), 3),
    `F1-Score` = round(2 * TP / (2 * TP + FP + FN), 3),
    Task = "A: Disturbance detection",
    Dataset = "Plot level test set",
    Response = "Disturbed / undisturbed"
  ) |>
  select(Task, Dataset, Response, manag, Precision, Recall, Accuracy, `F1-Score`)

s4_tbl_patch <- res$predictions$patch |>
  group_by(manag) |>
  summarise(
    TP = sum(pred_dist == "disturbed" & disturbed == "disturbed", na.rm = TRUE),
    TN = sum(pred_dist == "undisturbed" & disturbed == "undisturbed", na.rm = TRUE),
    FP = sum(pred_dist == "disturbed" & disturbed == "undisturbed", na.rm = TRUE),
    FN = sum(pred_dist == "undisturbed" & disturbed == "disturbed", na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Accuracy = round((TP + TN) / (TP + TN + FP + FN), 3),
    Precision = round(TP / (TP + FP), 3),
    Recall = round(TP / (TP + FN), 3),
    `F1-Score` = round(2 * TP / (2 * TP + FP + FN), 3),
    Task = "A: Disturbance detection",
    Dataset = "Patch level test set",
    Response = "Disturbed / undisturbed"
  ) |>
  select(Task, Dataset, Response, manag, Precision, Recall, Accuracy, `F1-Score`)

f_s4_q3_by_management <- function(df, pred_col, truth_col, dataset_label) {
  out <- tibble()

  for (mng in unique(df$manag)) {
    tmp_mng <- df |>
      filter(manag == mng) |>
      filter(!is.na(.data[[pred_col]]), !is.na(.data[[truth_col]])) |>
      mutate(
        pred_tmp = factor(.data[[pred_col]], levels = trajectory_levels),
        truth_tmp = factor(.data[[truth_col]], levels = trajectory_levels)
      )

    if (nrow(tmp_mng) == 0) {
      next
    }

    cm <- caret::confusionMatrix(
      data = tmp_mng$pred_tmp,
      reference = tmp_mng$truth_tmp,
      mode = "prec_recall"
    )

    tmp_df <- as.data.frame(cm$byClass) |>
      tibble::rownames_to_column(var = "Response") |>
      mutate(
        Response = gsub("^Class: ", "", Response),
        manag = mng,
        Task = "B: Classifying reorganization pathway",
        Dataset = dataset_label
      ) |>
      select(
        Task,
        Dataset,
        Response,
        manag,
        Precision,
        Recall,
        Accuracy = `Balanced Accuracy`,
        `F1-Score` = F1
      )

    out <- bind_rows(out, tmp_df)
  }

  out
}

s4_q3_plt <- f_s4_q3_by_management(
  df = res$predictions$plot,
  pred_col = "R_dir_pred",
  truth_col = "R_direction",
  dataset_label = "Plot level test set"
)

s4_q3_patch <- f_s4_q3_by_management(
  df = res$predictions$patch,
  pred_col = "R_dir_pred",
  truth_col = "R_direction",
  dataset_label = "Patch level test set"
)

s4_tbl <- bind_rows(
  s4_tbl_plt,
  s4_tbl_patch,
  s4_q3_plt,
  s4_q3_patch
) |>
  mutate(
    across(where(is.numeric), ~ round(.x, digits = 3))
  )

ft_s4 <- s4_tbl |>
  flextable() |>
  theme_booktabs() |>
  flextable::align(align = "center", part = "all") |>
  flextable::autofit() |>
  flextable::bold(part = "header") |> 
  flextable::fit_to_width(max_width = 6.5) # fit to word page width

flextable::save_as_docx(
  ft_s4,
  path = file.path(out_dir, "TableS4_management_performance.docx")
)

write_csv(
  s4_tbl,
  file.path(out_dir, "TableS4_management_performance.csv")
)

message("Wrote Table S4 to: ", out_dir)

# ============================================================
# FIGURE S2
# Predicted vs Observed Severity
# ============================================================

df_figS2 <- res$predictions$plot |>
  filter(manag != "living") |> 
  mutate(prd_sev = pred_sev)

df_figS2_patch <- res$predictions$patch |>
  filter(manag != "living") |> 
  mutate(prd_sev = pred_sev)

f_figS2_plot <- function(data, lvl) {
  ggplot(
    data,
    aes(x = severity, y = prd_sev)
  ) +
    geom_abline(
      slope = 1,
      intercept = 0,
      colour = "grey50"
    ) +
    geom_point(aes(colour = species)) +
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
    theme(aspect.ratio = 1)
}

pS2_plot <- f_figS2_plot(df_figS2, "Plot level")
pS2_patch <- f_figS2_plot(df_figS2_patch, "Patch level")

figS2 <- (pS2_plot / pS2_patch) +
  plot_annotation(tag_levels = "a") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

f_save_plot_bundle(
  plt = figS2,
  out_dir = out_dir,
  stem = "FigureS2",
  width_mm = 120,
  height_mm = 180,
  save_png = save_png_preview
)

# ============================================================
# FIGURE S3
# Observed vs Predicted Reorganization Pathways
# ============================================================

df_S3 <- res$predictions$plot |>
  rename(PrdTrd = R_dir_pred) |>
  mutate(
    R_direction = factor(R_direction, levels = trajectory_levels),
    PrdTrd = factor(PrdTrd, levels = trajectory_levels)
  ) |>
  filter(manag != "living")

df_S3_patch <- res$predictions$patch |>
  rename(PrdTrd = R_dir_pred) |>
  mutate(
    R_direction = factor(R_direction, levels = trajectory_levels),
    PrdTrd = factor(PrdTrd, levels = trajectory_levels)
  ) |> 
  filter(manag != "living")

f_figS3_plot <- function(data, lvl) {
  ggplot(
    data |>
      select(R_direction, PrdTrd, species, manag) |>
      pivot_longer(
        cols = c(R_direction, PrdTrd),
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
        label = ifelse(after_stat(count), after_stat(count), ""),
        group = val
      ),
      position = position_fill(vjust = 0.5),
      size = 3
    ) +
    facet_grid(
      manag ~ species,
      scales = "free_x"
    ) +
    scale_x_discrete(
      "",
      labels = c("Observed", "Predicted")
    ) +
    scale_fill_manual(
      values = c(
        Reassembly = "#fed976",
        Replacement = "#bd0026",
        Resilience = "#a6bddb",
        Restructuring = "#fd8d3c"
      )
    ) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = lvl,
      y = "Percent",
      fill = "Reorganization pathway"
    ) +
    theme_pubr() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

pS3_plot <- f_figS3_plot(df_S3, "Plot level")
pS3_patch <- f_figS3_plot(df_S3_patch, "Patch level")

figS3 <- (pS3_plot / pS3_patch) +
  plot_annotation(tag_levels = "a") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

f_save_plot_bundle(
  plt = figS3,
  out_dir = out_dir,
  stem = "FigureS3",
  width_mm = 200,
  height_mm = 220,
  save_png = save_png_preview
)

# ============================================================
# FIGURE S4
# Structural complexity and groundcover across forest and management
# ============================================================

patch_base <- res$data$patch_test

indicator_grade_cols <- names(patch_base)[grepl("^grade_", names(patch_base))]
indicator_gc_cols <- names(patch_base)[grepl("^gc_", names(patch_base))]
indicator_cols <- c(indicator_grade_cols, indicator_gc_cols)

if (length(indicator_cols) > 0) {
  base_data <- patch_base |>
    select(species, manag, all_of(indicator_cols)) |>
    pivot_longer(
      cols = all_of(indicator_cols),
      names_to = c("prefix", "Indicator"),
      names_pattern = "^(grade|gc)_(.+)$",
      values_to = "val"
    )

  p_grade <- ggplot(
    base_data |> filter(prefix == "grade"),
    aes(x = Indicator, y = val, colour = manag)
  ) +
    theme_bw() +
    geom_boxplot() +
    facet_grid(rows = vars(species), cols = vars(prefix)) +
    labs(y = "Complexity grade (0-5)", x = NULL) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )

  p_gc <- ggplot(
    base_data |> filter(prefix == "gc"),
    aes(x = Indicator, y = val, colour = manag)
  ) +
    theme_bw() +
    geom_boxplot() +
    facet_grid(rows = vars(species), cols = vars(prefix)) +
    labs(y = "Groundcover (%)", x = NULL) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )

  figS4 <- (p_grade + p_gc) +
    plot_layout(widths = c(1, 1), guides = "collect") &
    theme(legend.position = "bottom")

  f_save_plot_bundle(
    plt = figS4,
    out_dir = out_dir,
    stem = "FigureS4",
    width_mm = 120,
    height_mm = 140,
    save_png = save_png_preview
  )
} else {
  warning("Figure S4 skipped: no grade_/gc_ indicator columns found.")
}

# ============================================================
# FIGURE S5
# Relative change in indicators from living to disturbed conditions
# ============================================================

if (length(indicator_cols) > 0 && all(c("trip_n", "species", "manag") %in% names(patch_base))) {
  disturbed_df <- patch_base |>
    filter(manag != "living")

  living_df <- patch_base |>
    filter(manag == "living") |>
    select(trip_n, species, all_of(indicator_cols))

  rel_base <- disturbed_df |>
    select(trip_n, species, manag, all_of(indicator_cols)) |>
    left_join(
      living_df,
      by = c("trip_n", "species"),
      suffix = c("_o", "_l")
    )

  rel_diff <- purrr::map_dfc(
    indicator_cols,
    function(col_nm) {
      tibble(
        !!paste0(col_nm, "_diff") := f_rel_change(
          rel_base[[paste0(col_nm, "_l")]],
          rel_base[[paste0(col_nm, "_o")]]
        )
      )
    }
  )

  dfPrd_reldiff <- bind_cols(
    rel_base |> select(trip_n, species, manag),
    rel_diff
  )

  l_dfPrd_reldiff <- dfPrd_reldiff |>
    pivot_longer(
      cols = ends_with("_diff"),
      names_to = "param",
      values_to = "val"
    )

  figS5 <- ggplot(
    data = l_dfPrd_reldiff,
    aes(x = param, y = val, col = species)
  ) +
    geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
    geom_jitter(alpha = 0.15, show.legend = FALSE) +
    geom_hline(yintercept = 0, linetype = 5, col = "red4") +
    scale_y_continuous(limits = c(-1, 1)) +
    coord_flip() +
    facet_grid(manag ~ species) +
    theme_pubr()

  f_save_plot_bundle(
    plt = figS5,
    out_dir = out_dir,
    stem = "FigureS5",
    width_mm = 120,
    height_mm = 140,
    save_png = save_png_preview
  )
} else {
  warning("Figure S5 skipped: required keys or indicator columns missing.")
}

# ============================================================
# FIGURE S6
# Image label plausibility
# ============================================================

f_tidy_xl <- function(df) {
  corr_cols <- df |>
    select(starts_with("corrected_val_"))

  corr_mat <- as.matrix(corr_cols)

  df_corr <- df |>
    mutate(row_id = row_number()) |>
    pivot_longer(
      cols = starts_with("para"),
      names_to = "param_idx",
      values_to = "param_corrected"
    ) |>
    mutate(
      idx = readr::parse_number(param_idx),
      val_corrected = corr_mat[cbind(row_id, idx)]
    ) |>
    select(row_id, param_corrected, val_corrected)

  df_main <- df |>
    mutate(row_id = row_number()) |>
    pivot_longer(
      cols = starts_with("g"),
      names_to = "param",
      values_to = "val"
    )

  r_df <- df_main |>
    left_join(
      df_corr,
      by = "row_id",
      relationship = "many-to-many"
    ) |>
    filter(param == param_corrected | is.na(param_corrected)) |>
    select(image, param, val, val_corrected) |>
    group_by(image, param) |>
    slice_max(!is.na(val_corrected)) |>
    ungroup()

  unique(r_df)
}

plausibility_dir <- file.path(paper_dir, "1_dataRaw")
co_pl_file <- file.path(plausibility_dir, "compl_plausibilitycheck.xlsx")
gc_pl_file <- file.path(plausibility_dir, "gc_plausibilitycheck.xlsx")

if (file.exists(co_pl_file) && file.exists(gc_pl_file)) {
  co_pl <- readxl::read_xlsx(co_pl_file)
  gc_pl <- readxl::read_xlsx(gc_pl_file)

  l_co <- f_tidy_xl(co_pl |> filter(plausible == "no")) |>
    mutate(
      val_diff = abs(val - val_corrected),
      param = as.factor(param)
    )

  l_gc <- f_tidy_xl(gc_pl |> filter(plausible == "no")) |>
    mutate(
      val_diff = abs(val - val_corrected),
      param = as.factor(param)
    )

  if (nlevels(l_co$param) == 9) {
    l_co$param <- factor(
      l_co$param,
      labels = c(
        "Deadwood",
        "Grasses",
        "Herbs",
        "Vegetation layers",
        "Tree species mixing",
        "Mosses",
        "Shrubs",
        "Stem density",
        "Tree diversity"
      )
    )
  }

  if (nlevels(l_gc$param) == 10) {
    l_gc$param <- factor(
      l_gc$param,
      labels = c(
        "Deadwood",
        "Ferns",
        "Grasses",
        "Herbs",
        "Mature tree",
        "Mosses",
        "Rejuvenation",
        "Rock",
        "Shrubs",
        "Soil/Foliage"
      )
    )
  }

  p_co <- ggplot(
    data = l_co |> filter(!is.na(val_corrected)),
    aes(x = param, y = val_diff, fill = param)
  ) +
    theme_bw() +
    geom_boxplot(outliers = FALSE) +
    geom_jitter(width = 0.25) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(fill = "complexity indicators", x = NULL)

  p_gc <- ggplot(
    data = l_gc |> filter(!is.na(val_corrected)),
    aes(x = param, y = val_diff, fill = param)
  ) +
    theme_bw() +
    geom_boxplot(outliers = FALSE) +
    geom_jitter(width = 0.25) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(fill = "ground cover indicators", x = NULL)

  figS6 <- (p_co / p_gc) +
    plot_annotation(tag_levels = "a") +
    patchwork::plot_layout(guides = "collect") &
    theme(legend.position = "right")

  f_save_plot_bundle(
    plt = figS6,
    out_dir = out_dir,
    stem = "FigureS6",
    width_mm = 120,
    height_mm = 140,
    save_png = save_png_preview
  )
} else {
  warning(
    paste0(
      "Figure S6 skipped. Missing plausibility files: ",
      co_pl_file,
      " or ",
      gc_pl_file
    )
  )
}

# ============================================================
# DONE
# ============================================================

message("Supplement figures and tables written to: ", out_dir)