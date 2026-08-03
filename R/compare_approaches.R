# ============================================================
# compare_approaches.R
# ============================================================
# Purpose:
# Compare all saved analysis approaches across:
#   Q1 disturbance detection
#   Q2 severity estimation
#   Q3 disturbance pathway classification
#
# Expected inputs:
#   output/*.rds
#
# Outputs:
#   output/comparison_q1.csv
#   output/comparison_q2.csv
#   output/comparison_q3.csv
#   output/comparison_ranking.csv
#   output/comparison_all_metrics.csv
# ============================================================

library(tidyverse)
library(patchwork)

# ------------------------------------------------------------
# LOAD ANALYSIS OBJECTS
# ------------------------------------------------------------
setwd("/mnt/private/Paper_1/2_work/R")

files <- list.files(
  path = "output/analyses",
  pattern = "\\.rds$",
  full.names = TRUE
)

analyses <- lapply(
  files,
  readRDS
)

names(analyses) <- gsub(
  "\\.rds$",
  "",
  basename(files)
)

cat(
  "Loaded",
  length(analyses),
  "analysis objects.\n"
)

# ------------------------------------------------------------
# COMBINE ALL SUMMARY METRICS
# ------------------------------------------------------------

df_comp <- bind_rows(

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

write_csv(
  df_comp,
  "output/comparison_all_metrics.csv"
)

# ============================================================
# Q1 COMPARISON
# ============================================================

q1_comp <- df_comp |>
  filter(
    q == "q1"
  ) |>
  select(
    src,
    split_method,
    patch_method,
    lvl,
    Overall_accuracy,
    Precision,
    Recall,
    F1,
    summary_disturbed,
    summary_undisturbed
  ) |>
  arrange(
    desc(F1)
  )

print(q1_comp)

write_csv(
  q1_comp,
  "output/comparison_q1.csv"
)

# ============================================================
# Q2 COMPARISON
# ============================================================

q2_comp <- df_comp |>
  filter(
    q == "q2"
  ) |>
  select(
    src,
    split_method,
    patch_method,
    lvl,
    ME,
    ME_sd,
    MAE,
    RMSE,
    MAPE
  ) |>
  arrange(
    RMSE
  )

print(q2_comp)

write_csv(
  q2_comp,
  "output/comparison_q2.csv"
)

# ============================================================
# Q3 COMPARISON
# ============================================================

q3_macro <- bind_rows(

  lapply(
    analyses,

    function(x) {

      x$metrics$q3_macro_f1 |>
        mutate(
          src = x$metadata$src,
          split_method = x$metadata$split_method,
          patch_method = x$metadata$patch_method
        )

    }

  )

) |>
  arrange(
    desc(Macro_F1)
  )

print(q3_macro)

write_csv(
  q3_macro,
  "output/comparison_q3_macro_f1.csv"
)

# ------------------------------------------------------------
# CLASS-SPECIFIC Q3 RESULTS
# ------------------------------------------------------------

q3_classes <- bind_rows(

  lapply(
    analyses,

    function(x) {

      x$metrics$q3 |>
        mutate(
          src = x$metadata$src,
          split_method = x$metadata$split_method,
          patch_method = x$metadata$patch_method
        )

    }

  )

)

write_csv(
  q3_classes,
  "output/comparison_q3_classes.csv"
)

# ============================================================
# MANUSCRIPT RANKING TABLE
# ============================================================

ranking <- tibble(

  Approach = names(analyses),
  Q1_F1_Plot =
    map_dbl(
      analyses,
      \(x) {

        x$metrics$q1 |>
          filter(
            lvl == "plot"
          ) |>
          pull(F1)

      }
    ),

  Q1_F1_Patch =
    map_dbl(
      analyses,
      \(x) {

        x$metrics$q1 |>
          filter(
            lvl == "patch"
          ) |>
          pull(F1)

      }
    ),

  Q2_RMSE_Plot =
    map_dbl(
      analyses,
      \(x) {

        x$metrics$q2 |>
          filter(
            lvl == "plot"
          ) |>
          pull(RMSE)

      }
    ),

  Q2_RMSE_Patch =
    map_dbl(
      analyses,
      \(x) {

        x$metrics$q2 |>
          filter(
            lvl == "patch"
          ) |>
          pull(RMSE)

      }
    ),

  Q3_MacroF1_Plot =
    map_dbl(
      analyses,
      \(x) {

        x$metrics$q3_macro_f1 |>
          filter(
            lvl == "plot"
          ) |>
          pull(Macro_F1)

      }
    ),
  Q3_MacroF1_SD_Plot = map_dbl(
    analyses,
    \(x) {
      x$metrics$q3_macro_f1 |>
        filter(lvl == "plot") |>
        summarise(
          sd = (Macro_F1_high - Macro_F1_low) / (2 * 1.96)
        ) |>
        pull(sd)
    }
  ),

  Q3_MacroF1_Patch =
    map_dbl(
      analyses,
      \(x) {

        x$metrics$q3_macro_f1 |>
          filter(
            lvl == "patch"
          ) |>
          pull(Macro_F1)

      }
    ),
  Q3_MacroF1_SD_Patch = map_dbl(
    analyses,
    \(x) {
      x$metrics$q3_macro_f1 |>
        filter(lvl == "patch") |>
        summarise(
          sd = (Macro_F1_high - Macro_F1_low) / (2 * 1.96)
        ) |>
        pull(sd)
    }
  )

)

print(ranking)

write_csv(
  ranking,
  "output/comparison_ranking.csv"
)

# ============================================================
# OVERALL METHOD RANKING
# ============================================================
# Higher is better:
#   Q1 F1
#   Q3 Macro-F1
#
# Lower is better:
#   Q2 RMSE
# ============================================================

overall_ranking <- ranking |>
  mutate(

    rank_q1 =
      rank(
        -Q1_F1_Plot,
        ties.method = "average"
      ),

    rank_q2 =
      rank(
        Q2_RMSE_Plot,
        ties.method = "average"
      ),

    rank_q3 =
      rank(
        -Q3_MacroF1_Plot,
        ties.method = "average"
      ),

    mean_rank =
      (rank_q1 +
         rank_q2 +
         rank_q3) / 3

  ) |>
  arrange(
    mean_rank
  )

print(overall_ranking)

write_csv(
  overall_ranking,
  "output/comparison_overall_ranking.csv"
)

# ============================================================
# SUMMARY FOR CONSOLE
# ============================================================

cat("\n")
cat("============================================\n")
cat("BEST APPROACHES\n")
cat("============================================\n")

best_q1 <- overall_ranking$Approach[
  which.max(ranking$Q1_F1_Plot)
]

best_q2 <- overall_ranking$Approach[
  which.min(ranking$Q2_RMSE_Plot)
]

best_q3 <- overall_ranking$Approach[
  which.max(ranking$Q3_MacroF1_Plot)
]

cat("Best Q1 (F1):       ", best_q1, "\n")
cat("Best Q2 (RMSE):     ", best_q2, "\n")
cat("Best Q3 (Macro-F1): ", best_q3, "\n")
cat("\n")

cat(
  "Comparison files written to output/ \n"
)


# ============================================================
# Q1 FIGURE
# ============================================================

p_q1 <- ggplot(
  q1_comp,
  aes(
    x = reorder(src, F1),
    #y = Overall_accuracy,
    y = F1,
    fill = lvl
  )
) +
  geom_col(
    position = position_dodge(width = 0.8)
  ) +
  coord_flip() +
  labs(
    title = "Q1 Disturbance Detection",
    y = "F1 Score",
    x = NULL,
    fill = "Level"
  ) +
  theme_bw()

ggsave(
  filename = "output/comparison_q1_f1.png",
  plot = p_q1,
  width = 8,
  height = 5
)

# ============================================================
# Q2 FIGURE
# ============================================================

p_q2 <- ggplot(
  q2_comp,
  aes(
    x = reorder(src, RMSE),
    y = RMSE,
    fill = lvl
  )
) +
  geom_col(
    position = position_dodge(width = 0.8)
  ) +
  coord_flip() +
  labs(
    title = "Q2 Severity Estimation",
    y = "RMSE",
    x = NULL,
    fill = "Level"
  ) +
  theme_bw()

ggsave(
  filename = "output/comparison_q2_rmse.png",
  plot = p_q2,
  width = 8,
  height = 5
)

# ============================================================
# Q3 FIGURE
# ============================================================

p_q3 <- ggplot(
  q3_macro,
  aes(
    x = reorder(src, Macro_F1),
    y = Macro_F1,
    fill = lvl
  )
) +
  geom_col(
    position = position_dodge(width = 0.8)
  ) +
  coord_flip() +
  labs(
    title = "Q3 Disturbance Pathways",
    y = "Macro F1",
    x = NULL,
    fill = "Level"
  ) +
  theme_bw()

ggsave(
  filename = "output/comparison_q3_macrof1.png",
  plot = p_q3,
  width = 8,
  height = 5
)

# ============================================================
# COMBINED MANUSCRIPT FIGURE
# ============================================================

p_combined <-
  p_q1 /
  p_q2 /
  p_q3 +
  plot_annotation(
    tag_levels = "A"
  )

ggsave(
  filename = "output/comparison_overview.png",
  plot = p_combined,
  width = 10,
  height = 12
)
