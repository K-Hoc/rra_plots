library(tidyverse)

setwd("~/edfm/private/Paper_1/2_work/R/")

source("support_functions.R")
source("analysis_engine.R")

# CONFIG ####
analysis_configs <- list(
  list(
    src = "labels_oos_meanpatch",
    dataset_fun = f_dataset_imglabels,
    patch_method = "mean",
    split_method = "oos",
    test_fraction = 0.25
  ),
  list(
    src = "imgfeat_oos_meanpatch",
    dataset_fun = f_dataset_imgfeatures,
    patch_method = "mean",
    split_method = "oos",
    test_fraction = 0.25
  ),
  list(
    src = "imgfeat_random_meanpatch",
    dataset_fun = f_dataset_imgfeatures,
    patch_method = "mean",
    split_method = "random",
    test_fraction = 0.25
  ),
  list(
    src = "labels_oos_randompatch_I",
    dataset_fun = f_dataset_imglabels,
    patch_method = "random_plot",
    split_method = "oos",
    test_fraction = 0.25
  ),
  list(
    src = "labels_oos_randompatch_II",
    dataset_fun = f_dataset_imglabels,
    patch_method = "random_plot",
    split_method = "oos",
    test_fraction = 0.25,
    model_seed = 161,
    split_seed = 420
  ),
  list(
    src = "labels_oos_randompatch_III",
    dataset_fun = f_dataset_imglabels,
    patch_method = "random_plot",
    split_method = "oos",
    test_fraction = 0.25,
    model_seed = 161,
    split_seed = 1312
  ),
  list(
    src = "labels_oos_randompatch_IV",
    dataset_fun = f_dataset_imglabels,
    patch_method = "random_plot",
    split_method = "oos",
    test_fraction = 0.25,
    model_seed = 161,
    split_seed = 42
  ),
  list(
    src = "labels_random_meanpatch",
    dataset_fun = f_dataset_imglabels,
    patch_method = "mean",
    split_method = "random",
    test_fraction = 0.25
  ),
  list(
    src = "labels_oosonly_meanpatch",
    dataset_fun = f_dataset_imglabels,
    patch_method = "mean",
    split_method = "oos",
    test_fraction = 0.05
  )
)

# RUN ANALYSIS ####
analyses <- purrr::map(
  analysis_configs,
  \(cfg) {
    f_run_analysis(
      dataset = cfg$dataset_fun(),
      src = cfg$src,
      patch_method = cfg$patch_method,
      split_method = cfg$split_method,
      test_fraction = cfg$test_fraction,
      model_seed = cfg$model_seed,
      split_seed = cfg$split_seed
    )

  }

)

names(analyses) <- purrr::map_chr(
  analysis_configs,
  "src"
)

# SAVE OBJECTS ####
dir.create(
  "output/analyses",
  showWarnings = FALSE,
  recursive = TRUE
)

purrr::iwalk(
  analyses,
  \(obj, name) {
    saveRDS(
      object = obj,
      file = file.path(
        "output",
        "analyses",
        paste0(
          gsub(" ", "_", name),
          ".rds"
        )
      )
    )
  }
)

# EXPORT COMBINED METRICS ####
all_metrics <- bind_rows(
  lapply(
    analyses,
    \(x) {
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
  all_metrics,
  "output/analyses/all_metrics.csv"
)
