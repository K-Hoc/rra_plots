library(tidyverse)
library(caret)
library(randomForest)

# ============================================================
# DATASET GENERATORS
# ============================================================

f_dataset_imglabels <- function() {

  f_load_and_combine(
    img_feat = FALSE
  )

}

f_dataset_imgfeatures <- function() {

  f_load_and_combine(
    img_feat = TRUE
  ) |>
    group_by(
      trip_n,
      species,
      manag,
      sub_plt
    ) |>
    summarise(
      R_direction = first(R_direction),
      across(where(is.numeric), mean),
      .groups = "drop"
    )

}

# ============================================================
# PATCH CREATION
# ============================================================

f_create_patch_dataset <- function(
    df,
    method = "mean",
    split_seed = 161
) {

  if (method == "mean") {

    return(
      df |>
        select(-sub_plt) |>
        group_by(
          trip_n,
          manag,
          species
        ) |>
        summarise(
          R_direction = first(R_direction),
          across(
            where(is.numeric),
            mean
          ),
          .groups = "drop"
        )
    )

  }

  if (method == "random_plot") {

    set.seed(split_seed)

    return(
      df |>
        group_by(
          trip_n,
          manag,
          species
        ) |>
        slice_sample(n = 1) |>
        ungroup()
    )

  }

  stop("Unknown patch aggregation method.")

}

# ============================================================
# TRAIN TEST PREPARATION
# ============================================================

f_prepare_data <- function(
    df,
    patch_method = "mean",
    split_method = "oos",
    test_fraction = 0.25,
    oos_ids = c(3, 26, 47, 64),
    split_seed = 123
) {
  df_patch <- f_create_patch_dataset(
    df,
    method = patch_method,
    split_seed = split_seed
  )

  if (split_method == "oos") {
    df_oos <- df %>%
      filter(
        trip_n %in% oos_ids
      )

    df_remaining <- df %>%
      filter(
        !trip_n %in% oos_ids
      )

    oos_fraction <- nrow(df_oos) / nrow(df)
    message("oos_fraction: ", oos_fraction, " test_fraction: ", test_fraction)
    if (oos_fraction >= test_fraction) {
      message(
        "OOS fraction (", round(oos_fraction, 3),
        ") exceeds requested test_fraction (", test_fraction,
        "). Using OOS data as the complete test set."
      )
      split <- list(
        train = df_remaining,
        test = df_oos
      )
    } else {
      train_fraction_remaining <- (1 - test_fraction) / (1 - oos_fraction)
      train_fraction_remaining <- min(train_fraction_remaining, 1)
      
      split <- f_split_dataset(
        dataset = df_remaining,
        train_fraction = train_fraction_remaining,
        additional_test = df_oos,
        split_seed = split_seed
      )
    }
  }
  if (split_method == "random") {
    message("Test fraction = ", test_fraction)
    message("\n train_fraction = ", (1 - test_fraction))
    split <- f_split_dataset(
      dataset = df,
      train_fraction = (1 - test_fraction),
      split_seed = split_seed
    )
  }

  return(
    list(
      train = split$train,
      test = split$test,
      patch = df_patch
    )
  )
}

# ============================================================
# DISTURBANCE LABEL
# ============================================================

f_add_disturbance <- function(df) {

  df |>
    mutate(
      disturbed = factor(
        ifelse(
          manag != "living",
          "disturbed",
          "undisturbed"
        )
      )
    )

}

# ============================================================
# Adding reference features
# ============================================================

f_add_reference_features <- function(df) {

  ref_df <- df |> 
    filter(manag == "living") |> 
    group_by(trip_n, species) |> 
    summarise(
      across(where(is.numeric), mean, na.rm = TRUE),
      .groups = "drop"
    ) |> 
    select(
      #trip_n, species, starts_with("g")
      -severity
    )
  
  return_df <- left_join(
    x = df,
    y = ref_df,
    by = c("trip_n", "species"),
    suffix = c("", "_ref")
  )

  message("nrow - df:\n", nrow(df))
  message("nrow - ref:\n", nrow(ref_df))

  return(return_df)
}

# ============================================================
# REGRESSION METRICS
# ============================================================

f_regression_metrics <- function(df) {

  df |>
    filter(
      complete.cases(sev_err)
    ) |>
    summarise(
      ME = mean(sev_err),
      ME_sd = sd(sev_err),
      MAE = mean(abs(sev_err)),
      RMSE = sqrt(mean(sev_err^2)),
      MAPE = mean(
        abs(sev_err / severity)
      ) * 100
    )

}

# ============================================================
# MAIN ANALYSIS
# ============================================================

f_run_analysis <- function(
    dataset,
    src,
    patch_method = "mean",
    split_method = "oos",
    test_fraction = 0.25,
    model_seed = 161,
    split_seed = 123
) {
  message("Running analysis: ", src)

  # Q1 splits
  splits <- f_prepare_data(
    df = dataset,
    patch_method = patch_method,
    split_method = split_method,
    test_fraction = test_fraction,
    split_seed = split_seed
  )

  df <- f_add_disturbance(splits$train)
  df_oos <- f_add_disturbance(splits$test)
  df_patch <- f_add_disturbance(splits$patch)

  # Q2 and Q3 -> adding reference columns
  ref_data <- f_add_reference_features(dataset)
  splits_ref <- f_prepare_data(
    df = ref_data,
    patch_method = patch_method,
    split_method = split_method,
    test_fraction = test_fraction,
    split_seed = split_seed
  )

  df_ref <- f_add_disturbance(splits_ref$train)
  df_oos_ref <- f_add_disturbance(splits_ref$test)
  df_patch_ref <- f_add_disturbance(splits_ref$patch)

  # ==========================================================
  # Q1
  # ==========================================================

  set.seed(model_seed)

  mQ1 <- caret::train(
    disturbed ~ .,
    data = df |>
      select(
        -trip_n,
        -manag,
        -sub_plt,
        -species,
        -R_direction,
        -severity
      ),
    method = "rf",
    trControl = trainControl(
      method = "repeatedcv",
      number = 10,
      repeats = 3
    )
  )

  message("train data used for Q1: \n")
  message(str(
    df |>
      select(
        -trip_n,
        -manag,
        -sub_plt,
        -species,
        -R_direction,
        -severity
      )
  ))

  df_oos_ref$pred_dist <- predict(mQ1, df_oos_ref)
  df_patch_ref$pred_dist <- predict(mQ1, df_patch_ref)

  q1_conf <- confusionMatrix(
    df_oos_ref$pred_dist,
    df_oos_ref$disturbed
  )

  q1_conf_agg <- confusionMatrix(
    df_patch_ref$pred_dist,
    df_patch_ref$disturbed
  )

  # ==========================================================
  # Q2
  # ==========================================================

  message("train data used for Q2: \n")
  message(str(
    df_ref |>
      filter(!is.na(severity)) |>
      select(
        -trip_n,
        -manag,
        -sub_plt,
        -species,
        -R_direction,
        -disturbed
      )
  ))

  set.seed(model_seed)

  mQ2 <- caret::train(
    severity ~ .,
    data = df_ref |>
      filter(!is.na(severity)) |>
      select(
        -trip_n,
        -manag,
        -sub_plt,
        -species,
        -R_direction,
        -disturbed
      ),
    method = "rf",
    trControl = trainControl(
      method = "repeatedcv",
      number = 10,
      repeats = 3
    )
  )

  df_oos_ref$pred_sev <- predict(mQ2, df_oos_ref)
  df_patch_ref$pred_sev <- predict(mQ2, df_patch_ref)

  df_oos_ref$sev_err <- df_oos_ref$pred_sev - df_oos_ref$severity
  df_patch_ref$sev_err <- df_patch_ref$pred_sev - df_patch_ref$severity

  Q2_metrics <- f_regression_metrics(df_oos_ref)
  Q2_metrics_agg <- f_regression_metrics(df_patch_ref)

  # ==========================================================
  # Q3
  # ==========================================================

  message("train data used for Q3: \n")
  message(str(
    df_ref |>
      select(
        -trip_n,
        -manag,
        -sub_plt,
        -species,
        -severity,
        -disturbed
      ) |>
      na.omit()
  ))

  set.seed(model_seed)

  mQ3 <- caret::train(
    R_direction ~ .,
    data = df_ref |>
      select(
        -trip_n,
        -manag,
        -sub_plt,
        -species,
        -severity,
        -disturbed
      ) |>
      na.omit(),
    method = "rf",
    trControl = trainControl(
      method = "repeatedcv",
      number = 10,
      repeats = 3
    )
  )

  df_oos_ref$R_dir_pred <- predict(mQ3, df_oos_ref)
  df_patch_ref$R_dir_pred <- predict(mQ3, df_patch_ref)

  q3_conf <- confusionMatrix(
    df_oos_ref$R_dir_pred,
    df_oos_ref$R_direction,
    mode = "prec_recall"
  )

  q3_conf_agg <- confusionMatrix(
    df_patch_ref$R_dir_pred,
    df_patch_ref$R_direction,
    mode = "prec_recall"
  )

  q3_plt_macro_f1_stats <- f_calculate_macro_f1_boot(
      data = df_oos_ref,
      truth_col = "R_direction",
      pred_col = "R_dir_pred",
      iterations = 1000
    )

  q3_ptch_macro_f1_stats <- f_calculate_macro_f1_boot(
      data = df_patch_ref,
      truth_col = "R_direction",
      pred_col = "R_dir_pred",
      iterations = 1000
    )

  # ==========================================================
  # METRIC TABLES
  # ==========================================================

  df_q1_metr <- bind_rows(
    f_confusion_to_metrics(
      conf_mat = q1_conf$table,
      metrics_df = as_tibble_row(q1_conf$byClass) |> mutate(lvl = "plot", question = "q1"),
      return_wide = TRUE
    ) |> 
      select(
        Precision, Recall, F1, lvl, question, summary_disturbed, summary_undisturbed,
        Overall_accuracy
      ),

    f_confusion_to_metrics(
      conf_mat = q1_conf_agg$table,
      metrics_df = as_tibble_row(q1_conf_agg$byClass) |> mutate(lvl = "patch", question = "q1"),
      return_wide = TRUE
    ) |>
      select(
        Precision, Recall, F1, lvl, question, summary_disturbed, summary_undisturbed,
        Overall_accuracy
      )
  )

  df_q2 <- bind_rows(
    Q2_metrics |> mutate(lvl = "plot", q = "q2"),
    Q2_metrics_agg |> mutate(lvl = "patch", q = "q2")
  )

  df_q3_metr <- bind_rows(
    f_confusion_to_metrics(
      conf_mat = q3_conf$table,
      metrics_df = as_tibble(q3_conf$byClass, rownames = "class") |> mutate(lvl = "plot", q = "q3"),
      return_wide = FALSE
    ) |> 
      select(class, Precision, Recall, F1, `Balanced Accuracy`, lvl, q, summary, Overall_accuracy),

    f_confusion_to_metrics(
      conf_mat = q3_conf_agg$table,
      metrics_df = as_tibble(q3_conf_agg$byClass, rownames = "class") |> mutate(lvl = "patch", q = "q3"),
      return_wide = FALSE
    ) |> 
      select(class, Precision, Recall, F1, `Balanced Accuracy`, lvl, q, summary, Overall_accuracy)
  )

  df_tbl <- bind_rows(
    df_q1_metr |> mutate(class = "disturbed/undisturbed", q = "q1"),
    df_q3_metr,
    df_q2 |> mutate(class = "severity")
  ) |> 
    bind_rows(
      q3_plt_macro_f1_stats |> mutate(lvl = "plot", q = "q3"),
      q3_ptch_macro_f1_stats |> mutate(lvl = "patch", q = "q3")
    )

  # ==========================================================
  # VARIABLE IMPORTANCE
  # ==========================================================

  q1_importance <- varImp(mQ1)
  q2_importance <- varImp(mQ2)
  q3_importance <- varImp(mQ3)

  # ==========================================================
  # RETURN OBJECT
  # ==========================================================

  return(
    list(
      metadata = list(
        src = src,
        patch_method = patch_method,
        split_method = split_method,
        test_fraction = test_fraction,
        n_train = nrow(df),
        n_plot_test = nrow(df_oos),
        n_patch_test = nrow(df_patch),
        oos_ids = if (split_method == "oos") c(3,26,47,64) else NULL,
        model_seed = model_seed,
        split_seed = split_seed,
        run_date = Sys.time(),
        formulas = list(
          q1 = disturbed ~ .,
          q2 = severity ~ .,
          q3 = R_direction ~ .
        ),
        model_settings = list(
          method = "rf",
          cv_method = "repeatedcv",
          folds = 10,
          repeats = 3
        ),
        session_info = utils::sessionInfo()
      ),

      data = list(
        train = df,
        plot_test = df_oos,
        patch_test = df_patch
      ),

      predictions = list(
        plot = df_oos_ref,
        patch = df_patch_ref
      ),

      models = list(
        q1 = mQ1,
        q2 = mQ2,
        q3 = mQ3
      ),

      confusion_matrices = list(
        q1_plot = q1_conf,
        q1_patch = q1_conf_agg,

        q3_plot = q3_conf,
        q3_patch = q3_conf_agg
      ),

      importance = list(
        q1 = q1_importance,
        q2 = q2_importance,
        q3 = q3_importance
      ),

      metrics = list(
        q1 = df_q1_metr,
        q2 = bind_rows(
          Q2_metrics |> mutate(lvl = "plot"),
          Q2_metrics_agg |> mutate(lvl = "patch")
        ),
        q3 = df_q3_metr,
        q3_macro_f1 = bind_rows(
          q3_plt_macro_f1_stats |>
            mutate(lvl = "plot"),

          q3_ptch_macro_f1_stats |>
            mutate(lvl = "patch")
        ),
        summary = df_tbl
      ),

      class_distribution = list(
        train_disturbed = table(df$disturbed),
        plot_test_disturbed = table(df_oos$disturbed),
        patch_disturbed = table(df_patch$disturbed)
      )
    )
  )
}
