# Function to get the directory with the newest date (as directory name)
get_newest_directory <- function(base_path) {
  # List all directories in the base path
  dir_list <- list.dirs(base_path, full.names = TRUE, recursive = FALSE)
  
  # Remove the base path from the list (since list.dirs adds the base_path itself)
  dir_list <- dir_list[dir_list != base_path]
  
  # Extract the part of the directory name that corresponds to the date
  # Assuming the date is in the format YYYYMMDD-HHMMSS and is part of the folder name
  date_extracted <- gsub(".*/([0-9]{8}-[0-9]{6})$", "\\1", dir_list)
  
  # Convert the extracted date strings to Date objects (this assumes the format is correct)
  dates <- as.POSIXct(date_extracted, format = "%Y%m%d-%H%M%S", tz = "UTC")
  
  # Find the index of the newest directory
  newest_index <- which.max(dates)
  
  # Return the full path to the newest directory
  return(dir_list[newest_index])
}
# # Example usage
# base_path <- "output/complexity"
# newest_dir <- get_newest_directory(base_path)
# 
# # Print the path with the newest directory appended
# new_path <- file.path(base_path, newest_dir)
# cat("Newest directory added to the path:", new_path, "\n")


# Metric calculation functions ----
f_MAE <- function(error) {
  return(mean(abs(error)))
}
f_MSE <- function(error) {
  return(mean((error)^2))
}
f_RMSE <- function(error) {
  return(sqrt(mean(error^2)))
}
f_R2 <- function(prediction, groundtruth) {
  TSS <- sum((groundtruth - mean(groundtruth))^2) # total sum of squares - sum of squared differences between observed values and their mean
  RSS <- sum((groundtruth - prediction)^2) # residual sum of squares - sum of squared differences between observed and predicted values
  R2 <- 1 - (RSS / TSS)
  return(R2)
}


# Load latest predict file and combine with field data (severity, development trajectory) ----
f_load_and_combine <- function(img_feat = FALSE) {
  # -------------- DATA PREPARATION ----------------
  if (img_feat == TRUE) {
    dfImg_feat <- read_csv(
      # file = "../output/2026-06-10-1213_img_features.csv"
      file = "~/edfm/private/Paper_1/2_work/output/2026-06-10-1213_img_features.csv"
    ) %>% 
      mutate(
        image_path = tools::file_path_sans_ext(basename(image_path))
      ) %>% 
    # split filename into triplet, species, managemtn, subplot and orientation
      separate_wider_delim(
        image_path, delim = "_",
        names = c("trip_n","species","manag","sub_plt","orient")
      ) %>% 
      mutate(
        species = as.factor(species),
        manag = as.factor(manag),
        orient = as.factor(orient)
      )
    levels(dfImg_feat$manag) <- c("cleared","dead","living")
    dfImg_feat <- dfImg_feat |> select(
      trip_n, species, manag, sub_plt, orient,
      1:8, 11:24
    )
  } else {
    strCoDNNPath <- normalizePath(
      path = get_newest_directory(file.path("../output/complexity/xception")),
      winslash = "/"
    )
    strGcDNNPath <- normalizePath(
      path = get_newest_directory(file.path("../output/groundcover/xception/")),
      winslash = "/"
    )
    # 1st loading image features and prepare dataframe
    dfGC <- read_csv(file = file.path(strGcDNNPath, "predictions.csv")) %>% 
      mutate(
        image_path = tools::file_path_sans_ext(basename(image_path))
      ) %>% 
      separate_wider_delim(
        image_path,
        delim = "_",
        names = c("trip_n", "species", "manag", "sub_plt", "orient")
      ) %>% 
      mutate(
        species = as.factor(species),
        manag = as.factor(manag),
        orient = as.factor(orient)
      )
    levels(dfGC$manag) <- c("cleared", "dead", "living")
    dfCO <- read_csv(file = file.path(strCoDNNPath, "predictions.csv")) %>% 
      mutate(
        image_path = tools::file_path_sans_ext(basename(image_path))
      ) %>% 
      separate_wider_delim(
        image_path,
        delim = "_",
        names = c("trip_n", "species", "manag", "sub_plt", "orient")
      ) %>% 
      mutate(
        species = as.factor(species),
        manag = as.factor(manag),
        orient = as.factor(orient)
      )
    levels(dfCO$manag) <- c("cleared","dead","living")
  }
  # 2nd construct field data dataframe
  # 2.1 load and prepare disturbance severity estimations
  dfFieldSev <- read_csv(file = "data_franconia/BA_severity_estimation.csv")
  dfFieldSev$trip_n <- as.factor(dfFieldSev$trip_n)
  dfFieldSev$dom_sp <- as.factor(dfFieldSev$dom_sp)
  dfFieldSev[dfFieldSev$managed <= 0,]$managed <- 0
  dfFieldSev[dfFieldSev$unmanaged <= 0,]$unmanaged <- 0

  l_dfFieldSev <- dfFieldSev %>% 
    select(trip_n, dom_sp, managed, unmanaged) %>%
    pivot_longer(
      cols = c(managed, unmanaged),
      names_to = "manag",
      values_to = "severity"
    )
  l_dfFieldSev[l_dfFieldSev$manag == "managed",]$manag <- "cleared"
  l_dfFieldSev[l_dfFieldSev$manag == "unmanaged",]$manag <- "dead"
  l_dfFieldSev$manag <- factor(
    l_dfFieldSev$manag,
    levels = c("cleared", "dead", "living")
  )
  l_dfFieldSev$severity <- abs(l_dfFieldSev$severity)

  # 2.2 load and prepare reorganization pathway
  load(file = "../../1_dataRaw/out_reorg_full_v3/out_reorg_full.Rdata")
  dfFieldData <- out_reorg_full
  #dfFieldData <- read_csv(file = "../reorg_full.csv") %>% 

  dfFieldData <- dfFieldData %>%
    select(trip_n, manag, species = dom_sp, R_direction) %>% 
    mutate(
      trip_n  = as.factor(trip_n),
      manag   = as.factor(manag),
      species = as.factor(species),
      R_direction = as.factor(R_direction)
    )
  levels(dfFieldData$manag) <- c("cleared","dead","living")

  # 2.3 join severity and reorganization pathway
  dfField <- inner_join(
    x = dfFieldData,
    y = l_dfFieldSev %>% select(-dom_sp),
    by = join_by(trip_n, manag)
  )

  # 2.4 combine groundcover and context
  if (img_feat == TRUE) {
    df <- left_join(
      x = dfImg_feat, # %>% select(-species),
      y = dfField %>% select(-species),
      by = join_by(trip_n, manag)
    ) %>% 
      mutate(
        across(severity, ~replace_na(., 0))
      )
  } else {
    dfSubPlts <- inner_join(
      x = dfCO %>% group_by(trip_n, species, manag, sub_plt) %>% 
        select(-orient) %>%
        summarise(across(everything(), mean)) %>% ungroup(),
      y = dfGC %>% group_by(trip_n, species, manag, sub_plt) %>% 
        select(-orient) %>% 
        summarise(across(everything(), mean)) %>% ungroup(),
      by = join_by(trip_n, species, manag, sub_plt)
    )

    # 3rd combine image features and field data
    df <- left_join(
      x = dfSubPlts,
      y = dfField %>% select(-species),
      by = join_by(trip_n, manag)
    )
  }
  
  return(df)
}


# Convert confusion matrix to a table ----
f_confusion_to_metrics <- function(conf_mat, metrics_df = NULL, class_col = "lvl", return_wide = FALSE) {
  
  cm <- as.data.frame.matrix(conf_mat)
  classes <- colnames(cm)
  total <- sum(cm)
  
  overall_accuracy <- sum(diag(as.matrix(cm))) / total

  # ---- compute per-class stats ----
  stats_df <- lapply(classes, function(cls) {
    
    TP <- cm[cls, cls]
    FP <- sum(cm[cls, ]) - TP
    FN <- sum(cm[, cls]) - TP
    TN <- total - TP - FP - FN
    
    precision <- ifelse((TP + FP) == 0, NA, TP / (TP + FP))
    recall    <- ifelse((TP + FN) == 0, NA, TP / (TP + FN))
    f1        <- ifelse(
      is.na(precision) | is.na(recall) | (precision + recall == 0),
      NA,
      2 * precision * recall / (precision + recall)
    )
    
    data.frame(
      class = cls,
      TP = TP,
      FP = FP,
      FN = FN,
      TN = TN,
      support = TP + FN,
      predicted = TP + FP,
      Precision_calc = precision,
      Recall_calc = recall,
      F1_calc = f1,
      summary = paste0(TP, "/", TP + FN)
    )
  }) %>% bind_rows()
  
  
  # ---- merge with metrics_df ----
  if (!is.null(metrics_df)) {
    
    #if (class_col %in% names(metrics_df) && any(metrics_df[[class_col]] %in% stats_df$class)) {
    if ("class" %in% names(metrics_df)) {
      metrics_clean <- metrics_df %>%
        mutate(class = sub("^Class: ", "", class))  # clean labels
      
      combined <- metrics_clean %>%
        left_join(stats_df, by = "class")


      ## Case A: join by class
      #combined <- metrics_df %>%
      #  left_join(stats_df, by = setNames("class", class_col))
      
    } else {
      # Case B: expand (cartesian)
      stats_expanded <- stats_df[rep(1:nrow(stats_df), each = nrow(metrics_df)), ]
      metrics_expanded <- metrics_df[rep(1:nrow(metrics_df), times = nrow(stats_df)), ]
      
      combined <- bind_cols(metrics_expanded, stats_expanded)
    }
    
  } else {
    combined <- stats_df
  }
  
  combined <- combined |> mutate(Overall_accuracy = overall_accuracy)
  
  # ---- optionally reshape to wide ----
  if (return_wide) {
    
    # detect identifier columns (everything except stats + class)
    id_cols <- setdiff(names(combined),
                       c("class", "TP", "FP", "FN", "TN",
                         "support", "predicted",
                         "Precision_calc", "Recall_calc", "F1_calc",
                         "summary"))
    
    combined <- combined %>%
      pivot_wider(
        id_cols = all_of(id_cols),
        names_from = class,
        values_from = c(TP, FN, FP, TN, support, summary),
        names_glue = "{.value}_{class}"
      )
  }
  
  return(combined)
}


# New Function: Bootstrap Macro-F1
# This must be run on the RAW data, not the confusion matrix
f_calculate_macro_f1_boot <- function(data, truth_col, pred_col, iterations = 500) {
  
  boot_f1_scores <- numeric(iterations)
  
  for (i in 1:iterations) {
    # 1. Resample the original data with replacement
    boot_idx <- sample(nrow(data), replace = TRUE)
    boot_data <- data[boot_idx, ]
    
    # 2. Create temporary factors to ensure levels match even if a class is missing in a sample
    # This is crucial! If a bootstrap sample misses a class, 'confusionMatrix' will error 
    # unless we force the levels to match the original data.
    all_levels <- levels(data[[truth_col]])
    
    # 3. Calculate F1 using your existing logic style (or caret)
    try({
      # We use confusionMatrix mode='everything' to get per-class metrics easily
      cm <- confusionMatrix(
        factor(boot_data[[pred_col]], levels = all_levels),
        factor(boot_data[[truth_col]], levels = all_levels),
        mode = "everything"
      )
      
      # Extract F1 for all classes and take the mean (Macro-F1)
      # We handle NAs in case a class has 0 precision/recall in a sample
      f1_per_class <- cm$byClass[, "F1"]
      boot_f1_scores[i] <- mean(f1_per_class, na.rm = TRUE)
    }, silent = TRUE)
  }
  
  # Remove any failed iterations (NAs)
  boot_f1_scores <- boot_f1_scores[!is.na(boot_f1_scores)]
  
  # 4. Return a summary tibble
  return(tibble(
    Macro_F1 = mean(boot_f1_scores),
    Macro_F1_low = quantile(boot_f1_scores, 0.025),
    Macro_F1_high = quantile(boot_f1_scores, 0.975)
  ))
}


library(gt)

f_generate_supplement_tables_old <- function(
    df,
    group_col,
    id_cols,
    table_prefix = "Table S"
) {

  # ---------------------------
  # Validation
  # ---------------------------
  if (!group_col %in% names(df)) {
    stop(sprintf("Column '%s' not found in dataframe.", group_col))
  }

  # Split dataframe by grouping column
  split_data <- split(df, df[[group_col]])

  # ---------------------------
  # Build tables
  # ---------------------------
  tables_list <- lapply(names(split_data), function(group_name) {

    sub_df <- split_data[[group_name]]

    # ------------------------------------------------
    # Remove columns that are entirely NA
    # ------------------------------------------------
    useful_cols <- names(sub_df)[
      colSums(is.na(sub_df)) < nrow(sub_df)
    ]

    sub_df <- sub_df[, useful_cols, drop = FALSE]

    # ------------------------------------------------
    # Move identifier columns to the front
    # ------------------------------------------------
    existing_ids <- intersect(id_cols, names(sub_df))

    sub_df <- sub_df %>%
      dplyr::select(dplyr::all_of(existing_ids), dplyr::everything())

    # ------------------------------------------------
    # Merge Macro_F1 columns if present
    # ------------------------------------------------
    macro_cols <- grep("^Macro_F1", names(sub_df), value = TRUE)

    if (length(macro_cols) >= 3) {

      sub_df <- sub_df %>%
        dplyr::mutate(
          Macro_F1 = paste0(
            round(as.numeric(.data[[macro_cols[1]]]), 3),
            " (",
            round(as.numeric(.data[[macro_cols[2]]]), 3),
            "-",
            round(as.numeric(.data[[macro_cols[3]]]), 3),
            ")"
          )
        ) %>%
        dplyr::select(
          -dplyr::all_of(macro_cols),
          dplyr::everything(),
          Macro_F1
        )
    }

    # ------------------------------------------------
    # Pretty column labels
    # ------------------------------------------------
    names(sub_df) <- gsub("_", " ", names(sub_df))

    # ------------------------------------------------
    # Create gt table
    # ------------------------------------------------
    gt_table <- sub_df %>%
      gt::gt() %>%
      gt::tab_header(
        title = paste(table_prefix, ":", group_name),
        subtitle = "Performance metrics stratified by class and level"
      ) %>%
      gt::fmt_number(
        columns = where(is.numeric),
        decimals = 3
      ) %>%
      gt::tab_options(
        table.font.size = gt::px(14),
        table.width = gt::pct(100),
        column_labels.border.top.color = "black",
        column_labels.border.bottom.color = "black"
      )

    # ------------------------------------------------
    # Bold Macro F1 column if it exists
    # ------------------------------------------------
    if ("Macro F1" %in% names(sub_df)) {

      gt_table <- gt_table %>%
        gt::tab_style(
          style = gt::cell_text(weight = "bold"),
          locations = gt::cells_body(
            columns = "Macro F1"
          )
        )
    }

    gt_table
  })

  names(tables_list) <- names(split_data)

  return(tables_list)
}


f_generate_supplement_tables <- function(
    df,
    table_prefix = "Table S"
) {

  tables_list <- list()

  # ==================================================
  # TABLE S1: DISTURBANCE DETECTION
  # ==================================================

  df_bin <- df %>%
    dplyr::filter(class == "disturbed/undisturbed") %>%
    dplyr::select(
      lvl,
      summary_disturbed,
      summary_undisturbed,
      Precision,
      Recall,
      F1,
      Overall_accuracy
    ) %>%
    dplyr::rename(
      Level = lvl,
      Disturbed = summary_disturbed,
      Undisturbed = summary_undisturbed,
      Accuracy = Overall_accuracy
    )

  tables_list$disturbance_detection <-
    df_bin %>%
    gt::gt() %>%
    gt::tab_header(
      title = paste0(table_prefix, "1. Disturbance Detection"),
      subtitle = "Binary classification of disturbed versus undisturbed"
    ) %>%
    gt::fmt_percent(
      columns = c(
        Precision,
        Recall,
        F1,
        Accuracy
      ),
      decimals = 1
    )

  # ==================================================
  # TABLE S2: SEVERITY ESTIMATION
  # ==================================================

  df_reg <- df %>%
    dplyr::filter(
      !is.na(MAE) |
      !is.na(RMSE) |
      !is.na(ME)
    ) %>%
    dplyr::select(
      lvl,
      ME,
      ME_sd,
      MAE,
      RMSE,
      MAPE
    ) %>%
    dplyr::rename(
      Level = lvl,
      `Bias (ME)` = ME,
      `Bias SD` = ME_sd
    )

  tables_list$severity_regression <-
    df_reg %>%
    gt::gt() %>%
    gt::tab_header(
      title = paste0(table_prefix, "2. Severity Estimation"),
      subtitle = "Regression performance for disturbance severity (0-100)"
    ) %>%
    gt::fmt_number(
      columns = where(is.numeric),
      decimals = 2
    )

# ==================================================
# TABLE S3: MULTICLASS CLASSIFICATION
# ==================================================

multiclass_levels <- c(
  "Reassembly",
  "Replacement",
  "Restructuring",
  "Resilience"
)

# ------------------------------------------
# Class-specific rows
# ------------------------------------------

df_multi <- df %>%
  dplyr::filter(class %in% multiclass_levels) %>%
  dplyr::mutate(
    F1 = dplyr::if_else(
      is.nan(F1),
      NA_real_,
      F1
    )
  ) %>%
  dplyr::transmute(
    Level = lvl,
    Class = class,
    N = summary,
    Precision = Precision,
    Recall = Recall,
    F1 = ifelse(
      is.na(F1),
      NA_character_,
      sprintf("%.3f", F1)
    ),
    `Balanced Accuracy` = `Balanced Accuracy`
  )

# ------------------------------------------
# Macro F1 summary rows
# ------------------------------------------

macro_rows <- df %>%
  dplyr::filter(!is.na(Macro_F1)) %>%
  dplyr::transmute(
    Level = lvl,
    Class = "Macro F1",
    N = "",
    Precision = NA_real_,
    Recall = NA_real_,
    F1 = sprintf(
      "%.3f (%.3f-%.3f)",
      Macro_F1,
      Macro_F1_low,
      Macro_F1_high
    ),
    `Balanced Accuracy` = NA_real_
  )

# ------------------------------------------
# Combine
# ------------------------------------------

df_multi <- dplyr::bind_rows(
  df_multi,
  macro_rows
)

# ------------------------------------------
# Force row order
# ------------------------------------------

df_multi <- df_multi %>%
  dplyr::mutate(
    Class = factor(
      Class,
      levels = c(
        "Reassembly",
        "Replacement",
        "Restructuring",
        "Resilience",
        "Macro F1"
      )
    )
  ) %>%
  dplyr::arrange(
    Level,
    Class
  )

# ------------------------------------------
# Generate table
# ------------------------------------------

tables_list$multiclass_classification <-
  df_multi %>%
  gt::gt(
    groupname_col = "Level"
  ) %>%
  gt::tab_header(
    title = paste0(
      table_prefix,
      "3. Disturbance Type Classification"
    ),
    subtitle =
      "Multiclass classification of disturbance pathways"
  ) %>%
  gt::fmt_percent(
    columns = c(
      Precision,
      Recall,
      `Balanced Accuracy`
    ),
    decimals = 1
  ) %>%
  gt::sub_missing(
    columns = everything(),
    missing_text = ""
  ) %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_body(
      rows = Class == "Macro F1"
    )
  )

  return(tables_list)
}


f_add15prct_to_oos <- function(dataset, part_test = 0.835, oos_set) {
  # part_test = 0.835 -> because when wanting 15% from a original set but when the dataset
  # only consists of 90% of the original data, the values changes!
  # Create a group_id to later split dataset according to group
  tmp <- dataset %>%
    group_by(trip_n, manag, sub_plt) %>%
    mutate(group_id = cur_group_id()) %>%
    ungroup()
  
  unique_groups <- unique(tmp$group_id)

  # Split unique group_id into train and test
  set.seed(123) # reproductability
  grp_split <- sample(unique(unique_groups))
  split_point <- floor(part_test * length(unique(unique_groups)))
  
  train_groups <- grp_split[1:split_point]
  test_groups <- grp_split[(split_point + 1):length(unique_groups)]
  
  # Assign rows to training or test set, based on group_id
  train_set <- tmp %>%
    filter(group_id %in% train_groups) %>% 
    select(-group_id)

  test_set <- tmp %>% 
    filter(group_id %in% test_groups) %>% 
    select(-group_id)
  
  test_set <- bind_rows(test_set, oos_set)

  return(list(train_set, test_set))
}

f_split_dataset <- function(
    dataset,
    train_fraction = 0.75,
    additional_test = NULL
) {

  tmp <- dataset %>%
    group_by(trip_n, manag, sub_plt) %>%
    mutate(group_id = cur_group_id()) %>%
    ungroup()

  unique_groups <- unique(tmp$group_id)

  set.seed(123)

  grp_split <- sample(unique_groups)

  split_point <- floor(
    train_fraction * length(unique_groups)
  )

  train_groups <- grp_split[1:split_point]

  test_groups <- grp_split[
    (split_point + 1):length(unique_groups)
  ]

  train_set <- tmp %>%
    filter(group_id %in% train_groups) %>%
    select(-group_id)

  test_set <- tmp %>%
    filter(!group_id %in% train_groups) %>%
    select(-group_id)

  if (!is.null(additional_test)) {

    test_set <- bind_rows(
      test_set,
      additional_test
    )
  }

  return(
    list(
      train = train_set,
      test = test_set
    )
  )
}

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