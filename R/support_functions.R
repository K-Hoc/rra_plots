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
  dfFieldData <- read_csv(file = "../reorg_full.csv") %>% 
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
