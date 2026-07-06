library(tidyverse)
library(caret)
library(purrr)

#----------------------------------
# Helper functions
#----------------------------------

make_disturbed <- function(x) {
  factor(ifelse(x != "living", "disturbed", "undisturbed"))
}

# Convert confusion matrix to tidy metrics
extract_conf_metrics <- function(conf, lvl, q, seed) {
  
  # Class-level metrics
  class_df <- as_tibble(conf$byClass, rownames = "class") %>%
    mutate(
      lvl = lvl,
      question = q,
      seed = seed
    )
  
  # Overall metrics
  overall_df <- as_tibble_row(conf$overall) %>%
    mutate(
      lvl = lvl,
      question = q,
      seed = seed
    )
  
  list(
    class = class_df,
    overall = overall_df
  )
}

# Q2 metrics
calc_q2 <- function(df, lvl, seed) {
  df %>%
    filter(complete.cases(sev_err)) %>%
    summarise(
      ME = mean(sev_err),
      ME_sd = sd(sev_err),
      MAE = mean(abs(sev_err)),
      RMSE = sqrt(mean(sev_err^2)),
      MAPE = mean(abs(sev_err / severity)) * 100
    ) %>%
    mutate(
      lvl = lvl,
      question = "q2",
      seed = seed
    )
}

#----------------------------------
# MAIN FUNCTION
#----------------------------------

run_experiment <- function(df, df_oos, seed) {
  
  set.seed(seed)
  
  # ---- aggregation ----
  df_oos_agg <- df_oos |>
    select(-sub_plt) |>
    group_by(trip_n, manag, species) |>
    slice_sample(n = 1) |>
    ungroup()
  
  # ---- disturbance label ----
  df$disturbed <- make_disturbed(df$manag)
  df_oos$disturbed <- make_disturbed(df_oos$manag)
  df_oos_agg$disturbed <- make_disturbed(df_oos_agg$manag)

  ################################
  # Q1
  ################################
  mQ1 <- train(
    disturbed ~ .,
    data = df %>% select(-trip_n, -manag, -sub_plt, -species, -R_direction, -severity),
    method = "rf",
    trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
  )
  
  df_oos$pred_dist <- predict(mQ1, df_oos)
  df_oos_agg$pred_dist <- predict(mQ1, df_oos_agg)

  q1_plot <- confusionMatrix(df_oos$pred_dist, df_oos$disturbed)
  q1_patch <- confusionMatrix(df_oos_agg$pred_dist, df_oos_agg$disturbed)

  q1_plot_m <- extract_conf_metrics(q1_plot, "plot", "q1", seed)
  q1_patch_m <- extract_conf_metrics(q1_patch, "patch", "q1", seed)

  ################################
  # Q2
  ################################
  mQ2 <- train(
    severity ~ .,
    data = df %>% filter(!is.na(severity)) %>%
      select(-trip_n,-manag,-sub_plt,-species,-R_direction,-disturbed),
    method = "rf",
    trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
  )

  df_oos$pred_sev <- predict(mQ2, df_oos)
  df_oos$sev_err <- df_oos$pred_sev - df_oos$severity

  df_oos_agg$pred_sev <- predict(mQ2, df_oos_agg)
  df_oos_agg$sev_err <- df_oos_agg$pred_sev - df_oos_agg$severity

  q2_plot <- calc_q2(df_oos, "plot", seed)
  q2_patch <- calc_q2(df_oos_agg, "patch", seed)

  ################################
  # Q3
  ################################
  mQ3 <- train(
    R_direction ~ .,
    data = df %>%
      select(-trip_n, -manag, -sub_plt, -species, -severity) %>%
      na.omit(),
    method = "rf",
    trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
  )

  df_oos$R_dir_pred <- predict(mQ3, df_oos)
  df_oos_agg$R_dir_pred <- predict(mQ3, df_oos_agg)

  q3_plot <- confusionMatrix(df_oos$R_dir_pred, df_oos$R_direction)
  q3_patch <- confusionMatrix(df_oos_agg$R_dir_pred, df_oos_agg$R_direction)

  q3_plot_m <- extract_conf_metrics(q3_plot, "plot", "q3", seed)
  q3_patch_m <- extract_conf_metrics(q3_patch, "patch", "q3", seed)

  ################################
  # RETURN ALL
  ################################
  list(
    q1_class = bind_rows(q1_plot_m$class, q1_patch_m$class),
    q1_overall = bind_rows(q1_plot_m$overall, q1_patch_m$overall),
    q2 = bind_rows(q2_plot, q2_patch),
    q3_class = bind_rows(q3_plot_m$class, q3_patch_m$class),
    q3_overall = bind_rows(q3_plot_m$overall, q3_patch_m$overall)
  )
}


#----------------------------------
# LOAD DATA
#----------------------------------
setwd("~/edfm/private/Paper_1/2_work/R")
source("support_functions.R")

df <- f_load_and_combine(img_feat = FALSE)

# Spliting off out of sample part
df_oos <- df |> filter(
  trip_n == 3 | trip_n == 26 | trip_n == 47 | trip_n == 64
)
df <- df |> filter(
  trip_n != 3 & trip_n != 26 & trip_n != 47 & trip_n != 64
)

#----------------------------------
# RUN MULTIPLE TIMES
#----------------------------------

n_runs <- 30

all_runs <- map(1:n_runs, ~ run_experiment(df, df_oos, seed = 100 + .x))

#----------------------------------
# STACK RESULTS
#----------------------------------

q1_class_all  <- map_dfr(all_runs, "q1_class")
q1_overall_all <- map_dfr(all_runs, "q1_overall")

q2_all <- map_dfr(all_runs, "q2")

q3_class_all  <- map_dfr(all_runs, "q3_class")
q3_overall_all <- map_dfr(all_runs, "q3_overall")

#----------------------------------
# SUMMARISE (MEAN + SD)
#----------------------------------

summarise_metrics <- function(df, group_vars) {
  df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      across(
        where(is.numeric),
        list(mean = mean, sd = sd),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
}

# Q1
q1_class_summary <- summarise_metrics(q1_class_all, c("class","lvl","question"))
q1_overall_summary <- summarise_metrics(q1_overall_all, c("lvl","question"))

# Q2
q2_summary <- summarise_metrics(q2_all, c("lvl","question"))

# Q3
q3_class_summary <- summarise_metrics(q3_class_all, c("class","lvl","question"))
q3_overall_summary <- summarise_metrics(q3_overall_all, c("lvl","question"))

#----------------------------------
# OPTIONAL: tidy format (best for plots)
#----------------------------------

q1_long <- q1_class_all %>%
  pivot_longer(
    cols = where(is.numeric),
    names_to = "metric",
    values_to = "value"
  )

q2_long <- q2_all %>%
  pivot_longer(
    cols = c(ME, MAE, RMSE, MAPE),
    names_to = "metric",
    values_to = "value"
  )

q3_long <- q3_class_all %>%
  pivot_longer(
    cols = where(is.numeric),
    names_to = "metric",
    values_to = "value"
  )

write_csv(q1_class_all, file = "output/rng_plt_runs/q1_class_all.csv")
write_csv(q1_class_summary, file = "output/rng_plt_runs/q1_class_summary.csv")
write_csv(q1_long, file = "output/rng_plt_runs/q1_long.csv")
write_csv(q1_overall_all, file = "output/rng_plt_runs/q1_overall_all.csv")
write_csv(q1_overall_summary, file = "output/rng_plt_runs/q1_overall_summary.csv")

write_csv(q2_all, file = "output/rng_plt_runs/q2_all.csv")
write_csv(q2_long, file = "output/rng_plt_runs/q2_long.csv")
write_csv(q2_summary, file = "output/rng_plt_runs/q2_summary.csv")

write_csv(q3_class_all, file = "output/rng_plt_runs/q3_class_all.csv")
write_csv(q3_class_summary, file = "output/rng_plt_runs/q3_class_summary.csv")
write_csv(q3_long, file = "output/rng_plt_runs/q3_long.csv")
write_csv(q3_overall_all, file = "output/rng_plt_runs/q3_overall_all.csv")
write_csv(q3_overall_summary, file = "output/rng_plt_runs/q3_overall_summary.csv")