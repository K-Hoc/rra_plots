# This script should performa a sensitivity analysis to assess the impact of complexity indicators compared to the groundcover indicators.
library(tidyverse)
library(caret)
library(yardstick)

setwd("~/edfm/private/Paper_1/2_work/R/")
source("support_functions.R")

# DATA PREPARATION ----
strCoDNNPath <- normalizePath(
  path = get_newest_directory(file.path("../output/complexity/xception")),
  winslash = "/"
)
strGcDNNPath <- normalizePath(
  path = get_newest_directory(file.path("../output/groundcover/xception/")),
  winslash = "/"
)

# 1st loading image features and prepare dataframe
dfGC <- read_csv(file = file.path(strGcDNNPath, "predictions.csv")) |> 
  mutate(
    image_path = tools::file_path_sans_ext(basename(image_path))
  ) |> 
  separate_wider_delim(
    image_path,
    delim = "_",
    names = c("trip_n", "species", "manag", "sub_plt", "orient")
  ) |> 
  mutate(
    species = as.factor(species),
    manag = as.factor(manag),
    orient = as.factor(orient)
  )
levels(dfGC$manag) <- c("cleared", "dead", "living")
dfCO <- read_csv(file = file.path(strCoDNNPath, "predictions.csv")) |> 
  mutate(
    image_path = tools::file_path_sans_ext(basename(image_path))
  ) |> 
  separate_wider_delim(
    image_path,
    delim = "_",
    names = c("trip_n", "species", "manag", "sub_plt", "orient")
  ) |> 
  mutate(
    species = as.factor(species),
    manag = as.factor(manag),
    orient = as.factor(orient)
  )
levels(dfCO$manag) <- c("cleared","dead","living")

# 2nd create field data frame
dfFieldSev <- read_csv(file = "data_franconia/BA_severity_estimation.csv")
dfFieldSev$trip_n <- as.factor(dfFieldSev$trip_n)
dfFieldSev$dom_sp <- as.factor(dfFieldSev$dom_sp)
dfFieldSev[dfFieldSev$managed <= 0,]$managed <- 0
dfFieldSev[dfFieldSev$unmanaged <= 0,]$unmanaged <- 0

l_dfFieldSev <- dfFieldSev |> 
  select(trip_n, dom_sp, managed, unmanaged) |> 
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
dfFieldData <- read_csv(file = "../reorg_full.csv") |> 
  select(trip_n, manag, species = dom_sp, R_direction) |> 
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
  y = l_dfFieldSev |> select(-dom_sp),
  by = join_by(trip_n, manag)
)

# 2.4 combine groundcover and context
df_Plts <- inner_join(
  x = dfCO |> group_by(trip_n, species, manag, sub_plt) |> 
    select(-orient) |> 
    summarise(across(everything(), mean)) |> ungroup(),
  y = dfGC |> group_by(trip_n, species, manag, sub_plt) |> 
    select(-orient) |> 
    summarise(across(everything(), mean)) |> ungroup(),
  by = join_by(trip_n, species, manag, sub_plt)
)

# 3rd combine image labels and field data
df <- left_join(
  x = df_Plts,
  y = dfField %>% select(-species),
  by = join_by(trip_n, manag)
) |> 
  mutate(
    disturbed = as.factor(
      ifelse(
        test = manag != "living",
        yes = "disturbed",
        no = "undisturbed"
      )
    ) 
  ) |> 
  rename(
    gc_soil = `gc_soil/foliage`,
    gc_deadwood = `gc_deadwood/stumps`
  )

# 4th gather out of sample test set
df_oos <- df |> filter(
  trip_n == 3 | trip_n == 26 | trip_n == 47 | trip_n == 64
)
df <- df |> filter(
  trip_n != 3 & trip_n != 26 & trip_n != 47 & trip_n != 64
)

# Sensitivity analysis - Importance of indicators ----
# Indicator sets
gc_indis <- c(
  "gc_Mature_Trees",
  "gc_rejuvenation",
  "gc_shrub_layer",
  "gc_mosses",
  "gc_ferns",
  "gc_herb_layer",
  "gc_grasses",
  "gc_soil",
  "gc_rock",
  "gc_deadwood"
)
grad_indis <- c(
  "grade_stand_density",
  "grade_treespecies",
  "grade_shrubs",
  "grade_herbs",
  "grade_grass",
  "grade_moss",
  "grade_deadwood",
  "grade_layers",
  "grade_mixing"
)
y_vars <- c(
  "R_direction", "severity", "disturbed"
)


ctrl <- trainControl(method = "cv", number = 10)
run_models <- function(y_var) {
  both_form <- as.formula(paste(y_var, "~", paste(c(gc_indis, grad_indis), collapse = " + ")))
  gc_form <- as.formula(paste(y_var, "~", paste(gc_indis, collapse = " + ")))
  grad_form <- as.formula(paste(y_var, "~", paste(grad_indis, collapse = " + ")))

  set.seed(161)
  mdl_both <- train(both_form, data = df, method = "rf", trControl = ctrl, na.action = na.omit)
  mdl_gc <- train(gc_form, data = df, method = "rf", trControl = ctrl, na.action = na.omit)
  mdl_grad <- train(grad_form, data = df, method = "rf", trControl = ctrl, na.action = na.omit)

  tibble(
    outcome = y_var,
    model = c("both", "gc", "grad"),
    model_obj = list(mdl_both, mdl_gc, mdl_grad)
  )
}

results_tbl <- map_dfr(y_vars, run_models)

## METRIC EXTRACTION ----
get_metrics <- function(model) {
  res <- model$results

  # Regression models
  if ("RMSE" %in% names(res)) {

    best_row <- res[which.min(res$RMSE), ]

    tibble(
      metric_type = "regression",
      RMSE        = best_row$RMSE,
      Rsq         = best_row$Rsquared,
      Accuracy    = NA_real_,
      Kappa       = NA_real_
    )

  # Classification models
  } else if ("Accuracy" %in% names(res)) {

    best_row <- res[which.max(res$Accuracy), ]

    tibble(
      metric_type = "classification",
      RMSE        = NA_real_,
      Rsq         = NA_real_,
      Accuracy    = best_row$Accuracy,
      Kappa       = best_row$Kappa
    )

  } else {
    stop("Unknown model type")
  }
}

results_metrics <- results_tbl |>
  mutate(metrics = map(model_obj, get_metrics)) |>
  unnest(metrics)
results_metrics

# COMPARE PREDICTOR SETS ----
# Regression comparison
comparison_reg <- results_metrics |> 
  filter(metric_type == "regression") |> 
  select(outcome, model, RMSE) |> 
  pivot_wider(names_from = model, values_from = RMSE) |> 
  mutate(delta = grad - gc)

# Classification comparison
comparison_clf <- results_metrics |> 
  filter(metric_type == "classification") |> 
  select(outcome, model, Accuracy) |> 
  pivot_wider(names_from = model, values_from = Accuracy) |> 
  mutate(delta = grad - gc)

comparison_reg # RMSE
comparison_clf # Accuracy

# VARIABLE IMPORTANCE ----
results_tbl <- results_tbl |> 
  mutate(
    varimp = map(model_obj, ~ varImp(.x)$importance)
  )

varimp_tbl <- results_tbl |> 
  select(outcome, model, varimp) |> 
  mutate(variable = map(varimp, rownames)) |> 
  unnest(c(varimp, variable))

varimp_tbl

# PLOTS ----
ggplot(results_metrics |> filter(metric_type == "regression"),
       aes(x = outcome, y = RMSE, fill = model)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Regression model performance")

ggplot(results_metrics |> filter(metric_type == "classification"),
       aes(x = outcome, y = Accuracy, fill = model)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Classification model performance")

ggplot(varimp_tbl,
       aes(x = reorder(variable, Overall), y = Overall)) +
  geom_col() +
  coord_flip() +
  facet_grid(outcome ~ model, scales = "free") +
  labs(title = "Variable importance")


# Prediction on out of sample ----
evaluate_oos <- function(model, data, outcome) {
  # remove missing rows
  data <- data |> drop_na(all_of(c(all.vars(formula(model)), outcome)))

  preds <- predict(model, newdata = data)

  if (is.numeric(data[[outcome]])) {
    # Regression
    tibble(
      metric_type = "regression",
      RMSE = rmse_vec(truth = data[[outcome]], estimate = preds),
      MAE = mae_vec(truth = data[[outcome]], estimate = preds),
      Rsq = rsq_vec(truth = data[[outcome]], estimate = preds),
      Precision = NA_real_,
      Recall = NA_real_,
      F1 = NA_real_,
      Accuracy = NA_real_,
      Kappa = NA_real_
    )
  } else {
    #Classification
    preds <- as.factor(preds)

    tibble(
      metric_type = "classification",
      RMSE = NA_real_,
      MAE = NA_real_,
      Rsq = NA_real_,
      Precision = precision_vec(truth = data[[outcome]], estimate = preds),
      Recall = recall_vec(truth = data[[outcome]], estimate = preds),
      F1 = f_meas_vec(truth = data[[outcome]], estimate = preds),
      Accuracy = accuracy_vec(truth = data[[outcome]], estimate = preds),
      Kappa = kap_vec(truth = data[[outcome]], estimate = preds)
    )

  }
}

results_oos <- results_tbl |> 
  mutate(
    oos_metrics = map2(model_obj, outcome, ~ evaluate_oos(.x, df_oos, .y))
  ) |> 
  unnest(oos_metrics)
results_oos


comparison_oos_reg <- results_oos |> 
  filter(metric_type == "regression") |> 
  select(outcome, model, RMSE) |> 
  pivot_wider(names_from = model, values_from = RMSE) |> 
  mutate(delta = grad - gc)

comparison_oos_reg

comparison_oos_clf <- results_oos |> 
  filter(metric_type == "classification") |> 
  select(outcome, model, Accuracy) |> 
  pivot_wider(names_from = model, values_from = Accuracy) |> 
  mutate(delta = grad - gc)

comparison_oos_clf

# Regression
ggplot(results_oos |> filter(metric_type == "regression"),
       aes(x = outcome, y = RMSE, fill = model)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "OOS Regression performance")

# Classification
ggplot(results_oos |> filter(metric_type == "classification"),
       aes(x = outcome, y = Accuracy, fill = model)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "OOS Classification performance")



results_conf <- results_tbl |> 
  filter(outcome %in% c("R_direction", "disturbed")) |> 
  mutate(
    conf_mat = map2(model_obj, outcome, ~ {

      truth <- df_oos[[.y]]
      preds <- predict(.x, df_oos)

      preds <- factor(preds, levels = levels(truth))

      caret::confusionMatrix(
        data = preds,
        reference = truth
      )
    })
  )
results_conf

results_conf |> 
  mutate(
    table = map(conf_mat, ~ .x$table)
  ) |> 
  select(outcome, model, table)


lvl_R_direction <- levels(df$R_direction)
lvl_disturbed   <- levels(df$disturbed)

# Create confusion matrix
conf_tbl <- results_conf |> 
  mutate(
    table = map2(conf_mat, outcome, ~ {

      if (is.na(.x)[1]) return(NULL)

      tab <- .x$table

      # choose correct level set
      lvls <- if (.y == "R_direction") lvl_R_direction else lvl_disturbed

      # expand to full matrix
      full_tab <- matrix(
        0,
        nrow = length(lvls),
        ncol = length(lvls),
        dimnames = list(Prediction = lvls, Reference = lvls)
      )

      # fill observed values
      full_tab[rownames(tab), colnames(tab)] <- tab

      as.data.frame(as.table(full_tab))
    })
  ) |> 
  select(outcome, model, table) |> 
  unnest(table)

# Plotting ----
conf_tbl %>%
  filter(outcome == "R_direction") %>%
  ggplot(aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 3) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  facet_wrap(~ model) +
  coord_equal() +
  theme_minimal() +
  labs(title = "Confusion matrix: R_direction") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

conf_tbl %>%
  filter(outcome == "disturbed") %>%
  ggplot(aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 4) +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  facet_wrap(~ model) +
  coord_equal() +
  theme_minimal() +
  labs(title = "Confusion matrix: disturbed")

results_metrics
results_oos
write_csv(results_oos, file = "output/imglab_sensitivity_metrics.csv")
