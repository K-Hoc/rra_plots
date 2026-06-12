# Comparing labelled indicators to general image features
library(tidyverse)
library(caret)

# -------------- DATA PREPARATION ----------------
# 1st loading image features and prepare dataframe
dfImg_feat <- read_csv(file = "../output/2026-06-10-1213_img_features.csv") %>% 
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

# 3rd combine image features and field data
df <- left_join(
  x = dfImg_feat %>% select(-species),
  y = dfField,
  by = join_by(trip_n, manag)
)

# 4th gather out of sample test set
df_oos <- df %>% filter(
  trip_n == 3 | trip_n == 26 | trip_n == 47 | trip_n == 64
)
df <- df %>% filter(
  trip_n != 3 & trip_n != 26 & trip_n != 47 & trip_n != 64
)

# -------------- Q1: disturbance detection ----------------
df_oos$disturbed <- as.factor(
  ifelse(
    test = df_oos$manag != "living",
    yes = "disturbed",
    no = "undisturbed"
  )
) 
df$disturbed <- as.factor(
  ifelse(
    test = df$manag != "living",
    yes = "disturbed",
    no = "undisturbed"
  )
)

set.seed(161)
mQ1 <- caret::train(
  disturbed ~ .,
  data = df %>% select(-trip_n, -manag, -sub_plt, -orient, -species, -R_direction, -severity),
  method = "rf",
  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
)
df_oos$pred_dist <- as.factor(predict(mQ1, df_oos))

q1_conf <- confusionMatrix(df_oos$pred_dist, df_oos$disturbed)
q1_conf$byClass

# -------------- Q2: disturbance severity estimation ----------------
set.seed(161)
# Train a model for prediction
mQ2 <- caret::train(
  severity ~ .,
  data = df %>% filter(!is.na(severity)) %>% 
    select(-trip_n, -manag, -sub_plt, -orient, -species, -R_direction, -disturbed),
  method = "rf",
  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
)
df_oos$pred_sev <- as.numeric(predict(mQ2, df_oos))
df_oos$sev_err <- df_oos$pred_sev - df_oos$severity
ggplot(
  data = df_oos,
  aes(
    x = severity,
    y = pred_sev
  )
) +
  geom_abline(colour = "grey") +
  geom_point(
    aes(colour = species)
  ) +
  labs(
    title = "Plot level",
    x = "Observed severity (%)",
    y = "Predicted severity (%)"
  ) +
  ylim(c(0,100)) +
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
  theme_pubr() +
  theme(aspect.ratio = 1)

Q2_metrics <- df_oos %>% 
  filter(complete.cases(sev_err)) %>% 
  summarise(
    ME = mean(sev_err),
    ME_sd = sd(sev_err),
    MAE  = mean(abs(sev_err)),
    RMSE = sqrt(mean((sev_err)^2)),
    MAPE = mean(abs((sev_err)/severity)) * 100 #Mean Absolute Percentage Error (MAPE)

  )
  
Q2_metrics


# -------------- Q3: post-disturbance development ----------------
set.seed(161)
mQ3 <- caret::train(
  R_direction ~ .,
  data = df %>% select(-trip_n, -manag, -sub_plt, -orient, -species, -severity) %>% na.omit(),
  method = "rf",
  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
)
df_oos$R_dir_pred <- as.factor(predict(mQ3, df_oos))

q3_conf <- confusionMatrix(df_oos$R_dir_pred, df_oos$R_direction, mode = "prec_recall")
q3_conf$byClass

ggplot(
  data = df_oos %>% na.omit() %>% 
    select(R_direction, R_dir_pred, species, manag) %>%
    pivot_longer(
      cols = c(R_direction, R_dir_pred),
      names_to = "param",
      values_to = "val"
    )
) +
  geom_bar(
    aes(
      x = interaction(param, species),
      fill = factor(val)
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
  facet_grid(~species, scales = "free_x") +
  # facet_grid(manag~species, scales = "free_x") +
  scale_x_discrete(
    "", labels = c("Predicted", "Observed"), guide = guide_axis(angle = 45)
  ) +
  labs(
    y = "Percent",
    fill = "Reorganization pathway:"
  ) +
  scale_fill_manual(values = c(
    "Reassembly" = "#fed976",
    "Replacement" = "#bd0026",
    "Resilience" = "#a6bddb",
    "Restructuring" = "#fd8d3c"
  )) +
  scale_y_continuous(labels = c("0", "25%", "50%", "75%", "100%")) +
  theme_pubr()
