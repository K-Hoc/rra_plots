library(tidyverse)
library(patchwork)

# Plausibility check
co_pl <- readxl::read_xlsx("../../1_dataRaw/compl_plausibilitycheck.xlsx")
gc_pl <- readxl::read_xlsx("../../1_dataRaw/gc_plausibilitycheck.xlsx")

# Function to bring data in right format
f_tidy_xl <- function(df) {
  # ---- 1. Extract corrected-value columns and convert to matrix ----
  corr_cols <- df %>% 
    select(starts_with("corrected_val_")) 
  
  corr_mat <- as.matrix(corr_cols)   # Dynamic matrix for fast indexing
  
  
  # ---- 2. Build corrected-parameter table (long) ----
  df_corr <- df %>%
    mutate(row_id = row_number()) %>%
    
    pivot_longer(
      cols = starts_with("para"),
      names_to = "param_idx",
      values_to = "param_corrected"
    ) %>%
    
    mutate(
      # extract the numeric index from "param_3"
      idx = readr::parse_number(param_idx),
      
      # Dynamic lookup
      val_corrected = corr_mat[cbind(row_id, idx)]
    ) %>%
    
    select(row_id, param_corrected, val_corrected)
  
  
  # ---- 3. Build grade_* table (long) ----
  df_main <- df %>%
    mutate(row_id = row_number()) %>%
    
    pivot_longer(
      cols = starts_with("g"),
      names_to = "param",
      values_to = "val"
    )
  
  
  # ---- 4. Join, match, filter, dedupe ----
  r_df <- df_main %>%
    left_join(df_corr, by = "row_id") %>%
    
    # keep rows where param matches corrected param OR no correction exists
    filter(param == param_corrected | is.na(param_corrected)) %>%
    
    select(image, param, val, val_corrected) %>%
    
    # dedupe by keeping corrected rows first
    group_by(image, param) %>%
    slice_max(!is.na(val_corrected)) %>%
    ungroup()
  
  return(unique(r_df))
}

# Process data
l_co <- f_tidy_xl(co_pl %>% filter(plausible == "no"))
l_gc <- f_tidy_xl(gc_pl %>% filter(plausible == "no"))

# Add delta/difference columns
l_co <- l_co %>% 
  mutate(
    val_diff = abs(val - val_corrected),
    param = as.factor(param)
  )
l_gc <- l_gc %>% 
  mutate(
    val_diff = abs(val - val_corrected),
    param = as.factor(param)
  )

l_co$param <- factor(
  l_co$param,
  labels = c(
    "Deadwood", "Grasses", "Herbs", "Vegetation layers", "Tree species mixing",
    "Mosses", "Shrubs", "Stem density", "Tree diversity"
  )
)
l_gc$param <- factor(
  l_gc$param,
  labels = c(
    "Deadwood", "Ferns", "Grasses", "Herbs", "Mature tree", "Mosses",
    "Rejuvenation", "Rock", "Shrubs", "Soil/Foliage"
    
  )
)


# Plot
p1 <- ggplot(
  data = l_co %>% filter(!is.na(val_corrected)),
  aes(
    x = param,
    y = val_diff,
    fill = param
  )
) +
  theme_bw() +
  geom_boxplot(
    outliers = FALSE
  ) +
  geom_jitter(width = 0.25) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    fill = "complexity indicators",
    x = NULL
  )

p2 <- ggplot(
  data = l_gc %>% filter(!is.na(val_corrected)),
  aes(
    x = param,
    y = val_diff,
    fill = param
  )
) +
  theme_bw() +
  geom_boxplot(
    outliers = FALSE
  ) +
  geom_jitter(width = 0.25) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    fill = "ground cover indicators",
    x = NULL
  )


# Patch plots together
(p1 / p2) +
  plot_annotation(tag_levels = "a") +
  patchwork::plot_layout(guides = "collect") & theme(legend.position = "right")
ggsave(
  filename = "output/sup_fig_6.png",
  scale = 4,
  height = 600, # 1000
  width = 600,
  units = "px"
)
