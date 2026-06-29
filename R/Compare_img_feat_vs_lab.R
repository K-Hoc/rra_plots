# ---- Compare image feature performance with indicator performance ----
library(tidyverse)
library(flextable)

# image feature metrics
feat_q1 <- read_csv(file = "R/output/metrics_q1_imgfeat.csv") |> mutate(Task = "Disturbance detection")
feat_q2 <- read_csv(file = "R/output/metrics_q2_imgfeat.csv") |> rename(Question = q)
feat_q3 <- read_csv(file = "R/output/metrics_q3_imgfeat.csv") |> rename(Question = q, Task = class)

# image indicator metrics
lab_q1 <- read_csv(file = "R/output/metrics_q1_imglab.csv") |> mutate(Task = "Disturbance detection")
lab_q2 <- read_csv(file = "R/output/metrics_q2_imglab.csv") |> rename(Question = q)
lab_q3 <- read_csv(file = "R/output/metrics_q3_imglab.csv") |> rename(Question = q, Task = class)

# Combine as far as possible
df_q13 <- bind_rows(feat_q1, lab_q1, feat_q3, lab_q3)
df_q2 <- bind_rows(feat_q2, lab_q2)

# ---- Create Flextables
ft13 <- df_q13 |> 
  flextable() |> 
  theme_booktabs() |> 
  flextable::color(i = seq(2, nrow(df_q13), 2), color = "black", part = "body") |> 
  flextable::bg(i = seq(2, nrow(df_q13), 2), bg = "#F9F9F9", part = "body") |> 
  flextable::hline(i = c(2, 4, 8, 12, 16)) |> 
  flextable::align(align = "center", part = "all") |> 
  flextable::autofit() |> 
  flextable::bold(part = "header")

ft13 <- fit_to_width(ft13, max_width = 11)
ft13

ft2 <- df_q2 |> 
  flextable() |> 
  theme_booktabs() |> 
  flextable::color(i = seq(2, nrow(df_q2), 2), color = "black", part = "body") |> 
  flextable::bg(i = seq(2, nrow(df_q2), 2), bg = "#F9F9F9", part = "body") |> 
  flextable::hline(i = c(2)) |> 
  flextable::align(align = "center", part = "all") |> 
  flextable::autofit() |> 
  flextable::bold(part = "header")
ft2 <- fit_to_width(ft2, max_width = 6.5)
ft2

# ---- Save tables
flextable::save_as_docx(ft13, path = "R/output/comp_Q1Q3.docx", pr_section = officer::prop_section(page_size = page_size(orient = "landscape")))
flextable::save_as_docx(ft2, path = "R/output/comp_Q2.docx")
