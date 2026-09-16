library(openxlsx)

path2data <- paste0("C:/Users/ongph/github/data")
path2save <- paste0("C:/Users/ongph/github/data/dphil_res3")
ch1_out_file <- "catchment/childrenHospital1_2023_outpatients.xlsx"

df_ch1_out <- file.path(path2data, ch1_out_file) |> 
  read.xlsx()

df_ch1_out <- df_ch1_out |>
  mutate(
    across(c(ngaykham, ngaysinh), as_date),
    diaphuong = stringi::stri_trans_nfc(diaphuong)
  )

df_ch1_out |>
  count(tinh = str_remove(diaphuong, ".*,\\s*"), sort = TRUE)

df_ch1_hcm <- df_ch1_out |>
  filter(str_detect(diaphuong, fixed("Hồ Chí Minh")), !is.na(ngaykham))

df_ch1_hcm |>
  summarise(lines = n(), patient_days = n_distinct(mahoso, ngaykham))

df_ch1_day <- df_ch1_hcm |>
  # distinct(mahoso, ngaykham) |>
  count(ngaykham) |>
  complete(ngaykham = seq(min(ngaykham), max(ngaykham), by = "day"), fill = list(n = 0)) |> 
  filter(ngaykham >= "2023-01-01")

saveRDS(df_ch1_day, file.path(path2save, "ch1_visit_day.rds"))

df_ch1_week <- df_ch1_hcm |>
  # distinct(mahoso, ngaykham) |>
  count(week = floor_date(ngaykham, "week", week_start = 1)) |>
  filter(week >= "2023-01-02") |> 
  complete(week = seq(min(week), max(week), by = "week"), fill = list(n = 0))

saveRDS(df_ch1_week, file.path(path2save, "ch1_visit_week.rds"))

