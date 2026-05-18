library(tidyverse)

path2data <- paste0("C:/Users/ongph/github/data")

sero_file <- "sero/cleaned/archive/measles_elisa_2022_2024.rds"
sero_all_file <- "measles/all_measles_sero.rds"

# Read df
df_sero <- file.path(path2data, sero_file) |> 
  readRDS() |> 
  rename(
    date_collection = date_collect,
    titer = titre,
    exact_age = age,
    result = pos
  ) |> 
  mutate(
    result = case_when(
      titer < 150 ~ "negative",
      titer >= 150 & titer <= 200 ~ "equivocal",
      titer > 200 ~ "positive",
      .default = NA_character_ # Catches any missing values
    )
  )

# Read another file
df_sero_all <- file.path(path2data, sero_all_file) |> 
  readRDS() |> 
  filter(
    province == "HCMC",
    exact_age <= 15 | age_max <= 15
  )

df_combined <- bind_rows(df_sero, df_sero_all) |>
  group_by(sample_id, date_collection) |>
  arrange(
    # Priority 1: Location data. is.na() returns FALSE
    # FALSE sorts before TRUE, so rows with data jump to the top
    is.na(commune), 
    is.na(district), 
    
    # Priority 2: Lowest titer. 
    # If location status is tied, sort titer ascending (lowest first)
    titer, 
    
    .by_group = TRUE
  ) |>
  # Keep only the top row of each group based on the rules above
  slice(1) |>
  ungroup()

# Final check does df_combined miss any sample_id from df_sero and df_sero_all

# Count IDs in df_sero that are missing from df_combined
length(setdiff(df_sero$sample_id, df_combined$sample_id))

# Count IDs in df_sero_all that are missing from df_combined
length(setdiff(df_sero_all$sample_id, df_combined$sample_id))

saveRDS(df_combined, file.path(path2data, "sero/cleaned/measles_elisa_hcmc_children.rds"))

# tmp <- df_combined |> 
#   filter(exact_age > 15)
# 
# openxlsx::write.xlsx(tmp, "data/check_age.xlsx")
