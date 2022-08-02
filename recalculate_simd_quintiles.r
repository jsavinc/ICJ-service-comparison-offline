# Script to recalculate SIMD vigintiles as quintiles

library(tidyverse)
library(readxl)
library(openxlsx)

vigintiles <-
  read_excel(
    path = "../group_characteristics.xlsx",
    sheet = 1,
    range = "A13:D32",
    col_names = c("simd_vigintile","ICJ","Glasgow pre-ICJ", "ROS")
  ) %>%
  mutate(simd_vigintile = as.integer(str_trim(simd_vigintile)))

quintiles_raw <-
  vigintiles %>%
  pivot_longer(cols = 2:4, names_to = "group", values_to = "n_prop") %>%
  mutate(
    n = str_replace(n_prop, pattern = "^(\\d+) \\(.*\\)$", replacement = "\\1") %>% as.integer,
    simd_quintile = floor((simd_vigintile - 1) / 4 ) + 1
  )

quintiles_raw %>% count(simd_vigintile, simd_quintile)

quintiles_clean <-
  quintiles_raw %>%
  group_by(group, simd_quintile) %>%
  summarise(
    n = sum(n),
    .groups = "drop"
  ) %>%
  group_by(group) %>%
  mutate(percent = scales::percent(n / sum(n), accuracy = 0.1))

quintiles_for_print <-
  quintiles_clean %>%
  mutate(n_prop = paste0(n, " (", percent, ")")) %>%
  select(-n, -percent) %>%
  pivot_wider(names_from = group, values_from = n_prop)

## uncomment to copy to clipboard
# quintiles_for_print %>% write.table("clipboard" ,sep = "\t", row.names = FALSE)
