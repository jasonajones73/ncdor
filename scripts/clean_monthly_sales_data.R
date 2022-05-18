## Date: 2022-05-08
## Author: Jason Jones
## Purpose: Cleaning NC DOR Sales Tax Data for reporting

# Load packages ----
library(readxl)
library(janitor)
library(tidyverse)

# Load targets ----
targets <- tibble(files = list.files(path = "./files/monthly_sales/"))

# Construct read function for collections and refunds ----
f <- function(filename) {
  path = sprintf("files/monthly_sales/%s", filename)
  
  my <- ifelse(str_ends(filename, ".xlsx"), str_remove(filename, ".xlsx"), str_remove(filename, ".xls")) %>%
    str_remove(., "monthly-sales-") %>%
    str_split(pattern = "-") %>%
    tibble(month = .[[1]][1], year = .[[1]][2]) %>%
    select(-.)
  year <- pull(my, year)
  month <- pull(my, month)
  
  dat <- read_xls(path = path,
                  sheet = "County", skip = 0, trim_ws = TRUE, col_types = "text")
  head_row <- which(dat[,1] == "County")
  dat <- read_xls(path = path,
                  sheet = "County", skip = head_row, trim_ws = TRUE, col_types = "text") %>%
    clean_names() %>%
    remove_empty(which = "rows") %>%
    select(-c(x2, x4, x6, x8, x10))
  part_one <- select(dat, 1:3) %>%
    rename(county = 1, gross_collections = 2, taxable_sales = 3) %>%
    mutate_at(.vars = vars(gross_collections, taxable_sales), .funs = as.numeric) %>%
    filter(is.na(taxable_sales) != TRUE) %>%
    filter(county != "TOTALS") %>%
    filter(is.character(taxable_sales) != TRUE) %>%
    filter(taxable_sales != 0)
  part_two <- select(dat, 4:6) %>%
    rename(county = 1, gross_collections = 2, taxable_sales = 3) %>%
    mutate_at(.vars = vars(gross_collections, taxable_sales), .funs = as.numeric) %>%
    filter(is.na(taxable_sales) != TRUE) %>%
    filter(county != "TOTALS") %>%
    filter(is.character(taxable_sales) != TRUE) %>%
    filter(taxable_sales != 0)
  rbind(part_one, part_two) %>%
    mutate(month = month, year = year)
}

# Combine files for collections and refunds ---
monthly_sales <- map_df(.x = targets$files,
                         .f = ~f(filename = .x)) %>%
  mutate(date = sprintf("%s/%s/1", str_to_title(month), year)) %>%
  mutate(date = as.Date(date, format = "%B/%Y/%d")) %>%
  mutate(fiscal_year = ifelse(lubridate::month(date) > 6, lubridate::year(date) + 1, lubridate::year(date)))

# Write data ----
write_rds(monthly_sales, file = "data/monthly_sales.rds")
write_csv(monthly_sales, file = "data/monthly_sales.csv", na = "", append = FALSE)
