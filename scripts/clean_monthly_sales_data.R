## Date: 2020-01-10
## Author: Jason Jones
## Purpose: Cleaning NC DOR Sales Tax Data for reporting

# Load packages ----
library(readxl)
library(janitor)
library(tidyverse)

# Load targets ----
targets <- read_csv("files/monthly_sales/monthly_sales_targets.csv",
                    col_types = cols(year = col_character())) %>%
  mutate(month = str_to_lower(month))

# Construct read function for collections and refunds ----
f <- function(month, year) {
  path = sprintf("files/monthly_sales/monthly-sales-%s-%s.xls", month, year)
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
    filter(is.character(taxable_sales) != TRUE)
  part_two <- select(dat, 4:6) %>%
    rename(county = 1, gross_collections = 2, taxable_sales = 3) %>%
    mutate_at(.vars = vars(gross_collections, taxable_sales), .funs = as.numeric) %>%
    filter(is.na(taxable_sales) != TRUE) %>%
    filter(county != "TOTALS") %>%
    filter(is.character(taxable_sales) != TRUE)
  rbind(part_one, part_two) %>%
    mutate(month = month, year = year)
}

# Combine files for collections and refunds ---
monthly_sales <- map2_df(.x = targets$month,
                         .y = targets$year,
                         .f = ~f(month = .x, year = .y)) %>%
  mutate(date = sprintf("%s/%s/1", str_to_title(month), year)) %>%
  mutate(date = as.Date(date, format = "%B/%Y/%d")) %>%
  mutate(fiscal_year = ifelse(lubridate::month(date) > 6, lubridate::year(date) + 1, lubridate::year(date)))

# Write data ----
write_rds(monthly_sales, path = "data/monthly_sales.rds")
write_csv(monthly_sales, path = "data/monthly_sales.csv", na = "", append = FALSE)
