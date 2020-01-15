## Date: 2020-01-09
## Author: Jason Jones
## Purpose: Cleaning NC DOR Sales Tax Data for reporting

# Load packages ----
library(readxl)
library(janitor)
library(tidyverse)

# Load targets ----
targets <- read_csv("files/distributions/dist_targets.csv",
                    col_types = cols(year = col_character())) %>%
  mutate(month = str_to_lower(month))

# Construct read function for collections and refunds ----
f <- function(month, year) {
  path = sprintf("files/distributions/%s-%s.xlsx", month, year)
  read_xlsx(path = path,
            sheet = 3, skip = 3, col_types = "text") %>%
    clean_names() %>%
    remove_empty(which = c("rows", "cols")) %>%
    rename(gross_collections = collections_3, foreign_collections = collections_4, net_collections = collections_6) %>%
    filter(tolower(county) != "totals") %>%
    filter(is.na(gross_collections) != TRUE) %>%
    mutate(month = month, year = year)
}

# Combine files for collections and refunds ---
collections_refunds <- map2_df(.x = targets$month,
                               .y = targets$year,
                               .f = ~f(month = .x, year = .y)) %>%
  mutate(date = sprintf("%s/%s/1", str_to_title(month), year)) %>%
  mutate(date = as.Date(date, format = "%B/%Y/%d")) %>%
  mutate(fiscal_year = ifelse(lubridate::month(date) > 6, lubridate::year(date) + 1, lubridate::year(date))) %>%
  mutate_at(.vars = c("gross_collections", "foreign_collections", "refunds", "net_collections"),
            .funs = as.numeric)


# Write data ----
write_rds(collections_refunds, path = "data/collections_refunds.rds")
write_csv(collections_refunds, path = "data/collections_refunds.csv", na = "", append = FALSE)

