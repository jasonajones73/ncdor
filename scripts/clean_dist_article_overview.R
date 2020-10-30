## Date: 2020-01-11
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
            sheet = "Article Overview", skip = 0, col_types = "text") %>%
    clean_names() %>%
    remove_empty(which = c("rows", "cols")) %>%
    select(-1) %>%
    mutate(month = month, year = year) %>%
    mutate_at(.vars = 3:10, .funs = ~replace_na(.,"0"))
}

# Combine files for collections and refunds ---
article_overview <- map2_df(.x = targets$month,
                            .y = targets$year,
                            .f = ~f(month = .x, year = .y)) %>%
  mutate(date = sprintf("%s/%s/1", str_to_title(month), year)) %>%
  mutate(date = as.Date(date, format = "%B/%Y/%d")) %>%
  mutate(fiscal_year = ifelse(lubridate::month(date) > 6, lubridate::year(date) + 1, lubridate::year(date))) %>%
  mutate_at(.vars = 3:10,
            .funs = as.numeric)

# Write data ----
write_rds(article_overview, file = "data/article_overview.rds")
write_csv(article_overview, file = "data/article_overview.csv", na = "", append = FALSE)

