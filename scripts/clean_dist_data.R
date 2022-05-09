## Date: 2022-05-08
## Author: Jason Jones
## Purpose: Cleaning NC DOR Sales Tax Data for reporting

# Load packages ----
library(readxl)
library(janitor)
library(tidyverse)

# Load targets ----
targets <- tibble(files = list.files(path = "./files/distributions/"))

# Construct read function for collections and refunds ----
f <- function(filename) {
  path = sprintf("files/distributions/%s", filename)
  
  my <- ifelse(str_ends(filename, ".xlsx"), str_remove(filename, ".xlsx"), str_remove(filename, ".xls")) %>%
    str_split(pattern = "-") %>%
    tibble(month = .[[1]][1], year = .[[1]][2]) %>%
    select(-.)
  year <- pull(my, year)
  month <- pull(my, month)
  
  file <- read_excel(path = path,
                     sheet = 3, skip = 3, col_types = "text") %>%
    clean_names() %>%
    remove_empty(which = c("rows", "cols"))
  
  if(length(colnames(file)) == 4) {
    file %>%
      rename(gross_collections = collections_3, net_collections = collections_5) %>%
      filter(tolower(county) != "totals") %>%
      filter(is.na(gross_collections) != TRUE) %>%
      mutate(foreign_collections = "0") %>%
      select(county, gross_collections, foreign_collections, refunds, net_collections) %>%
      mutate(month = month, year = year)
  } else {
    file %>%
      rename(gross_collections = collections_3, foreign_collections = collections_4, net_collections = collections_6) %>%
      filter(tolower(county) != "totals") %>%
      filter(is.na(gross_collections) != TRUE) %>%
      mutate(month = month, year = year)
  }
}

# Combine files for collections and refunds ---
collections_refunds <- map_df(.x = targets$files,
                              .f = ~f(filename = .x)) %>%
  mutate(date = sprintf("%s/%s/1", str_to_title(month), year)) %>%
  mutate(date = as.Date(date, format = "%B/%Y/%d")) %>%
  mutate(fiscal_year = ifelse(lubridate::month(date) > 6, lubridate::year(date) + 1, lubridate::year(date))) %>%
  mutate_at(.vars = c("gross_collections", "foreign_collections", "refunds", "net_collections"),
            .funs = as.numeric)


# Write data ----
write_rds(collections_refunds, file = "data/collections_refunds.rds")
write_csv(collections_refunds, file = "data/collections_refunds.csv", na = "", append = FALSE)

