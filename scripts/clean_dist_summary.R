## Date: 2020-01-12
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
  
  my_file_test <- read_excel(path = path,
                        sheet = "Summary", skip = 0, col_types = "text") %>%
    clean_names() %>%
    remove_empty(which = c("rows", "cols")) %>%
    pull(var = 2) %>%
    .[1]
  
  my_file_test <- ifelse(is.na(my_file_test), "special", my_file_test)

  if(my_file_test == "(Per Capita)") {
    read_excel(path = path, sheet = "Summary", skip = 0, col_types = "text") %>%
      clean_names() %>%
      remove_empty(which = c("rows", "cols")) %>%
      rename(municipality = 2, county = 1) %>%
      mutate(municipality = str_to_title(municipality)) %>%
      filter(municipality != "Total") %>%
      mutate(category = case_when(str_detect(municipality, "Per Capita") ~ "Per Capita",
                                  str_detect(municipality, "Ad Valorem") ~ "Ad Valorem")) %>%
      mutate(municipality = case_when(str_detect(municipality, "Per Capita") == TRUE ~ county,
                                      str_detect(municipality, "Ad Valorem") == TRUE ~ county,
                                      str_detect(municipality, "Per Capita") != TRUE ~ municipality,
                                      str_detect(municipality, "Ad Valorem") != TRUE ~ municipality)) %>%
      mutate(type = case_when(is.na(category) != TRUE ~ "County",
                              is.na(category) == TRUE ~ "Municipality")) %>%
      fill(category, county, .direction = "down") %>%
      mutate(municipality = str_remove(municipality, "\\*")) %>%
      select(-total) %>%
      mutate_at(.vars = 3:11, .funs = ~replace_na(.,"0")) %>%
      mutate(month = month, year = year)
  } else if (my_file_test == "ALAMANCE                           (PER CAPITA)") {
    read_excel(path = path,
               sheet = "Summary", skip = 0, col_types = "text") %>%
      clean_names() %>%
      remove_empty(which = c("rows", "cols")) %>%
      select(-1) %>%
      mutate(municipality = str_to_title(as.character(municipality))) %>%
      filter(municipality != "Total") %>%
      mutate(category = case_when(str_detect(municipality, "Per Capita") ~ "Per Capita",
                                  str_detect(municipality, "Ad Valorem") ~ "Ad Valorem")) %>%
      
      mutate(municipality = str_remove(municipality, "\\(Per Capita\\)")) %>%
      mutate(municipality = str_remove(municipality, "\\(Ad Valorem\\)")) %>%
      mutate(type = case_when(is.na(category) != TRUE ~ "County",
                              is.na(category) == TRUE ~ "Municipality")) %>%
      mutate(county = case_when(is.na(category) != TRUE ~ municipality)) %>%
      fill(category, county, .direction = "down") %>%
      mutate(municipality = str_remove(municipality, "\\*")) %>%
      select(-total) %>%
      mutate_at(.vars = 2:11, .funs = ~replace_na(.,"0")) %>%
      mutate(month = month, year = year) %>%
      select(county, everything())
  } else if (my_file_test == "special") {
    read_excel(path = path,
               sheet = "Summary", skip = 0, col_types = "text") %>%
      clean_names() %>%
      remove_empty(which = c("rows", "cols")) %>%
      select(-1, -2) %>%
      mutate(municipality = str_to_title(as.character(municipality))) %>%
      filter(municipality != "Total") %>%
      mutate(category = case_when(str_detect(municipality, "Per Capita") ~ "Per Capita",
                                  str_detect(municipality, "Ad Valorem") ~ "Ad Valorem")) %>%
      
      mutate(municipality = str_remove(municipality, "\\(Per Capita\\)")) %>%
      mutate(municipality = str_remove(municipality, "\\(Ad Valorem\\)")) %>%
      mutate(type = case_when(is.na(category) != TRUE ~ "County",
                              is.na(category) == TRUE ~ "Municipality")) %>%
      mutate(county = case_when(is.na(category) != TRUE ~ municipality)) %>%
      fill(category, county, .direction = "down") %>%
      mutate(municipality = str_remove(municipality, "\\*")) %>%
      select(-total) %>%
      mutate_at(.vars = 2:11, .funs = ~replace_na(.,"0")) %>%
      mutate(month = month, year = year) %>%
      select(county, everything())
  } else {
    read_excel(path = path, sheet = "Summary", skip = 0, col_types = "text") %>%
      clean_names() %>%
      remove_empty(which = c("rows", "cols")) %>%
      rename(municipality = 2, county = 1) %>%
      mutate(municipality = str_to_title(municipality)) %>%
      filter(municipality != "Total") %>%
      mutate(category = case_when(str_detect(municipality, "Per Capita") ~ "Per Capita",
                                  str_detect(municipality, "Ad Valorem") ~ "Ad Valorem")) %>%
      mutate(municipality = case_when(str_detect(municipality, "Per Capita") == TRUE ~ county,
                                      str_detect(municipality, "Ad Valorem") == TRUE ~ county,
                                      str_detect(municipality, "Per Capita") != TRUE ~ municipality,
                                      str_detect(municipality, "Ad Valorem") != TRUE ~ municipality)) %>%
      mutate(type = case_when(is.na(category) != TRUE ~ "County",
                              is.na(category) == TRUE ~ "Municipality")) %>%
      fill(category, county, .direction = "down") %>%
      mutate(municipality = str_remove(municipality, "\\*")) %>%
      select(-total) %>%
      mutate_at(.vars = 3:11, .funs = ~replace_na(.,"0")) %>%
      mutate(month = month, year = year)
  }
}

# Combine files for collections and refunds ---
summary <- map_df(.x = targets$files,
                   .f = ~f(filename = .x)) %>%
  mutate(date = sprintf("%s/%s/1", str_to_title(month), year)) %>%
  mutate(date = as.Date(date, format = "%B/%Y/%d")) %>%
  mutate(fiscal_year = ifelse(lubridate::month(date) > 6, lubridate::year(date) + 1, lubridate::year(date))) %>%
  mutate_at(.vars = 3:11,
            .funs = as.numeric) %>%
  filter(!municipality %in% c("Per Capita Distributable", "Advalorem Distributable",
                              "Grand Total", "Total Distributable Amount", "Summary Of Amounts", "County/Transit Distributable")) %>%
  mutate_all(.funs = str_trim) %>%
  mutate_all(.funs = ~str_replace_all(., "[\r\n]" , ""))

# Write data ----
write_rds(summary, file = "data/summary.rds")
write_csv(summary, file = "data/summary.csv", na = "", append = FALSE)

