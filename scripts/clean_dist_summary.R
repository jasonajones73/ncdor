## Date: 2020-01-12
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
  if(paste(year, month) == "2016 july") {
    read_xlsx(path = path,
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
  } else if (paste(year, month) == "2018 march") {
    read_xlsx(path = path,
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
    read_xlsx(path = path,
              sheet = "Summary", skip = 0, col_types = "text") %>%
      clean_names() %>%
      remove_empty(which = c("rows", "cols")) %>%
      select(-1) %>%
      mutate(municipality = str_to_title(municipality)) %>%
      rename(county = municipality) %>%
      rename(municipality = 2) %>%
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
summary <- map2_df(.x = targets$month,
                   .y = targets$year,
                   .f = ~f(month = .x, year = .y)) %>%
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

