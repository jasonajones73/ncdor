## Date: 2020-01-09
## Author: Jason Jones
## Purpose: Cleaning NC DOR Sales Tax Data for reporting

# Load packages ----
library(readxl)
library(janitor)
library(tidyverse)

# Load targets ----
targets <- read_csv("files/sales_and_use/sales_and_use_targets.csv",
                    col_types = cols(year = col_character()))

county_list <- read_csv("files/sales_and_use/county_listing.csv") %>%
  mutate(county = str_to_upper(county))

# Construct read function for collections and refunds ----
f <- function(year) {
  path = sprintf("files/sales_and_use/sales-and-use-fy-%s.xls", year)
  
  sheet_names <- excel_sheets(path) %>%
    tolower() %>%
    str_match(.,"county detail")
  
  sheet_num <- which(is.na(sheet_names) != TRUE)
  
  dat <- read_excel(path = path,
                    sheet = sheet_num, skip = 0, col_types = "text",
                    trim_ws = TRUE) %>%
    clean_names()
  
  target_row <- which(dat[,1] == "County and major business group")[[1]]
  
  dat <- read_excel(path = path,
                    sheet = sheet_num, skip = target_row,
                    col_types = "text", trim_ws = TRUE) %>%
    clean_names() %>%
    remove_empty("rows")
  
  row_deletes <- which(dat[,1] == "County and major business group")
  
  dat <- dat[-c(row_deletes),]
  
  new_format <- ifelse(dat[1,1][[1]] == "ALAMANCE", TRUE, FALSE)
  
  dat <- if(new_format == FALSE) {
    dat[-c(1,2),] %>%
      rename("county_and" = 7)
  } else {
    dat
  }
  
  dat <- if(as.integer(year) > 2010) {
    dat 
  } else if (is.na(dat[1,7][[1]])) {
    dat
  } else {
    dat %>% rename("county_and" = 7)
  }
  
  dat <- dat %>%
    rename(county = 1) %>%
    mutate(county_1 = county %in% county_list$county) %>%
    mutate(county_1 = case_when(county_1 == TRUE ~ county)) %>%
    fill(county_1, .direction = "down")
  
  county_2_index <- which(str_detect(variable.names(dat), "^county_and"))
  
  dat <- dat %>%
    rename(county_2_ref = county_2_index) %>%
    mutate(county_2 = county_2_ref %in% county_list$county) %>%
    mutate(county_2 = case_when(county_2 == TRUE ~ county_2_ref)) %>%
    fill(county_2, .direction = "down")
  
  dat <- if(is.na(dat[1,2][[1]])) {
    dat
  } else if (dat[1,2][[1]] == "$") {
    dat %>% select(-c(2,4,8,10))
  } else {
    dat
  }
  
  dat <- if(length(tbl_vars(dat)) > 10) {
    dat %>%
      select(-c(1,3,5,7,8,10,12))
  } else {
    dat %>% select(-4)
  }
  
  part_one <- select(dat, county_1, 1:3) %>%
    rename(county = 1, categories = 2, gross_collections = 3, taxable_sales_and_purchases = 4) %>%
    mutate_at(.vars = vars(gross_collections, taxable_sales_and_purchases), .funs = ~str_remove_all(., "♣")) %>%
    mutate_at(.vars = vars(gross_collections, taxable_sales_and_purchases), .funs = ~str_remove_all(., "\\)")) %>%
    mutate_at(.vars = vars(gross_collections, taxable_sales_and_purchases), .funs = ~str_replace(., "\\(", "-")) %>%
    mutate_at(.vars = vars(gross_collections, taxable_sales_and_purchases), .funs = ~str_replace(., ",", "")) %>%
    filter(str_detect(gross_collections, "[:alpha:]") != TRUE) %>%
    mutate(categories = case_when(is.na(categories) == TRUE ~ county,
                                  is.na(categories) != TRUE ~ categories)) %>%
    mutate(categories = str_to_title(categories)) %>%
    mutate_at(.vars = vars(gross_collections, taxable_sales_and_purchases), .funs = as.numeric) %>%
    mutate(categories = str_remove_all(categories, "♣"))
  
  part_two <- select(dat, county_2, 4:6) %>%
    rename(county = 1, categories = 2, gross_collections = 3, taxable_sales_and_purchases = 4) %>%
    mutate_at(.vars = vars(gross_collections, taxable_sales_and_purchases), .funs = ~str_remove_all(., "♣")) %>%
    mutate_at(.vars = vars(gross_collections, taxable_sales_and_purchases), .funs = ~str_remove_all(., "\\)")) %>%
    mutate_at(.vars = vars(gross_collections, taxable_sales_and_purchases), .funs = ~str_replace(., "\\(", "-")) %>%
    mutate_at(.vars = vars(gross_collections, taxable_sales_and_purchases), .funs = ~str_replace(., ",", "")) %>%
    filter(str_detect(gross_collections, "[:alpha:]") != TRUE) %>%
    mutate(categories = case_when(is.na(categories) == TRUE ~ county,
                                  is.na(categories) != TRUE ~ categories)) %>%
    mutate(categories = str_to_title(categories)) %>%
    mutate_at(.vars = vars(gross_collections, taxable_sales_and_purchases), .funs = as.numeric) %>%
    mutate(categories = str_remove_all(categories, "♣"))
  
  rbind(part_one, part_two) %>%
    mutate(year = year)
}

# Combine files for collections and refunds ---
sales_and_use <- map_df(.x = targets$year,
                        .f = ~f(year = .x))


# Write data ----
write_rds(sales_and_use, path = "data/sales_and_use.rds")
write_csv(sales_and_use, path = "data/sales_and_use.csv", na = "", append = FALSE)





# Function refinement
# Everything below is for ad-hoc evaluation of function scoping

path = "files/sales_and_use/sales-and-use-fy-2019.xls"

county_list <- read_csv("files/sales_and_use/county_listing.csv") %>%
  mutate(county = str_to_upper(county))

sheet_names <- excel_sheets(path) %>%
  tolower() %>%
  str_match(.,"county detail")

sheet_num <- which(is.na(sheet_names) != TRUE)

dat <- read_excel(path = path,
                  sheet = sheet_num, skip = 0, col_types = "text",
                  trim_ws = TRUE) %>%
  clean_names()

target_row <- which(dat[,1] == "County and major business group")[[1]]

dat <- read_excel(path = path,
                  sheet = sheet_num, skip = target_row,
                  col_types = "text", trim_ws = TRUE) %>%
  clean_names() %>%
  remove_empty("rows")

row_deletes <- which(dat[,1] == "County and major business group")

dat <- dat[-c(row_deletes),]

new_format <- ifelse(dat[1,1][[1]] == "ALAMANCE", TRUE, FALSE)

dat <- if(new_format == FALSE) {
  dat[-c(1,2),] %>%
    rename("county_and" = 7)
} else {
  dat
}

dat <- if(as.integer(year) > 2010) {
  dat 
} else if (is.na(dat[1,7][[1]])) {
  dat
} else {
  dat %>% rename("county_and" = 7)
}

dat <- dat %>%
  rename(county = 1) %>%
  mutate(county_1 = county %in% county_list$county) %>%
  mutate(county_1 = case_when(county_1 == TRUE ~ county)) %>%
  fill(county_1, .direction = "down")

county_2_index <- which(str_detect(variable.names(dat), "^county_and"))

dat <- dat %>%
  rename(county_2_ref = county_2_index) %>%
  mutate(county_2 = county_2_ref %in% county_list$county) %>%
  mutate(county_2 = case_when(county_2 == TRUE ~ county_2_ref)) %>%
  fill(county_2, .direction = "down")

dat <- if(is.na(dat[1,2][[1]])) {
  dat
} else if (dat[1,2][[1]] == "$") {
  dat %>% select(-c(2,4,8,10))
} else {
  dat
}

dat <- if(length(tbl_vars(dat)) > 10) {
  dat %>%
    select(-c(1,3,5,7,8,10,12))
} else {
  dat %>% select(-4)
}

part_one <- select(dat, county_1, 1:3) %>%
  rename(county = 1, categories = 2, gross_collections = 3, taxable_sales_and_purchases = 4) %>%
  mutate_at(.vars = vars(gross_collections, taxable_sales_and_purchases), .funs = ~str_remove_all(., "♣")) %>%
  mutate_at(.vars = vars(gross_collections, taxable_sales_and_purchases), .funs = ~str_remove_all(., "\\)")) %>%
  mutate_at(.vars = vars(gross_collections, taxable_sales_and_purchases), .funs = ~str_replace(., "\\(", "-")) %>%
  mutate_at(.vars = vars(gross_collections, taxable_sales_and_purchases), .funs = ~str_replace(., ",", "")) %>%
  filter(str_detect(gross_collections, "[:alpha:]") != TRUE) %>%
  mutate(categories = case_when(is.na(categories) == TRUE ~ county,
                                is.na(categories) != TRUE ~ categories)) %>%
  mutate(categories = str_to_title(categories)) %>%
  mutate_at(.vars = vars(gross_collections, taxable_sales_and_purchases), .funs = as.numeric) %>%
  mutate(categories = str_remove_all(categories, "♣"))

part_two <- select(dat, county_2, 4:6) %>%
  rename(county = 1, categories = 2, gross_collections = 3, taxable_sales_and_purchases = 4) %>%
  mutate_at(.vars = vars(gross_collections, taxable_sales_and_purchases), .funs = ~str_remove_all(., "♣")) %>%
  mutate_at(.vars = vars(gross_collections, taxable_sales_and_purchases), .funs = ~str_remove_all(., "\\)")) %>%
  mutate_at(.vars = vars(gross_collections, taxable_sales_and_purchases), .funs = ~str_replace(., "\\(", "-")) %>%
  mutate_at(.vars = vars(gross_collections, taxable_sales_and_purchases), .funs = ~str_replace(., ",", "")) %>%
  filter(str_detect(gross_collections, "[:alpha:]") != TRUE) %>%
  mutate(categories = case_when(is.na(categories) == TRUE ~ county,
                                is.na(categories) != TRUE ~ categories)) %>%
  mutate(categories = str_to_title(categories)) %>%
  mutate_at(.vars = vars(gross_collections, taxable_sales_and_purchases), .funs = as.numeric) %>%
  mutate(categories = str_remove_all(categories, "♣"))

rbind(part_one, part_two)

