## Date: 2020-01-11
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
  
  df <- read_excel(path = path,
            sheet = "Article Overview", skip = 0, col_types = "text") %>%
    clean_names() %>%
    remove_empty(which = c("rows", "cols")) %>%
    select(-1) %>%
    mutate(month = month, year = year) %>%
    mutate_at(.vars = 3:10, .funs = ~replace_na(.,"0")) %>%
    mutate(county_name = str_to_title(county_name))
  
  df <- if("art" %in% colnames(df)) { rename(df, article = art) } else { df }
  df <- if("tx_allocation_point_of_sale" %in% colnames(df)) { rename(df, tax_allocation_point_of_sale = tx_allocation_point_of_sale) } else { df }
  df <- if("article_adjustment_3" %in% colnames(df)) { rename(df, article_42_adjustment_2 = article_adjustment_3) } else { df }
  df <- if("tx_allocation_other_2" %in% colnames(df)) { rename(df, tax_allocation_other_1 = tx_allocation_other_2) } else { df }
  df <- if("per_capita_adjustment_gs_105_486_b_5" %in% colnames(df)) { rename(df, gs_105_486_b_adjustment_4 = per_capita_adjustment_gs_105_486_b_5) } else { df }
  df <- if("cost_of_collection_4" %in% colnames(df)) { rename(df, cost_of_collection_3 = cost_of_collection_4) } else { df }
  df <- if("tax_allocation_per_capita_1" %in% colnames(df)) { rename(df, per_capita_adjustments = tax_allocation_per_capita_1) } else { df }
  df
}

# Combine files for collections and refunds ---
article_overview <- map_df(.x = targets$files,
                            .f = ~f(filename = .x)) %>%
  filter(!(is.na(county_name))) %>%
  filter(county_name != "Article Summary Totals (Statewide)") %>%
  filter(county_name != "Rpt Totals") %>%
  filter(county_name != "Total") %>%
  filter(county_name != "Grand") %>%
  mutate(date = sprintf("%s/%s/1", str_to_title(month), year)) %>%
  mutate(date = as.Date(date, format = "%B/%Y/%d")) %>%
  mutate(fiscal_year = ifelse(lubridate::month(date) > 6, lubridate::year(date) + 1, lubridate::year(date))) %>%
  select("county_name", "article", "per_capita_adjustments", "tax_allocation_point_of_sale",
         "tax_allocation_other_1", "total_allocation_before_adjustments", "article_42_adjustment_2", 
         "cost_of_collection_3", "gs_105_486_b_adjustment_4", "gs_105_524_adjustments", 
         "distributable_proceeds", "month", "year", "date", "fiscal_year") %>%
  mutate(gs_105_524_adjustments = replace_na(gs_105_524_adjustments, "0")) %>%
  mutate_at(.vars = 3:11,
            .funs = as.numeric)

# Write data ----
write_rds(article_overview, file = "data/article_overview.rds")
write_csv(article_overview, file = "data/article_overview.csv", na = "", append = FALSE)

