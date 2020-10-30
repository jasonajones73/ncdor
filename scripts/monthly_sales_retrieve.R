## Date: 2020-01-10
## Author: Jason Jones
## Purpose: Harvest files for Monthly Sales from NC DOR

# Load packages ----
library(rvest)
library(httr)
library(tidyverse)

# Read in targets ----
# Note: targets.csv file should be updated to reflect all of the files that
# you would like to capture. Currently Jul 2008 - Oct 2019
targets <- read_csv("files/monthly_sales/monthly_sales_targets.csv",
                    col_types = cols(year = col_character())) %>%
  mutate(month = str_to_lower(month))

# Here is where we are updating the target object for new month
# You should change these values for the new month
targets <- targets %>%
  add_row(month = "september", year = "2020")

# This is overwriting your targets.csv file with the new row you have added
write_csv(targets, "files/monthly_sales/monthly_sales_targets.csv", append = FALSE)

# Construct function
f <- function(month, year) {
  target_url <- sprintf("https://www.ncdor.gov/documents/monthly-state-sales-and-use-tax-statistics-%s-%s", month, year)
  ncdor <- read_html(target_url)
  xml_stuff <- html_nodes(ncdor, ".file a") %>%
    xml_attrs()
  doc_link <- ifelse(length(xml_stuff) > 3,
                     xml_stuff[[4]][["href"]],
                     "https://www.ncdor.gov/documents/monthly-state-sales-and-use-tax-statistics-october-2007")
  GET(url = doc_link, write_disk(path = sprintf("files/monthly_sales/monthly-sales-%s-%s.xls", month, year), overwrite = TRUE))
}

# Download files
map2(.x = targets$month,
     .y = targets$year,
     .f = ~f(month = .x, year = .y))
