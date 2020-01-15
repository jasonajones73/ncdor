## Date: 2020-01-10
## Author: Jason Jones
## Purpose: Harvest files for Sales and Use Tax from NC DOR

# Load packages ----
library(rvest)
library(httr)
library(tidyverse)

# Read in targets ----
# Note: targets.csv file should be updated to reflect all of the files that
# you would like to capture. Currently Jul 2008 - Oct 2019
targets <- read_csv("files/sales_and_use/sales_and_use_targets.csv",
                    col_types = cols(year = col_character()))

# Here is where we are updating the target object for new month
# You should change this value to the new year
targets <- targets %>%
  add_row(year = "2020")

# This is overwriting your targets.csv file with the new row you have added
write_csv(targets, "files/sales_and_use/sales_and_use_targets.csv", append = FALSE)

# Construct function
f <- function(year) {
  target_url <- sprintf("https://www.ncdor.gov/documents/annual-state-sales-and-use-tax-statistics-fy-%s", year)
  ncdor <- read_html(target_url)
  xml_stuff <- html_nodes(ncdor, ".file a") %>%
    xml_attrs()
  doc_link <- xml_stuff[[4]][["href"]]
  GET(url = doc_link, write_disk(path = sprintf("files/sales_and_use/sales-and-use-fy-%s.xls", year), overwrite = TRUE))
}

# Download files
map(.x = targets$year,
    .f = ~f(year = .x))
