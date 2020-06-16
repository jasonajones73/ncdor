## Date: 2020-01-09
## Author: Jason Jones
## Purpose: Harvest files for Local Gov Distributions from NC DOR

# Load packages ----
library(rvest)
library(httr)
library(tidyverse)

# Read in targets ----
# Note: targets.csv file should be updated to reflect all of the files that
# you would like to capture. Currently Jul 2016 - Nov 2019
targets <- read_csv("files/distributions/dist_targets.csv",
                    col_types = cols(year = col_character())) %>%
  mutate(month = str_to_lower(month))

# Here is where we are updating the target object for new month
# You should change these values for the new month
targets <- targets %>%
  add_row(month = "june", year = "2020")

# This is overwriting your targets.csv file with the new row you have added
write_csv(targets, "files/distributions/dist_targets.csv", append = FALSE)

# Construct function
# Note: This is not how I wanted to construct this function. There is a single
# misspelling in the URL for October 2017 that I could not write error
# handling for. Since it is the only known error, I wrote in an ifelse handle
f <- function(month, year) {
  target_url <- sprintf("https://www.ncdor.gov/documents/sales-use-distribution-%s-%s", month, year)
  ncdor <- read_html(target_url)
  xml_stuff <- html_nodes(ncdor, ".file a") %>%
    xml_attrs()
  doc_link <- ifelse(length(xml_stuff) > 0,
                     xml_stuff[[4]][["href"]],
                     "https://files.nc.gov/ncdor/documents/files/sandu_10-17_2.xlsx")
  GET(url = doc_link, write_disk(path = sprintf("files/distributions/%s-%s.xlsx", month, year), overwrite = TRUE))
}

# Download files
map2(.x = targets$month,
     .y = targets$year,
     .f = ~f(month = .x, year = .y))
