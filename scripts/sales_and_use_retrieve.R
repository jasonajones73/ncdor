## Date: 2020-01-10
## Author: Jason Jones
## Purpose: Harvest files for Sales and Use Tax from NC DOR

# Load packages ----
library(rvest)
library(Rcrawler)
library(tidyverse)
library(httr)
library(lubridate)
library(stringi)

## Extract links from NC DOR Website
LinkExtractor(url = "https://www.ncdor.gov/news/reports-and-statistics/state-sales-and-use-tax-reports-fiscal-year", ExternalLInks = TRUE) -> pages

## Grab internal links and looks for Sales and Use standard naming
tibble(links = pages$InternalLinks) %>%
  filter(str_detect(links, "annual-state-sales-and-use-tax-statistics") | str_detect(links, "/node/")) -> pages

## Cycle through pages and extract links from those pages as well
pages %>%
  mutate(links = map(.x = links, .f = ~LinkExtractor(url = .x, ExternalLInks = TRUE))) -> pages

## Cycle through internal links of each of those pages, filter those pages for
## the links we are looking for, take the second of each of those links for the 
## Excel spreadsheet, and get rid of links from other pages we don't want yet
pages %>%
  mutate(links = map(.x = links, .f = ~tibble(links = .x$InternalLinks))) %>%
  mutate(links = map(.x = links, .f = ~filter(.data = .x, str_ends(links, "open")))) %>%
  mutate(links = map_chr(.x = links, .f = ~.x$links[2])) %>%
  filter(!(is.na(links))) %>%
  mutate(links = str_remove(links, "-amended")) -> test

## Function to download each spreadsheet from State site
f <- function(state_url) {
  x <- str_remove(state_url, "https://www.ncdor.gov/documents/reports/")
  x <- str_remove(str_split_fixed(x, "-", n = 2)[1], "fy")
  x <- ifelse(str_length(x) == 4, x, str_glue("20", as.character(as.numeric(x) + 1)))
  x <- ifelse(str_length(x) == 3, str_glue("200", str_remove(x, "20")), x)
  
  year <- x
  
  map(.x = state_url,
      .f = ~GET(url = .x, write_disk(path = sprintf("files/sales_and_use/sales-and-use-fy-%s.xls", year), overwrite = TRUE)))
}

## Mapping that function to the State web links we now have to download files
map(.x = test$links,
    .f = ~f(.x))
