## Date: 2022-05-08
## Author: Jason Jones
## Purpose: Harvest files for Monthly Sales from NC DOR

# Load packages ----
library(rvest)
library(Rcrawler)
library(tidyverse)
library(httr)
library(lubridate)
library(stringi)

## Extract links from NC DOR Website
LinkExtractor(url = "https://www.ncdor.gov/news/reports-and-statistics/monthly-sales-and-use-tax-statistics", ExternalLInks = TRUE) -> pages

## Grab internal links and looks for Sales and Use standard naming
tibble(links = pages$InternalLinks) %>%
  filter(str_detect(links, "monthly-state-sales-and-use-tax-statistics") | str_detect(links, "/node/")) -> pages

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
  mutate(links = str_remove(links, "-amended"))-> test

## Function to download each spreadsheet from State site
f <- function(state_url) {
  x <- str_sub(state_url, 1, -8)
  x <- ifelse(str_ends(x, "-"), str_sub(state_url, 1, -6), x)
  x <- stri_sub(x, -5)
  x <- ifelse(str_starts(x, "-"), str_remove(x, "-"), x)
  x <- ifelse(str_length(x) == 4, paste0("0", x, "-01"), paste0(x, "-01"))
  x <- as.POSIXct(x, format = "%m-%y-%d")
  
  tibble(months = str_to_lower(month.name)) %>%
    mutate(test = str_detect(state_url, months)) %>%
    filter(test == TRUE) %>%
    pull(months) -> month
  
  month <- if(length(month) == 0) {
    str_to_lower(as.character(month(x, label = TRUE, abbr = FALSE)))
  } else {
    month
  }
  
  tibble(years = as.character(seq(2010, year(today())))) %>%
    mutate(test = str_detect(state_url, years)) %>%
    filter(test == TRUE) %>%
    pull(years) -> year
  
  year <- if(length(year) == 0) {
    as.character(year(x))
  } else {
    year
  }
  
  map(.x = state_url,
      .f = ~GET(url = .x, write_disk(path = sprintf("files/monthly_sales/monthly-sales-%s-%s.xls", month, year), overwrite = TRUE)))
}

## Mapping that function to the State web links we now have to download files
map(.x = test$links,
    .f = ~f(.x))
