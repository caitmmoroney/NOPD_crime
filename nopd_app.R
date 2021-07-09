# R Shiny App for New Orleans Crime


# 0. Load packages & data

# Issue loading sf & rgdal resolved in GitHub thread: https://github.com/r-spatial/sf/issues/1536
#
# install.packages("rgeos", repos="http://R-Forge.R-project.org", type="source")
# install.packages("rgdal", repos="http://R-Forge.R-project.org", type="source")
# library(devtools)
# install_github("r-spatial/sf", configure.args = "--with-proj-lib=/usr/local/lib/")

# install.packages("RSocrata")
library(RSocrata)
suppressPackageStartupMessages(library(dplyr))
library(stringr)
library(tidyr)
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(sf))
library(httr)

# Load csv files using URL
policereport2021 <- read.csv("https://data.nola.gov/api/views/6pqh-bfxa/rows.csv?accessType=DOWNLOAD")

'
policereport2020 <- read.csv("https://data.nola.gov/api/views/hjbe-qzaz/rows.csv?accessType=DOWNLOAD")
policereport2019 <- read.csv("https://data.nola.gov/api/views/mm32-zkg7/rows.csv?accessType=DOWNLOAD")
policereport2018 <- read.csv("https://data.nola.gov/api/views/3m97-9vtw/rows.csv?accessType=DOWNLOAD")
policereport2017 <- read.csv("https://data.nola.gov/api/views/qtcu-97s9/rows.csv?accessType=DOWNLOAD")
policereport2016 <- read.csv("https://data.nola.gov/api/views/4gc2-25he/rows.csv?accessType=DOWNLOAD")
policereport2015 <- read.csv("https://data.nola.gov/api/views/9ctg-u58a/rows.csv?accessType=DOWNLOAD")
policereport2014 <- read.csv("https://data.nola.gov/api/views/6mst-xjhm/rows.csv?accessType=DOWNLOAD")
policereport2013 <- read.csv("https://data.nola.gov/api/views/je4t-6qub/rows.csv?accessType=DOWNLOAD")
policereport2012 <- read.csv("https://data.nola.gov/api/views/x7yt-gfg9/rows.csv?accessType=DOWNLOAD")
policereport2011 <- read.csv("https://data.nola.gov/api/views/t596-ginn/rows.csv?accessType=DOWNLOAD")
policereport2010 <- read.csv("https://data.nola.gov/api/views/s25y-s63t/rows.csv?accessType=DOWNLOAD")
'

# Load files using RSocrata
pr2021_socrata <- read.socrata("https://data.nola.gov/resource/6pqh-bfxa.json")

'
policereport2020 <- read.socrata("https://data.nola.gov/resource/hjbe-qzaz.json")
policereport2019 <- read.socrata("https://data.nola.gov/resource/mm32-zkg7.json")
policereport2018 <- read.socrata("https://data.nola.gov/resource/3m97-9vtw.json")
policereport2017 <- read.socrata("https://data.nola.gov/resource/qtcu-97s9.json")
policereport2016 <- read.socrata("https://data.nola.gov/resource/4gc2-25he.json")
policereport2015 <- read.socrata("https://data.nola.gov/resource/9ctg-u58a.json")
policereport2014 <- read.socrata("https://data.nola.gov/resource/6mst-xjhm.json")
policereport2013 <- read.socrata("https://data.nola.gov/resource/je4t-6qub.json")
policereport2012 <- read.socrata("https://data.nola.gov/resource/x7yt-gfg9.json")
policereport2011 <- read.socrata("https://data.nola.gov/resource/t596-ginn.json")
policereport2010 <- read.socrata("https://data.nola.gov/resource/s25y-s63t.json")
'

nopd_uof <- read.socrata("https://data.nola.gov/resource/9mnw-mbde.json")

nopd_stopsearch <- read.socrata("https://data.nola.gov/resource/kitu-f4uy.json")

# Geo data
nopd_districts <- read.socrata("https://data.nola.gov/resource/6fjz-7kjs.json")

url_prefix <- parse_url("https://opendata.arcgis.com/datasets")
url <- url_prefix
url$path <- paste(url$path,
                  "bf8f32f8203247b9a6d982e145e5c3da_0.geojson",
                  sep = "/")
request <- build_url(url)
road_centerlines <- st_read(request)

url <- url_prefix
url$path <- paste(url$path,
                  "bf8f32f8203247b9a6d982e145e5c3da_2.geojson",
                  sep = "/")
request <- build_url(url)
alias_st_name <- st_read(request)


# 1. Data cleaning

pr2021_socrata %>%
  mutate(signal_type_category = str_remove(signal_type, "[A-Z]+")) ->
  pr2021_socrata

pr2021_socrata %>%
  filter(persontype == "VICTIM") %>%
  select(item_number, victim_number) %>%
  group_by(item_number) %>%
  mutate(n_victims = length(unique(victim_number))) %>%
  select(item_number, n_victims) %>%
  unique() ->
  victim_nums

pr2021_socrata %>%
  filter(!is.na(offenderid)) %>%
  select(item_number, offenderid) %>%
  group_by(item_number) %>%
  mutate(n_offenders = length(unique(offenderid))) %>%
  select(item_number, n_offenders) %>%
  unique() ->
  offender_nums

pr2021_socrata %>%
  select(item_number, offenderid, offenderstatus) %>%
  filter(offenderstatus == "ARRESTED") %>%
  unique() %>%
  count(item_number, offenderstatus) %>%
  select(-offenderstatus) %>%
  rename(n_arrested = n) ->
  num_arrests

pr2021_socrata <- merge(pr2021_socrata, victim_nums, all = TRUE)
pr2021_socrata <- merge(pr2021_socrata, offender_nums, all = TRUE)
pr2021_socrata <- merge(pr2021_socrata, num_arrests, all = TRUE)

pr2021_socrata %>%
  replace_na(list(n_victims = 0, n_offenders = 0, n_arrested = 0)) ->
  pr2021_socrata

crime_info <- pr2021_socrata %>%
  select(item_number,
         district,
         location,
         disposition,
         signal_type,
         signal_type_category,
         signal_description,
         occurred_date_time,
         n_victims,
         n_offenders,
         n_arrested,
         hate_crime) %>%
  mutate(arrest_made = ifelse(n_arrested >= 1, T, F)) %>%
  unique()

signal_info <- pr2021_socrata %>%
  select(signal_type, signal_description) %>%
  unique()

signal_type_category_map <- crime_info %>%
  select(item_number, signal_type_category) %>%
  merge(y = signal_info, all.x = TRUE, by.x = "signal_type_category", by.y = "signal_type")

no_overall_signal_desc <- signal_type_category_map %>%
  select(signal_type_category, signal_description) %>%
  unique() %>%
  filter(is.na(signal_description))

