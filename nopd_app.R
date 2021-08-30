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

# Load files using RSocrata
socrata_ids <- c(
  "6pqh-bfxa",
  "hjbe-qzaz",
  "mm32-zkg7",
  "3m97-9vtw",
  "qtcu-97s9",
  "4gc2-25he",
  "9ctg-u58a",
  "6mst-xjhm",
  "je4t-6qub",
  "x7yt-gfg9",
  "t596-ginn",
  "s25y-s63t"
)

# this for loop doesn't work; datasets have different # variables
for (i in 1:length(socrata_ids)) {
  dataset_id <- socrata_ids[i]
  fname <- paste0("https://data.nola.gov/resource/", dataset_id, ".json")
  annual_epr <- read.socrata(fname)
  
  if (i == 1) {
    epr <- annual_epr
  } else {
    epr <- merge(epr, annual_epr)
  }
}

# hard-code data loading by year
epr2021 <- read.socrata("https://data.nola.gov/resource/6pqh-bfxa.json")
epr2020 <- read.socrata("https://data.nola.gov/resource/hjbe-qzaz.json")
epr2019 <- read.socrata("https://data.nola.gov/resource/mm32-zkg7.json")
epr2018 <- read.socrata("https://data.nola.gov/resource/3m97-9vtw.json")
epr2017 <- read.socrata("https://data.nola.gov/resource/qtcu-97s9.json")
epr2016 <- read.socrata("https://data.nola.gov/resource/4gc2-25he.json")
epr2015 <- read.socrata("https://data.nola.gov/resource/9ctg-u58a.json")
epr2014 <- read.socrata("https://data.nola.gov/resource/6mst-xjhm.json")
epr2013 <- read.socrata("https://data.nola.gov/resource/je4t-6qub.json")
epr2012 <- read.socrata("https://data.nola.gov/resource/x7yt-gfg9.json")
epr2011 <- read.socrata("https://data.nola.gov/resource/t596-ginn.json")
epr2010 <- read.socrata("https://data.nola.gov/resource/s25y-s63t.json")


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

epr2021 %>%
  mutate(signal_type_category = str_remove(signal_type, "[A-Z]+")) ->
  epr2021

epr2021 %>%
  filter(persontype == "VICTIM") %>%
  select(item_number, victim_number) %>%
  group_by(item_number) %>%
  mutate(n_victims = length(unique(victim_number))) %>%
  select(item_number, n_victims) %>%
  unique() ->
  victim_nums

epr2021 %>%
  filter(!is.na(offenderid)) %>%
  select(item_number, offenderid) %>%
  group_by(item_number) %>%
  mutate(n_offenders = length(unique(offenderid))) %>%
  select(item_number, n_offenders) %>%
  unique() ->
  offender_nums

epr2021 %>%
  select(item_number, offenderid, offenderstatus) %>%
  filter(offenderstatus == "ARRESTED") %>%
  unique() %>%
  count(item_number, offenderstatus) %>%
  select(-offenderstatus) %>%
  rename(n_arrested = n) ->
  num_arrests

epr2021 <- merge(epr2021, victim_nums, all = TRUE)
epr2021 <- merge(epr2021, offender_nums, all = TRUE)
epr2021 <- merge(epr2021, num_arrests, all = TRUE)

epr2021 %>%
  replace_na(list(n_victims = 0, n_offenders = 0, n_arrested = 0)) ->
  epr2021

crime_info <- epr2021 %>%
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

signal_info <- epr2021 %>%
  select(signal_type, signal_description) %>%
  unique()

signal_type_category_map <- crime_info %>%
  select(item_number, signal_type_category) %>%
  merge(y = signal_info, all.x = TRUE, by.x = "signal_type_category", by.y = "signal_type")

# these signal types do not have an overarching signal description
no_overall_signal_desc <- signal_type_category_map %>%
  select(signal_type_category, signal_description) %>%
  unique() %>%
  filter(is.na(signal_description))



# join crime_info with geo data
crime_info %>% # separate intersecting streets from location street when provided
  separate(location, c("location", "intersection"), " & ", fill = "right") ->
  crime_info

crime_info %>%
  mutate(street_suffix = str_extract(location, "[A-Z][a-z]{1,2}( #.+)?$"),
         unit_number = str_extract(street_suffix, "#[^#]+"),
         street_suffix = str_extract(street_suffix, "[A-Za-z]+")) ->
  crime_info



crime_info %>%
  select(street_suffix) %>%
  unique() # Ave, Hwy; Jr, Law, Lee, Ann, Eve, Lis, End, In, Ex

print(paste0("Number of rows in crime_info: ", nrow(crime_info)))
print(paste0("Number of rows in road_centerlines: ", nrow(road_centerlines)))
# test_crime_geo <- merge(crime_info, road_centerlines, all.x = TRUE, by.x = "location", by.y = "FULLNAMEABV")
test_crime_geo <- merge(road_centerlines, crime_info, all.y = TRUE, by.y = "location", by.x = "FULLNAMEABV")
print(paste0("Number of rows in merged df: ", nrow(test_crime_geo)))





