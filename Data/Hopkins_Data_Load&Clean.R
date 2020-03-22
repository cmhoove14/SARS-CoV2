require(tidyverse)
require(lubridate)

# Data from https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv
cases_raw <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"))

deaths_raw <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"))

rcvrd_raw <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"))

today <- paste0("X", month(Sys.Date()), ".", day(Sys.Date())-1, ".20")

cases <- cases_raw %>% 
  pivot_longer(X1.22.20:all_of(today),
               names_to = "Date",
               values_to = "Cases") %>% 
  mutate(Date = as.Date(Date %>% 
           str_remove("X") %>% 
           str_replace_all("\\.", "-") %>% 
           str_c(., "20"), format = "%m-%d-%Y"))

deaths <- deaths_raw %>% 
  pivot_longer(X1.22.20:all_of(today),
               names_to = "Date",
               values_to = "Deaths") %>% 
  mutate(Date = as.Date(Date %>% 
           str_remove("X") %>% 
           str_replace_all("\\.", "-") %>% 
           str_c(., "20"), format = "%m-%d-%Y"))

rcvrd <- rcvrd_raw %>% 
  pivot_longer(X1.22.20:all_of(today),
               names_to = "Date",
               values_to = "Rcvrd") %>% 
  mutate(Date = as.Date(Date %>% 
           str_remove("X") %>% 
           str_replace_all("\\.", "-") %>% 
           str_c(., "20"), format = "%m-%d-%Y"))

covid <- cases %>% 
  left_join(deaths, by = c("Province.State", "Country.Region", "Lat", "Long", "Date")) %>% 
  left_join(rcvrd, by = c("Province.State", "Country.Region", "Lat", "Long", "Date")) %>% 
  group_by(Province.State, Country.Region) %>% 
  mutate(Country_State = paste(Country.Region, Province.State, sep = "_"),
         `New Cases` = Cases - lag(Cases),
         `New Deaths` = Deaths - lag(Deaths),
         `Active Cases` = Cases - Deaths - Rcvrd) %>% 
  ungroup()

saveRDS(covid, file = paste0("jhu_covid_cleaned_", Sys.Date(), ".rds"))
