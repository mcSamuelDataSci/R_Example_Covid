# the library() function loads needed packages for the current session
#  these packages must have already been installed 
#  there are several ways to do this including:  install.packages("readr") etc.

library(readr) # tools for reading all sorts of data into R
library(dplyr) # core "tidyverse" package for data manipulation
library(lubridate) # makes working with dates easy

library(tigris) # package for reading "tiger" "shape files" for making maps
options(tigris_class = "sf") # causes tigris to read files in the "simple features" format

#=============================================================================

# Make California county map with simplified shapes
county_map <- counties(state = "CA", cb = TRUE) %>%
  select(county = NAME) # geometry automatically saved with sf object

# URL for California COVID-19 data on the CHHS Open Data POrtal
ca_covid_url <- "https://data.chhs.ca.gov/dataset/f333528b-4d38-4814-bebb-12db1f10f535/resource/046cdd2b-31e5-4d34-9ed3-b48cdbc4be7a/download/covid19cases_test.csv"

# Read that Covid data directly from the Open Data Portal
covid_data0 <- read_csv(ca_covid_url)

  
# Data manipulation 
covid_data <-  covid_data0 %>%
      mutate(year_month = floor_date(date, "month")) %>%
      rename(county = area) %>%
      group_by(county, year_month) %>%
      summarize(Cases  = sum(cases),
                Deaths = sum(deaths))


# Join the simple feature map file and the data
data_map <- left_join(county_map, covid_data, by= "county")

saveRDS(data_map, "data_map.RDS")
saveRDS(covid_data, "covid_data.RDS")


