# the library() function loads needed packages for the current session
#  these packages must have already been installed 
#  there are several ways to do this including:  install.packages("readr") etc.

library(dplyr) # core "tidyverse" package for data manipulation
library(ggplot2) # data visualization package
library(readr) # tools for reading all sorts of data into R
library(tidyr) # "tidyverse" package mainly used to convert between different data formats (long vs wide)

# URL for California COVID-19 data on the CHHS Open Data Portal
covid_re_url <- "https://data.chhs.ca.gov/dataset/f88f9d7f-635d-4334-9dac-4ce773afe4e5/resource/b500dae2-9e58-428e-b125-82c7e9b07abb/download/covid19demographicratecumulative.csv"

# Read the Covid data directly from the Open Data Portal, and filter on race/ethnicity COVID data in California
covid_re     <- read_csv(covid_re_url) %>%  
  filter(county == "California", demographic_set == "race_ethnicity") 



# maybe a histogram?
# maybe a scatterplot?
  


# "Clean up"  the dataset a bit using "tidyverse" approach
case_data <- covid_re %>% 
  filter(metric == "cases",    # Filter on cases
         !demographic_set_category %in% c("Other", "Unknown")) # filter out some race groups


##############

# Some simple plotting using **BASE R***


# Produce simple bar chart of case rate per 100k population by race/ethnicity
barplot(case_data$metric_value_per_100k)


# Add race/ethnicity labels
barplot(case_data$metric_value_per_100k, 
        names.arg = case_data$demographic_set_category)

# Add titles for each axis, and the plot
barplot(case_data$metric_value_per_100k, 
        names.arg = case_data$demographic_set_category, 
        main = "COVID-19 Case Rate per 100k Population by Race/Ethnicity in CA", 
        xlab = "Race/Ethnicity", 
        ylab = "Case Rate per 100k Population")


# The charts above were created using base R commands.

############################



# Add note here about ggplot2

# Simple bar chart of case rate per 100k population by race/ethnicity, using ggplot2
ggplot(data = case_data, aes(x = demographic_set_category, y = metric_value_per_100k)) +
  geom_bar(stat = 'identity') +
  labs(title = "COVID-19 Case Rate per 100k Population by Race/Ethnicity in CA", 
       x = "Race/Ethnicity", 
       y = "Case Rate per 100k Population")


# Same chart, but as a horizontal bar chart
ggplot(data = case_data, aes(x = demographic_set_category, y = metric_value_per_100k)) +
  geom_bar(stat = 'identity') +
  coord_flip() + # This rotates the chart
  labs(title = "COVID-19 Case Rate per 100k Population by Race/Ethnicity in CA", 
       x = "Race/Ethnicity", 
       y = "Case Rate per 100k Population")



# This one
# Add # of case as text labels inside each bar
ggplot(data = case_data, aes(x = demographic_set_category, y = metric_value_per_100k)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = paste0("N = ", metric_value)), vjust = 1, color = 'white') +
  #coord_flip() + # This rotates the chart
  labs(title = "COVID-19 Case Rate per 100k Population by Race/Ethnicity in CA", 
       x = "Race/Ethnicity", 
       y = "Case Rate per 100k Population")



# this two
ggplot(data=covid_re, aes(x=demographic_set_category, y=metric_value_per_100k)      ) + geom_bar(stat = "identity") +
  facet_grid(rows=vars(metric), scales = "free")




### then with facet....

covid_re_2 <- covid_re %>% select(metric, demographic_set_category,  
                                  N = metric_value,
                                  Rate = metric_value_per_100k)  %>%
  pivot_longer(cols=N:Rate)


## this three
ggplot(data=covid_re_2, aes(x=demographic_set_category, y=value)      ) + geom_bar(stat = "identity") +
  facet_grid(rows=vars(metric), cols=vars(name), scales = "free")

