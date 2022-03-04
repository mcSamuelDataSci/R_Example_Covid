# the library() function loads needed packages for the current session
#  these packages must have already been installed 
#  there are several ways to do this including:  install.packages("readr") etc.

library(dplyr) # core "tidyverse" package for data manipulation
library(ggplot2) # data visualization package
library(readr) # tools for reading all sorts of data into R
library(tidyr) # "tidyverse" package mainly used to convert between different data formats (long vs wide)
library(stringr)

# URL for California COVID-19 data on the CHHS Open Data Portal. NOTE: This data reflects past 30 days
covid_re_url <- "https://data.chhs.ca.gov/dataset/f88f9d7f-635d-4334-9dac-4ce773afe4e5/resource/b500dae2-9e58-428e-b125-82c7e9b07abb/download/covid19demographicratecumulative.csv"

# Read the Covid data directly from the Open Data Portal. 
# Filter on race/ethnicity COVID data in California, and "clean up" the dataset a bit using "tidyverse" approach
covid_re     <- read_csv(covid_re_url) %>%  
  filter(county == "California", 
         demographic_set == "race_ethnicity", 
         !demographic_set_category %in% c("Other", "Unknown")) # Filter out 'Other' and 'Unknown' race groups 


# Filter on case data
case_data <- covid_re %>% 
  filter(metric == "cases")    # Filter on cases


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



# Charts below are creating using 'ggplot2', a popular and powerful open-sourced data visualization R package

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



# Customizing appearance of chart
# 1) Add text labels (# of COVID-19 cases) inside each bar
# 2) Wrap Race/Ethnicity labels
# 3) Change bar color
# 4) Change ggplot2 theme
# 5) Change font attributes of plot and axis titles, axis text
ggplot(data = case_data, aes(x = demographic_set_category, y = metric_value_per_100k)) +
  geom_bar(stat = 'identity', fill = "darkblue", colour = 'black') + # Add bars; Specify bar color and border color
  geom_text(aes(label = paste0("N = ", scales::comma(metric_value))), vjust = 1, color = 'white') + # Add bar text labels
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) + # Wrap Race/Ethnic labels
  labs(title = "COVID-19 Case Rate per 100k Population by Race/Ethnicity in CA", 
       x = "Race/Ethnicity", 
       y = "Case Rate per 100k Population") +
  theme_bw() + # Specify ggplot theme
  theme(plot.title = element_text(face = "bold", colour = "darkblue", size = 18), # Plot title font properties
        axis.title = element_text(face = "bold", size = 16), # Axis title font properties
        axis.text = element_text(size = 14)) # Axis text font properties



# COVID-19 Case, Death, and Test Rates by Race/Ethnicity in one chart using 'facet_grid'
ggplot(data = covid_re, aes(x = demographic_set_category, y = metric_value_per_100k)) + 
  geom_bar(stat = 'identity', fill = "darkblue", colour = 'black') +
  geom_text(aes(label = paste0("N = ", scales::comma(metric_value, accuracy = 1))), vjust = 1, color = 'white') +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  labs(title = "COVID-19 Rates per 100k Population by Race/Ethnicity in CA", 
       x = "Race/Ethnicity", 
       y = "Rate per 100k Population") +
  facet_grid(rows = vars(metric), scales = "free") + # Forms a matrix of panels defined by faceting variable (in this case, by metric variable)
  theme_bw() + 
  theme(plot.title = element_text(face = "bold", colour = "darkblue", size = 18), 
        axis.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 14), 
        strip.text = element_text(face = "bold", size = 12)) 




### then with facet.... IF TIME PERMITS

covid_re_2 <- covid_re %>% 
  select(metric, demographic_set_category, N = metric_value, Rate = metric_value_per_100k)  %>%
  pivot_longer(cols = N:Rate)


## this three
ggplot(data=covid_re_2, aes(x=demographic_set_category, y=value)      ) + geom_bar(stat = "identity") +
  facet_grid(rows=vars(metric), cols=vars(name), scales = "free")

