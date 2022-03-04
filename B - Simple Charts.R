add comments....


library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

covid_re_url <- "https://data.chhs.ca.gov/dataset/f88f9d7f-635d-4334-9dac-4ce773afe4e5/resource/b500dae2-9e58-428e-b125-82c7e9b07abb/download/covid19demographicratecumulative.csv"

covid_re     <- read_csv(covid_re_url) %>%  
  filter(county == "California", demographic_set == "race_ethnicity")




maybe a histogram?
maybe a scatterplot?
  



temp_data <- covid_re %>% filter(metric == "cases")


barplot(temp_data$metric_value_per_100k)
barplot(temp_data$metric_value_per_100k, names.arg = temp_data$demographic_set_category)


#  J fix labels; not other ... unknown...



# same chart with gglot
# same chart with gglot  horizontal ...
# same chart + ???




ggplot(data=covid_re, aes(x=demographic_set_category, y=metric_value_per_100k)      ) + geom_bar(stat = "identity") +
  facet_grid(rows=vars(metric), scales = "free")




### then with facet....

covid_re_2 <- covid_re %>% select(metric, demographic_set_category,  
                                  N = metric_value,
                                  Rate = metric_value_per_100k)  %>%
  pivot_longer(cols=N:Rate)

ggplot(data=covid_re_2, aes(x=demographic_set_category, y=value)      ) + geom_bar(stat = "identity") +
  facet_grid(rows=vars(metric), cols=vars(name), scales = "free")

