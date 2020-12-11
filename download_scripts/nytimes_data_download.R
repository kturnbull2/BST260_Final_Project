
library(tidyverse)

# state-level cases and deaths data from nytimes
url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
covid <- read_csv(url)
write_csv(covid, "data/covid.csv")
