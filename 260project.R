library(rvest)
library(lubridate)
library(stringr)
library(tidyverse)
library(dplyr)
library(ggplot2)

url <- "https://en.wikipedia.org/wiki/List_of_post-election_Donald_Trump_rallies"
#extract table and remove day of week
h <- read_html(url)
tab <- h %>% html_nodes("table") %>% .[8]
tab <- tab %>% html_table() %>% .[[1]]
test <- tab$`Date of rally`
test <- test %>% str_replace("Monday, |Tuesday, |Wednesday, |Thursday, |Friday, |Saturday, |Sunday, ", "")

#next problem is the [###] at the end of the dates.
#all rows have at least one, so remove it
test <- test %>% substr(1, nchar(test) - 5)

#now, some rows have more [###], so remove them one at a time when 
#the characters exceed 20 (no rows without [###] have character # bigger than 20)
pattern <- nchar(test) > 20
test[pattern] <- test[pattern] %>% substr(1, nchar(test[pattern]) -5)
pattern <- nchar(test) > 20
test[pattern] <- test[pattern] %>% substr(1, nchar(test[pattern]) -5)
pattern <- nchar(test) > 20
test[pattern] <- test[pattern] %>% substr(1, nchar(test[pattern]) -5)
pattern <- nchar(test) > 20
test[pattern] <- test[pattern] %>% substr(1, nchar(test[pattern]) -5)

#convert to dates
test <- mdy(test)
test <- test[1:67]
tab$`Date of rally` <- test

#get city and covid data
cities <- read.csv("uscities.csv")
covid <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

#combine rally data and city data 
new_test <- left_join(tab, cities, by=c("State" ="state_id", "City" = "city"))
#three  didn't match due to being townships not real cities
new_test[60, 9] <- "Macomb"
new_test[52, 9] <- "Oakland"
new_test[18, 9] <- "Beaver"

new_test <- new_test %>% select(c("Date of rally", "City", "State", "county_name", "population", "state_name")) %>% rename(date = "Date of rally")
covid$date <- ymd(covid$date)
new_test$date <- ymd(new_test$date)``
covid <- covid %>% filter(county %in% unique(as.character(new_test$county_name))) %>% filter(state %in% unique(as.character(new_test$state_name)))


adjacent <- read.table("https://www2.census.gov/geo/docs/reference/county_adjacency.txt", sep="\t", fill=FALSE, strip.white=TRUE)[,c(1,3)]
adjacent <- adjacent[-c(12459, 12460, 12461, 12462, 12463, 21751, 21752, 21753, 21754, 21755),]
adjacent <- adjacent[1:21721,]


for (i in (1:nrow(adjacent))){
  if (i != (nrow(adjacent))){
    if (is_empty(adjacent$V1[i+1])){
      adjacent$V1[i+1]<-adjacent$V1[i]
    }
  }
}
  
adjacent <- adjacent %>% mutate(V2=unlist(str_split(V1, " "))[1:(length(unlist(str_split(V1, " "))) - 2)])



#make plot 
rally_data <- covid %>% filter(state=="Pennsylvania", county=="Westmoreland")
#tried to add a vertical line w date of rally but had a hard time doing so
p <- rally_data %>% ggplot(aes(x=date, y=cases, group=1)) + geom_line()
p
