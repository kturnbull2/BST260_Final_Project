library(shiny)
library(rvest)
library(lubridate)
library(stringr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(shinythemes)
library(rlang)

#######beginning of wrangling
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

pattern = nchar(test) > 20
test[pattern] <- test[pattern] %>% substr(1, nchar(test[pattern]) -5)
pattern = nchar(test) > 20
test[pattern] <- test[pattern] %>% substr(1, nchar(test[pattern]) -5)
pattern = nchar(test) > 20
test[pattern] <- test[pattern] %>% substr(1, nchar(test[pattern]) -5)
pattern = nchar(test) > 20
test[pattern] <- test[pattern] %>% substr(1, nchar(test[pattern]) -5)


#convert to dates
test <- mdy(test)
test <- test[1:67]
tab$`Date of rally` <- test

#get city and covid data
cities <- read.csv("data/uscities.csv")
covid <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

#combine rally data and city data 
new_test <- left_join(tab, cities, by=c("State" ="state_id", "City" = "city"))
#three  didn't match due to being townships not real cities
new_test[60, 9] <- "Macomb"
new_test[52, 9] <- "Oakland"
new_test[18, 9] <- "Beaver"

new_test <- new_test %>% select(c("Date of rally", "City", "State", "county_name", "population", "state_name")) %>% rename(date = "Date of rally")
covid$date <- ymd(covid$date)
new_test$date <- ymd(new_test$date)

#adjacent county data
adjacent <- read.table("https://www2.census.gov/geo/docs/reference/county_adjacency.txt", sep="\t", fill=FALSE, strip.white=TRUE)[,c(1,3)]
adjacent <- adjacent %>% rename(county=V1,adj_counties=V3)
adjacent$adj_counties[adjacent$adj_counties == "Do\xb1a Ana County, NM"] <- "Dona Ana County, NM"
adjacent$county[adjacent$county == "Do\xb1a Ana County, NM"] <- "Dona Ana County, NM"

#adjacent <- adjacent[-c(12459, 12460, 12461, 12462, 12463, 21751, 21752, 21753, 21754, 21755),]
adjacent <- adjacent[1:21721,]


for (i in (1:nrow(adjacent))){
    if (i != (nrow(adjacent))){
        if (nchar(adjacent$county[i+1])==0){
            adjacent$county[i+1]<-adjacent$county[i]
        }
    }
}

#adjacent <- adjacent[-c(12511),]
#adjacent <- adjacent[-c(12528),]
#adjacent$V3 <- adjacent$V3[!(adjacent$V3=="Do<b1>a Ana County, NM")]

adjacent$county_name <- substr(adjacent$county, 1, nchar(adjacent$county)-11)
adjacent$county_state <- substr(adjacent$county, nchar(adjacent$county)-2, nchar(adjacent$county))
adjacent$adj_county_name <- substr(adjacent$adj_counties, 1, nchar(adjacent$adj_counties)-11)
adjacent$adj_county_state <- substr(adjacent$adj_counties, nchar(adjacent$adj_counties)-1, nchar(adjacent$adj_counties))

#state abbreviations data set
state_abs <- read_csv("data/state_abbrevs.csv")
adjacent <- left_join(adjacent, state_abs, by=c("adj_county_state" = "Code"))

#census data with county populations
county_pop <- read_csv("data/county-pop.csv")
county_pop <- county_pop %>% select(c("STNAME", "CTYNAME", "POPESTIMATE2019")) %>% 
    rename("state"="STNAME", "county"="CTYNAME", "population"="POPESTIMATE2019")
county_pop$county[1835]<-"Dona Ana County"
county_pop$county[county_pop$county=="Cars"]<- "Carson City"
county_pop$county <- substr(county_pop$county, 1, nchar(county_pop$county)-7)
covid <- left_join(covid, county_pop, by=c("county", "state"))
covid <- covid %>% mutate(cases_per_thous = cases*1000 / population)

new_test <- new_test %>% mutate(city_state = paste(City, State, sep=", "))


######## end wrangling


####start of app
# Define UI for application that draws a histogram
ui <- fluidPage(theme=shinytheme("flatly"),

    # Application title
    titlePanel("Trump 2020 Election Rallies and Covid-19"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons(inputId = "yvar", label = "Select a variable to display",
                         choiceNames = c("New cases", "New cases per thousand"), choiceValues = c("cases", "cases_per_thous")),
            radioButtons(inputId = "adj", label = "Choose which cases to display",
                         choiceNames = c("County of Rally", "County & Surrounding Counties of Rally"), choiceValues=c("F", "T")),
            selectInput(inputId= "city", label="Select a Rally",choices=str_sort(unique(as.character(new_test$city_state))))
          
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(align="center",
                plotOutput("covidplot"),
                verbatimTextOutput("text")
                
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        #find date of rally for selected city for the vertical line 
        rallydate <- reactive(as.numeric(new_test$date[new_test$city_state==input$city]))
        
        
        
        output$covidplot <- renderPlot({
            if (input$adj=="F"){
                #covid data for plot 
                coviddata <- reactive(covid %>% filter(county==new_test$county_name[new_test$city_state==input$city], state==new_test$state_name[new_test$city_state==input$city]))
                coviddata() %>% ggplot(aes_string(x="date", y=ifelse(input$yvar=="cases", "cases", "cases_per_thous"), group=1)) + geom_line(color="mediumblue") + 
                        scale_x_date(date_breaks="1 month", date_labels="%b %d") +
                        geom_vline(xintercept=rallydate(),linetype=4) + theme_few() +
                        ggtitle(paste("Covid-19 Cases in ",input$city, "'s County", sep="")) +
                        ylab(ifelse(input$yvar=="cases", "Cases", "Cases per Thousand"))
            }
            
            else if (input$adj=="T"){
                adj_data <- reactive(adjacent %>% filter(county_name==new_test$county_name[new_test$city_state==input$city] & State==new_test$state_name[new_test$city_state==input$city]))
                coviddata <- reactive(covid %>% filter(county %in% adj_data()$adj_county_name, state %in% adj_data()$State))
                coviddata() %>% ggplot(aes_string(x="date", y=ifelse(input$yvar=="cases", "cases", "cases_per_thous"), color="county", group="county")) + geom_line() + 
                        scale_x_date(date_breaks="1 month", date_labels="%b %d") +
                        geom_vline(xintercept=rallydate(),linetype=4) + theme_few() +
                        ggtitle(paste("Covid-19 Cases in ",input$city, "'s County and Surrounding Counties", sep="")) +
                        ylab(ifelse(input$yvar=="cases", "Cases", "Cases per Thousand"))
            }
            
        })
        
        output$text <- renderText({paste("Covid-19 case data is from the New York Times Covid-19 data set.", "Adjacent counties were found from US Census Bureau County Adjacency file.", "Data connecting counties to their cities is from simplemaps.com.", sep="\n")})
       
}

 

# Run the application 
shinyApp(ui = ui, server = server)
