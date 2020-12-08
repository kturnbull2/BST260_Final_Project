library(shiny)
library(rvest)
library(lubridate)
library(stringr)
library(tidyverse)
library(dplyr)
library(ggplot2)

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


#for (i in c(1,2,3,4)){
 #   pattern = nchar(test) > 20
  #  test[pattern] <- test[pattern] %>% substr(1, nchar(test[pattern]) -5)
#}


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
new_test$date <- ymd(new_test$date)

######## end wrangling


####start of app
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Trump Rallys and Covid-19"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId= "city", label="Select a Rally",choices=str_sort(unique(as.character(new_test$City))))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(align="center",
                plotOutput("covidplot"),
                tableOutput("table")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        #find date of rally for selected city for the vertical line 
        rallydate <- reactive(as.numeric(new_test$date[new_test$City==input$city]))
        
        #covid data for plot 
        #county_data <- adjacent %>% filter(V4 == new_test$county_name[new_test$City==input$city])
        coviddata <- reactive(covid %>% filter(county==new_test$county_name[new_test$City==input$city], state==new_test$state_name[new_test$City==input$city]))
        output$covidplot <- renderPlot({coviddata() %>% ggplot(aes(x=date, y=cases, group=1)) + geom_line() + 
                scale_x_date(date_breaks="1 month", date_labels="%b %d") +
                geom_vline(xintercept=rallydate(),linetype=4)})
        
        #table data w dates before and after rally 
        table_data <- reactive(coviddata() %>% 
        filter(date %in% seq.Date(from=new_test$date[new_test$City==input$city]-2, to=new_test$date[new_test$City==input$city]+14, by="days")))
        #table_data$date <- format(table_data$date, "%Y-%m-%d") 
        output$table <- renderTable({expr=table_data()})
}

 

# Run the application 
shinyApp(ui = ui, server = server)
