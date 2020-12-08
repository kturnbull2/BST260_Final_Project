#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Trump Rallys and Covid-19"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId= "city", label="Select a Rally", choices=unique(as.character(new_test$City)))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("covidplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        rallydate <- reactive(as.numeric(new_test$date[new_test$City==input$city]))
        coviddata <- reactive(covid %>% filter(county==new_test$county_name[new_test$City==input$city]))
        output$covidplot <- renderPlot({coviddata() %>% ggplot(aes(x=date, y=cases, group=1)) + geom_line() + 
                scale_x_date(date_breaks="1 month", date_labels="%b %d") +
                geom_vline(xintercept=rallydate(),linetype=4)})
    
}

#+
#geom_vline(xintercept=rallydate(),linetype=4 

# Run the application 
shinyApp(ui = ui, server = server)
