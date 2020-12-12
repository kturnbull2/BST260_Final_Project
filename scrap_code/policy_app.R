library(shiny)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(rvest)
library(stringr)
library(zoo)
library(directlabels)
library(plotly)

# wrangling code

# read in state-level cases and deaths data from nytimes
covid <- read_csv("data/covid.csv")

# read in policy data
policies <- read_csv("data/policies.csv")

# download population data 
url <- "https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population"
h <- read_html(url)

# extract data from wiki population table
population_names <- c("rank_curr", "rank_2010", "state", "pop_2019", "pop_2010", "perc_change", "abs_change", "house_seats", "house_perc", "est_pop_per_elec_vote", "cen_pop_per_seat_2019", "cen_pop_per_seat_2010", "perc_pop_2019", "perc_pop_2010", "perc_pop_change", "perc_of_elec")
population <- h %>% 
    html_nodes("table") %>% 
    .[1] %>% 
    html_table(fill = TRUE) %>% 
    .[[1]] %>% 
    setNames(population_names)

# removed extra row at the top and total rows at the bottom of data frame and row for American Samoa in population data
population <- population %>% 
    slice(2:56) %>% 
    select(state, pop_2019) %>% 
    rename(population = pop_2019)

# remove out [10], [12] and [14] subscripts from last 3 rows and remove commas from population numbers
# also change population to numeric class
population <- population %>% 
    mutate(population = str_replace_all(population, "\\[\\d{2}\\]", ""),
           population = str_replace_all(population, ",", ""),
           population = as.numeric(population))

# change to U.S. Virgin Islands to Virgin Islands to match covid data frame 
population <- population %>% 
    mutate(state = recode(state, 
                          `U.S. Virgin Islands` = "Virgin Islands"))

# create tidy data for covid data
covid_tidy <- covid %>% 
    gather(measure, cum_count, cases:deaths)

# create 7 day rolling average of cases and deaths
covid_tidy <- covid_tidy %>% 
    group_by(state, measure) %>%
    arrange(state, measure, date) %>%
    mutate(new_count = cum_count - lag(cum_count),
           new_count_7dayavg = rollmean(new_count, k = 7, fill = NA)) %>%
    ungroup()

# join population data to covid data frame
covid_tidy <- covid_tidy %>% left_join(population, by = "state")

# create per 10 million population variables for 7 day average variables
covid_tidy <- covid_tidy %>% 
    mutate(new_count_7dayavg_per_1mil = new_count_7dayavg / (population / 1000000),
           cum_count_per_100thous = cum_count / (population / 100000))

# remove county-level policies data to keep only state-level policies 
policies_subset <- policies %>% 
    filter(policy_level == "state") %>%
    rename(state = state_id)

# removed row that was incorrectly labeled as state-level policy
policies_subset <- policies_subset %>% 
    filter(is.na(county)) %>% 
    select(-c(source, fips_code, county, policy_level))

# change from abbreviated to complete state names in policy data frame
covid_df_state_names <- sort(unique(covid$state))
policies_df_state_names <- c("AL", "AK", "AZ", "AR", "CA", 
                             "CO", "CT", "DE", "DC", "FL", 
                             "GA", "GU", "HI", "ID", "IL", 
                             "IN", "IA", "KS", "KY", "LA", 
                             "ME", "MD", "MA", "MI", "MN", 
                             "MS", "MO", "MT", "NE", "NV", 
                             "NH", "NJ", "NM", "NY", "NC", 
                             "ND", "MP", "OH", "OK", "OR", 
                             "PA", "PR", "RI", "SC", "SD", 
                             "TN", "TX", "UT", "VT", "VI", 
                             "VA", "WA", "WV", "WI", "WY")
key <- setNames(covid_df_state_names, policies_df_state_names)

policies_subset <- policies_subset %>% 
    mutate(state = recode(state, !!!key))

# join combined covid and population data with policies data
tidy_df <- covid_tidy %>% left_join(policies_subset, by = c("state", "date"))

# create wide_df for summary tables in app 
wide_df <- tidy_df %>%
    pivot_wider(names_from = measure,
                values_from = c(cum_count, new_count,
                                new_count_7dayavg,
                                new_count_7dayavg_per_1mil, 
                                cum_count_per_100thous))

# create table with ranks for policies
policies_rank <- policies_subset %>% 
    filter(start_stop == "start") %>%
    group_by(state) %>%
    summarise(n_policy = n()) %>%
    arrange(desc(n_policy)) %>% 
    mutate(rank_policy = as.numeric(rownames(.)), 
           policy_comb = paste0(rank_policy, " (", n_policy, ")")) %>%
    ungroup()

# create table for ranks of cum_cases, cum_deaths, cum_cases_per_100thous, and cum_deaths_per_100thous
cum_ranks <- covid_tidy %>%
    pivot_wider(names_from = measure,
                values_from = c(cum_count, new_count,
                                new_count_7dayavg,
                                new_count_7dayavg_per_1mil, 
                                cum_count_per_100thous)) %>%
    filter(date == max(date)) %>%
    select(state, cum_count_cases, cum_count_deaths,
           cum_count_per_100thous_cases, 
           cum_count_per_100thous_deaths) %>%
    arrange(desc(cum_count_cases)) %>% 
    mutate(rank_cum_cases = as.numeric(rownames(.)),
           cum_cases_comb = paste0(rank_cum_cases, " (",
                                   cum_count_cases, ")")) %>% 
    arrange(desc(cum_count_deaths)) %>%
    mutate(rank_cum_deaths = as.numeric(rownames(.)),
           cum_deaths_comb = paste0(rank_cum_deaths, " (",
                                    cum_count_deaths, ")")) %>%
    arrange(desc(cum_count_per_100thous_cases)) %>% 
    mutate(rank_cum_cases_per_100thous = as.numeric(rownames(.)),
           cum_cases_per_100thous_comb = paste0(rank_cum_cases_per_100thous, " (",
                                                round(cum_count_per_100thous_cases,
                                                      digits = 2), ")")) %>%
    arrange(desc(cum_count_per_100thous_deaths)) %>% 
    mutate(rank_cum_deaths_per_100thous = as.numeric(rownames(.)),
           cum_deaths_per_100thous_comb = paste0(rank_cum_deaths_per_100thous, " (",
                                                 round(cum_count_per_100thous_deaths,
                                                       digits = 2), ")"))
# merge rank tables 
rank_table <- policies_rank %>% left_join(cum_ranks, by = "state")

# import US map data
us_map <- map_data("state")

# update states to match spelling from tidy_df
us_map <- us_map %>% 
    rename(state = region) %>% 
    mutate(state = str_to_title(state)) %>%
    mutate(state = str_replace(state, "District Of Columbia", "District of Columbia"))

# Define UI 
ui <- navbarPage(
    theme = shinytheme("yeti"),
    "Covid-19 App",
    navbarMenu("Comparing States/Territories",
        tabPanel("Line Graphs",
                 sidebarLayout(
                     sidebarPanel(
                         p("Hover over any of the plots to get more information on exact case and death numbers!"),
                         pickerInput(inputId = "comp_states_variable", 
                                     label = "Select variable to display:",
                                     choices = list("Cumulative" = "cum_count",
                                                    "Cumulative/100k" = "cum_count_per_100thous",
                                                    "New (7-day average)" = "new_count_7dayavg",
                                                    "New/million (7-day average)" = "new_count_7dayavg_per_1mil"),
                                     multiple = FALSE),
                         pickerInput(inputId = "comp_states",
                                     label = "Choose states to compare:",
                                     options = list(`actions-box` = TRUE, 
                                                    `none-selected-text` = "Please select some states!"), # create select/deselect action box
                                     choices = as.list(levels(factor(sort(tidy_df$state)))),
                                     selected = as.list(levels(factor(sort(tidy_df$state)))[1:5]),
                                     multiple = TRUE),
                         dateRangeInput(inputId = "comp_states_plot_date_range",
                                        label = "Select date range:",
                                        start = min(tidy_df$date),
                                        end = max(tidy_df$date),
                                        min = min(tidy_df$date),
                                        max = max(tidy_df$date),
                                        format = "yyyy-mm-dd")
                     ),
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Cases", plotlyOutput(outputId = "comp_states_cases_plot")), 
                             tabPanel("Deaths", plotlyOutput(outputId = "comp_states_deaths_plot"))
                         )
                     )
                 )
        ),
        
        tabPanel("Maps",
                 sidebarLayout(
                     sidebarPanel(
                         p("Hover over any of the maps to get more information on exact case and death numbers!"),
                         pickerInput(inputId = "map_variable", 
                                     label = "Select variable to display:",
                                     choices = list("Cumulative" = "cum_count",
                                                    "Cumulative/100k" = "cum_count_per_100thous",
                                                    "New (7-day average)" = "new_count_7dayavg",
                                                    "New/million (7-day average)" = "new_count_7dayavg_per_1mil"),
                                     multiple = FALSE),
                         dateInput(inputId = "map_date", 
                                   label = "Choose date",
                                   value = max(tidy_df$date),
                                   min = min(tidy_df$date),
                                   max = max(tidy_df$date),
                                   format = "yyyy-mm-dd"),
                     ),
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Cases", plotlyOutput(outputId = "map_cases")), 
                             tabPanel("Deaths", plotlyOutput(outputId = "map_deaths"))
                         )
                     )
                 )
        )
    ),
    navbarMenu("Exploring State-Wide Policies",
               tabPanel("Time Trends",
                   sidebarLayout(
                       sidebarPanel(p("Filter your selected states' state-wide policy table further by clicking",
                                      "on the point corresponding to your policy of interest on the plot,",
                                      "modifying the date range or searching the table!"),
                                    pickerInput(inputId = "state", 
                                                label = "Select state/territory:",
                                                choices = as.list(levels(factor(tidy_df$state)))),
                                    dateRangeInput(inputId = "policies_plot_date_range",
                                                   label = "Select date range:",
                                                   start = min(tidy_df$date),
                                                   end = max(tidy_df$date),
                                                   min = min(tidy_df$date),
                                                   max = max(tidy_df$date),
                                                   format = "yyyy-mm-dd"),
                                    tableOutput(outputId = "policies_summary_table")
                       ),
                       mainPanel(br(),
                                 br(),
                                 br(),
                                 br(),
                                 plotOutput(outputId = "policies_plot",
                                            click = "policies_plot_click")
                       )
                   ),
                   
                   tags$b(textOutput(outputId = "policies_click_table_caption")),
                   br(),
                   dataTableOutput(outputId = "policies_click_table")
                    
               ),
               
               tabPanel("State Rankings",
                        sidebarLayout(
                            sidebarPanel(
                                p("Adjust the ranking table to your liking by selecting inputs below!"),
                                pickerInput(inputId = "rank_table_measure", 
                                            label = "Select measure to display:",   
                                            choices = c("Cumulative", "Cumulative per 100,000 people"), 
                                            selected = c("Cumalative"),
                                            multiple = FALSE),
                                pickerInput(inputId = "rank_table_variable", 
                                            label = "Select variable to order by:",   
                                            choices = c("Policies", "Cases", "Deaths"), 
                                            selected = c("Policies"),
                                            multiple = FALSE),
                                pickerInput(inputId = "rank_table_order", 
                                            label = "Select order to display:",   
                                            choices = c("Ascending", "Descending"), 
                                            selected = c("Ascending"),
                                            multiple = FALSE),
                                sliderInput(inputId = "rank_table_n",
                                            label = "Select # states/territories to display:",
                                            min = 1, 
                                            max = 55, 
                                            value = 15, 
                                            ticks = FALSE),
                                pickerInput(inputId = "rank_table_custom", 
                                            label = "Customize which states/territories to display:",   
                                            choices = c("No", "Yes"), 
                                            selected = c("Ascending"),
                                            multiple = FALSE),
                                uiOutput(outputId = "rank_table_dynamic") # create dynamic input that changes based on input$rank_table_custom
                            ),
                            mainPanel(tableOutput(outputId = "rank_table_output"))
                        )
               )
    ),
    tabPanel("About",
             tags$h4("Introduction"),
             p("This app was meant to help people explore the intersection of Covid-19 and politics",
               "in the United States in a user-friendly way. The app begins by first giving an introduction", 
               "to data on Covid cases and deaths for the user to get acquainted with the general patterns",
               "across different states. Then the app focuses on two specific angles related to the intersection", 
               "of Covid-19 and politics: 1) the impact of Trump rallies on cases; and 2) the relationship", 
               "between state-wide policies and cases and deaths."),
             br(),
             tags$h4("Data"),
             tags$b("Cases and deaths: "), tags$a(href = "https://github.com/nytimes/covid-19-data", "New York Times Github"),
             br(),
             tags$b("Policies: "), tags$a(href = "https://healthdata.gov/dataset/covid-19-state-and-county-policy-orders", "HHS manually-curated dataset"),
             br(),
             tags$b("State populations: "), tags$a(href = "https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population", "Wikipedia"),
             br(),
             br(),
             br(),
             tags$h4("Disclaimer"),
             p("Due to the quickly evolving nature of the virus, a few of the people who have put together the data sets",
               "that were used in this app have cautioned against claiming completeness and correctness of the data.",
               "We think it is important to point out that the policies dataset is likely not fully complete and",
               "conclusions should be drawn with caution for that portion especially.")
        
    )
)

# Define server
server <- function(input, output) {
    
    # create df filtered by selected states and selected date range
    comp_states_df <- reactive({
        tidy_df %>% filter(state %in% input$comp_states,
                           date >= input$comp_states_plot_date_range[1] & date <= input$comp_states_plot_date_range[2])
    })
    
    # create plot of cases over time with lines for each selected state and filtered by selected date range
    output$comp_states_cases_plot <- renderPlotly({
        
        # output plots that correspond to cases variable input selection
        if (input$comp_states_variable == "cum_count") {
            
            p <- comp_states_df() %>%
                filter(measure == "cases") %>% 
                ggplot(aes(x = date, y = cum_count, color = state, group = 1,
                           text = paste0("Day: ", format(date, format ="%b %d %Y"),
                                        "\n", state, ": ", round(cum_count, digits = 3)))) + # text argument defines what ggplotly will show
                scale_x_date(date_breaks = "1 month", date_labels = "%b") + # designate nicer breaks for plot
                geom_line() +
                ggtitle("Cumulative cases over time") +
                xlab("Date") +
                ylab("Cumulative cases") +
                theme_bw() +
                theme(legend.title = element_blank(), legend.position = "")
            
        } else if (input$comp_states_variable == "cum_count_per_100thous") {
            
            p <- comp_states_df() %>%
                filter(measure == "cases") %>%
                ggplot(aes(x = date, y = cum_count_per_100thous, color = state, group = 1,
                           text = paste0("Day: ", format(date, format ="%b %d %Y"),
                                        "\n", state, ": ", round(cum_count_per_100thous, digits = 3)))) +
                scale_x_date(date_breaks = "1 month", date_labels = "%b") +
                geom_line() +
                ggtitle("Cumulative cases over time") +
                xlab("Date") +
                ylab("Cumulative cases/100k") +
                theme_bw() +
                theme(legend.title = element_blank(), legend.position = "")
            
        } else if (input$comp_states_variable == "new_count_7dayavg") {
            
            p <- comp_states_df() %>%
                filter(measure == "cases") %>%
                ggplot(aes(x = date, y = new_count_7dayavg, color = state, group = 1,
                           text = paste0("Day: ", format(date, format ="%b %d %Y"),
                                        "\n", state, ": ", round(new_count_7dayavg, digits = 3)))) +
                scale_x_date(date_breaks = "1 month", date_labels = "%b") +
                geom_line() +
                ggtitle(paste("New cases over time")) +
                xlab("Date") +
                ylab("New cases (7-day average)") +
                theme_bw() +
                theme(legend.title = element_blank(), legend.position = "")
            
        } else if (input$comp_states_variable == "new_count_7dayavg_per_1mil") {
            
            p <- comp_states_df() %>%
                filter(measure == "cases") %>%
                ggplot(aes(x = date, y = new_count_7dayavg_per_1mil, color = state, group = 1,
                           text = paste0("Day: ", format(date, format ="%b %d %Y"),
                                        "\n", state, ": ", round(new_count_7dayavg_per_1mil, digits = 3)))) +
                scale_x_date(date_breaks = "1 month", date_labels = "%b") +
                geom_line() +
                ggtitle(paste("New cases over time")) +
                xlab("Date") +
                ylab("New cases/million (7-day average)") +
                theme_bw() +
                theme(legend.title = element_blank(), legend.position = "")
            
        }
        
        # make plot interactive so user can hover over and get information designated in text field
        ggplotly(p, tooltip = "text")
        
    })
    
    # create plot of deaths with lines for each state and filtered by select data range
    output$comp_states_deaths_plot <- renderPlotly({
        
        # output plots that correspond to deaths variable input selection
        if (input$comp_states_variable == "cum_count") {
            
            p <- comp_states_df() %>%
                filter(measure == "deaths") %>%
                ggplot(aes(x = date, y = cum_count, color = state, group = 1,
                           text = paste0("Day: ", format(date, format ="%b %d %Y"),
                                        "\n", state, ": ", round(cum_count, digits = 3)))) +
                scale_x_date(date_breaks = "1 month", date_labels = "%b") +
                geom_line() +
                ggtitle("Cumulative deaths over time") +
                xlab("Date") +
                ylab("Cumulative deaths") +
                theme_bw() +
                theme(legend.title = element_blank(), legend.position = "")
            
        } else if (input$comp_states_variable == "cum_count_per_100thous") {
            
            p <- comp_states_df() %>%
                filter(measure == "deaths") %>%
                ggplot(aes(x = date, y = cum_count_per_100thous, color = state, group = 1,
                           text = paste0("Day: ", format(date, format ="%b %d %Y"),
                                        "\n", state, ": ", round(cum_count_per_100thous, digits = 3)))) +
                scale_x_date(date_breaks = "1 month", date_labels = "%b") +
                geom_line() +
                ggtitle("Cumulative deaths over time") +
                xlab("Date") +
                ylab("Cumulative deaths/100k") +
                theme_bw() +
                theme(legend.title = element_blank(), legend.position = "")
            
        } else if (input$comp_states_variable == "new_count_7dayavg") {
            
            p <- comp_states_df() %>%
                filter(measure == "deaths") %>%
                ggplot(aes(x = date, y = new_count_7dayavg, color = state, group = 1,
                           text = paste0("Day: ", format(date, format ="%b %d %Y"),
                                        "\n", state, ": ", round(new_count_7dayavg, digits = 3)))) +
                scale_x_date(date_breaks = "1 month", date_labels = "%b") +
                geom_line() +
                ggtitle(paste("New deaths over time")) +
                xlab("Date") +
                ylab("New deaths (7-day average)") +
                theme_bw() +
                theme(legend.title = element_blank(), legend.position = "")
            
        } else if (input$comp_states_variable == "new_count_7dayavg_per_1mil") {
            
            p <- comp_states_df() %>%
                filter(measure == "deaths") %>%
                ggplot(aes(x = date, y = new_count_7dayavg_per_1mil, color = state, group = 1,
                           text = paste0("Day: ", format(date, format ="%b %d %Y"),
                                        "\n", state, ": ", round(new_count_7dayavg_per_1mil, digits = 3)))) +
                scale_x_date(date_breaks = "1 month", date_labels = "%b") +
                geom_line() +
                ggtitle(paste("New deaths over time")) +
                xlab("Date") +
                ylab("New deaths/million (7-day average)") +
                theme_bw() +
                theme(legend.title = element_blank(), legend.position = "")
            
        }
        
        ggplotly(p, tooltip = "text")
        
    })
    
    # create map of cases for selected data 
    output$map_cases <- renderPlotly({
        
        # create map df for cases maps
        map_df <- tidy_df %>% 
            filter(date == input$map_date & measure == "cases") %>%  # filter by selected date and cases
            left_join(us_map, by = "state") # join map data after filtering
        
        # output maps that correspond to cases variable input selection
        if (input$map_variable == "cum_count") {
            
            max_lim <- tidy_df %>% 
                filter(measure == "cases") %>% 
                pull(cum_count) %>% 
                max(na.rm = TRUE) # pull max cumulative cases number to use as limit in plot
            
            p <- map_df %>%
                ggplot(aes(x = long, y = lat, fill = cum_count, group = group,
                           text = paste0("Day: ", format(date, format ="%b %d %Y"),
                                         "\n", state, ": ", round(cum_count, digits = 3)))) +
                geom_polygon(color = "white") +
                scale_fill_viridis_c(name = "Cumulative \n(log10)", trans = "log10",
                                     limits = c(1, max_lim), option = "viridis") + # set limits so legend doesn't change when selecte data is changed
                ggtitle(paste("Cumulative cases on", 
                              format(input$map_date, format ="%b %d %Y"))) +
                theme(panel.grid.major = element_blank(), 
                      panel.background = element_blank(),
                      axis.title = element_blank(), 
                      axis.text = element_blank(),
                      axis.ticks = element_blank())  
            
        } else if (input$map_variable == "cum_count_per_100thous") {
            
            max_lim <- tidy_df %>% 
                filter(measure == "cases") %>% 
                pull(cum_count_per_100thous) %>% 
                max(na.rm = TRUE) 
            
            p <- map_df %>%
                ggplot(aes(x = long, y = lat, fill = cum_count_per_100thous, group = group,
                           text = paste0("Day: ", format(date, format ="%b %d %Y"),
                                         "\n", state, ": ", round(cum_count_per_100thous, digits = 3)))) +
                geom_polygon(color = "white") +
                scale_fill_viridis_c(name = "Cumulative \n/100k \n(log10)", trans = "log10",
                                     limits = c(1, max_lim), option = "magma") +
                ggtitle(paste("Cumulative cases on", 
                              format(input$map_date, format ="%b %d %Y"))) +
                theme(panel.grid.major = element_blank(), 
                      panel.background = element_blank(),
                      axis.title = element_blank(), 
                      axis.text = element_blank(),
                      axis.ticks = element_blank())  
            
        } else if (input$map_variable == "new_count_7dayavg") {
            
            max_lim <- tidy_df %>% 
                filter(measure == "cases") %>% 
                pull(new_count_7dayavg) %>% 
                max(na.rm = TRUE)
            
            p <- map_df %>%
                ggplot(aes(x = long, y = lat, fill = new_count_7dayavg, group = group,
                           text = paste0("Day: ", format(date, format ="%b %d %Y"),
                                         "\n", state, ": ", round(new_count_7dayavg, digits = 3)))) +
                geom_polygon(color = "white") +
                scale_fill_viridis_c(name = "New \n(log10)", trans = "log10",
                                     limits = c(1, max_lim), option = "plasma") +
                ggtitle(paste("New cases (7-day average) on", 
                              format(input$map_date, format ="%b %d %Y"))) +
                theme(panel.grid.major = element_blank(), 
                      panel.background = element_blank(),
                      axis.title = element_blank(), 
                      axis.text = element_blank(),
                      axis.ticks = element_blank())
            
        } else if (input$map_variable == "new_count_7dayavg_per_1mil") {
            
            max_lim <- tidy_df %>% 
                filter(measure == "cases") %>% 
                pull(new_count_7dayavg_per_1mil) %>% 
                max(na.rm = TRUE)
            
            p <- map_df %>%
                ggplot(aes(x = long, y = lat, fill = new_count_7dayavg_per_1mil, group = group,
                           text = paste0("Day: ", format(date, format ="%b %d %Y"),
                                         "\n", state, ": ", round(new_count_7dayavg_per_1mil, digits = 3)))) +
                geom_polygon(color = "white") +
                scale_fill_viridis_c(name = "New \n/million \n(log10)", trans = "log10",
                                     limits = c(1, max_lim), option = "inferno") +
                ggtitle(paste("New cases (7-day average) on", 
                              format(input$map_date, format ="%b %d %Y"))) +
                theme(panel.grid.major = element_blank(), 
                      panel.background = element_blank(),
                      axis.title = element_blank(), 
                      axis.text = element_blank(),
                      axis.ticks = element_blank())
        }
        
        ggplotly(p, tooltip = "text")
        
    })
    
    # create map of deaths for selected date
    output$map_deaths <- renderPlotly({
        
        # create deaths df for map plots
        map_df <- tidy_df %>% 
            filter(date == input$map_date & measure == "deaths") %>% # fitler by selected date and deaths
            left_join(us_map, by = "state") # join map data
        
        # output maps that correspond to deaths variable input selection
        if (input$map_variable == "cum_count") {
            
            max_lim <- tidy_df %>% 
                filter(measure == "deaths") %>% 
                pull(cum_count) %>% 
                max(na.rm = TRUE)
            
            p <- map_df %>%
                ggplot(aes(x = long, y = lat, fill = cum_count, group = group,
                           text = paste0("Day: ", format(date, format ="%b %d %Y"),
                                         "\n", state, ": ", round(cum_count, digits = 3)))) +
                geom_polygon(color = "white") +
                scale_fill_viridis_c(name = "Cumulative \n(log10)", trans = "log10",
                                     limits = c(1, max_lim), option = "viridis") +
                ggtitle(paste("Cumulative deaths on", 
                              format(input$map_date, format ="%b %d %Y"))) +
                theme(panel.grid.major = element_blank(), 
                      panel.background = element_blank(),
                      axis.title = element_blank(), 
                      axis.text = element_blank(),
                      axis.ticks = element_blank())  
            
        } else if (input$map_variable == "cum_count_per_100thous") {
            
            max_lim <- tidy_df %>% 
                filter(measure == "deaths") %>% 
                pull(cum_count_per_100thous) %>% 
                max(na.rm = TRUE)
            
            p <- map_df %>%
                ggplot(aes(x = long, y = lat, fill = cum_count_per_100thous, group = group,
                           text = paste0("Day: ", format(date, format ="%b %d %Y"),
                                         "\n", state, ": ", round(cum_count_per_100thous, digits = 3)))) +
                geom_polygon(color = "white") +
                scale_fill_viridis_c(name = "Cumulative \n/100k \n(log10)", trans = "log10",
                                     limits = c(1, max_lim), option = "magma") +
                ggtitle(paste("Cumulative deaths on", 
                              format(input$map_date, format ="%b %d %Y"))) +
                theme(panel.grid.major = element_blank(), 
                      panel.background = element_blank(),
                      axis.title = element_blank(), 
                      axis.text = element_blank(),
                      axis.ticks = element_blank())  
            
        } else if (input$map_variable == "new_count_7dayavg") {
            
            max_lim <- tidy_df %>% 
                filter(measure == "deaths") %>% 
                pull(new_count_7dayavg) %>% 
                max(na.rm = TRUE)
            
            p <- map_df %>%
                ggplot(aes(x = long, y = lat, fill = new_count_7dayavg, group = group,
                           text = paste0("Day: ", format(date, format ="%b %d %Y"),
                                         "\n", state, ": ", round(new_count_7dayavg, digits = 3)))) +
                geom_polygon(color = "white") +
                scale_fill_viridis_c(name = "New \n(log10)", trans = "log10",
                                     limits = c(1, max_lim), option = "plasma") +
                ggtitle(paste("New deaths (7-day average) on", 
                              format(input$map_date, format ="%b %d %Y"))) +
                theme(panel.grid.major = element_blank(), 
                      panel.background = element_blank(),
                      axis.title = element_blank(), 
                      axis.text = element_blank(),
                      axis.ticks = element_blank())
            
        } else if (input$map_variable == "new_count_7dayavg_per_1mil") {
            
            max_lim <- tidy_df %>% 
                filter(measure == "deaths") %>% 
                pull(new_count_7dayavg_per_1mil) %>% 
                max(na.rm = TRUE)
            
            p <- map_df %>%
                ggplot(aes(x = long, y = lat, fill = new_count_7dayavg_per_1mil, group = group,
                           text = paste0("Day: ", format(date, format ="%b %d %Y"),
                                         "\n", state, ": ", round(new_count_7dayavg_per_1mil, digits = 3)))) +
                geom_polygon(color = "white") +
                scale_fill_viridis_c(name = "New \n/million \n(log10)", trans = "log10",
                                     limits = c(1, max_lim), option = "inferno") +
                ggtitle(paste("New deaths (7-day average) on", 
                              format(input$map_date, format ="%b %d %Y"))) +
                theme(panel.grid.major = element_blank(), 
                      panel.background = element_blank(),
                      axis.title = element_blank(), 
                      axis.text = element_blank(),
                      axis.ticks = element_blank())
        }
        
        ggplotly(p, tooltip = "text")
        
    })
    
    # create table of top 5 states with most policies passed    
    output$policies_summary_table <- renderTable({
        policies_rank %>%
            select(state, n_policy) %>%
            rename(State = state, `# passed` = n_policy) %>%
            head(n = 5)
    }, caption = "Top 5 states with most policies passed:",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    width = "100%"
    )
    
    # create df filtered by selected state and selected date range for policies plot
    policies_plot_df <- reactive({
        tidy_df %>%
            filter(state == input$state,
                   date >= input$policies_plot_date_range[1] & date <= input$policies_plot_date_range[2])
    })
    
    # get max of new cases and deaths (7-day average)
    max_lim_policies_plot <- tidy_df %>%
        pull(new_count_7dayavg) %>% max(na.rm = TRUE)
    
    # filter df for policies plot to only include dates where policies were passed/stopped
    # this will be used to add vertical lines for started/stopped policies in plot
    policies_filtered <- reactive({
        policies_plot_df() %>% 
            filter(!is.na(policy_type)) %>%
            mutate(plot_position = max_lim_policies_plot)
    })
    
    # turn df that only includes days where policies were passed/stopped into wide df for display in app
    policies_click_table <- reactive({
        policies_filtered() %>% 
            pivot_wider(names_from = measure, 
                        values_from = c(cum_count, new_count, 
                                        new_count_7dayavg,
                                        new_count_7dayavg_per_1mil,
                                        cum_count_per_100thous))
    })
    
    # create plot of cases and deaths with policy start/stop information for selected state
    output$policies_plot <- renderPlot({
        policies_plot_df() %>%
            ggplot(aes(x = date)) +
            scale_x_date(date_breaks = "1 month", date_labels = "%b") +
            geom_vline(data = policies_filtered(),
                       aes(xintercept = date), color = "darkgrey", lty = 2) + # add vertical line for days where policy was stopped/started in that selected state
            geom_point(data = policies_filtered(), 
                       aes(y = plot_position, color = start_stop),
                       size = 2) +
            geom_line(aes(y = new_count_7dayavg, color = measure), 
                      show.legend = FALSE) +
            geom_dl(aes(y = new_count_7dayavg, label = measure, color = measure), 
                    method = list("last.points", cex = 0.90)) + # label lines to designate cases line and deaths line
            scale_y_log10(limits = c(1, max_lim_policies_plot)) + # set limits so y axis doesn't change as selected state is changed
            ggtitle(paste("New cases and deaths in", input$state, "over time")) +
            xlab("Date") +
            ylab("Count (7-day average) (log10 scale)") +
            theme_bw() +
            scale_colour_brewer(type = "qual", palette = "Paired", 
                                name = "Policy started or stopped?",
                                breaks = c("start", "stop"),
                                labels = c("Started", "Stopped")) + # use brewer color palette for cases/deaths lines and policy lines
            theme(legend.position = "bottom") 
    })
    
    # create caption for table of policies 
    output$policies_click_table_caption <- renderText({
        
        general_cap <- paste("State-wide policies for", input$state)
        
        # when plot has not been clicked on, use selected date range for caption
        if (is.null(input$policies_plot_click)) {
            
            paste(general_cap, "from", 
                  format(input$policies_plot_date_range[1], 
                         format ="%b %d %Y"),
                  "to", format(input$policies_plot_date_range[2], 
                               format ="%b %d %Y")) 
        
        # when policy plot has been clicked on...
        } else {
            
            # filter policies table to just the clicked date(s)
            click_info <- nearPoints(policies_click_table(),
                       input$policies_plot_click,
                       xvar = "date", yvar = "plot_position") %>%
                arrange(date)
            
            # create object of clicked date(s)
            clicked_date <- unique(click_info$date)
            
            # if more than one date was selected by the click, list range of the dates in the table caption
            if (length(clicked_date) != 1) {
                
                paste(general_cap, "from", 
                      format(first(clicked_date), format ="%b %d %Y"), 
                      "to", format(last(clicked_date), format ="%b %d %Y"))
            
            # if only one date was selected by the click, just list that one date in the caption
            } else {
                
                paste(general_cap, "on", 
                      format(clicked_date, format ="%b %d %Y")) 
                
            }
        }
    })
    
    # create policies table output
    output$policies_click_table <- renderDataTable({
        
        # if user hasn't clicked on plot yet, return df of all policies from that state
        if (is.null(input$policies_plot_click)) {
            
            policies_click_table() %>%
                select(date, new_count_7dayavg_cases, 
                       new_count_7dayavg_deaths,
                       policy_type, start_stop, comments) %>%
                rename(Date = date, Cases = new_count_7dayavg_cases,
                       Deaths = new_count_7dayavg_deaths, 
                       `Policy Type` = policy_type,
                       `Started/\nstopped` = start_stop, Comments = comments)
        
        # if user has clicked plot, filter df to return policies corresponding to the clicked date(s)
        } else {
            
            nearPoints(policies_click_table(),
                       input$policies_plot_click,
                       xvar = "date", yvar = "plot_position") %>%
                arrange(date) %>%
                select(date, new_count_7dayavg_cases, 
                       new_count_7dayavg_deaths,
                       policy_type, start_stop, comments) %>%
                rename(Date = date, Cases = new_count_7dayavg_cases,
                       Deaths = new_count_7dayavg_deaths, 
                       `Policy Type` = policy_type,
                       `Started/\nstopped` = start_stop, Comments = comments)
            
        }
    }, options = list("pageLength" = 10)) # show only 10 results per page of the displayed table
    
    # display user option to select states for ranking table only if "yes" has been selected for input$rank_table_custom 
    output$rank_table_dynamic <- renderUI({
        
        if (input$rank_table_custom == "Yes") {
            
            pickerInput(inputId = "rank_table_states",
                        label = "Chosen states:",
                        options = list(`actions-box` = TRUE, 
                                       `none-selected-text` = "Please select some states!"),
                        choices = as.list(levels(factor(sort(rank_table$state)))),
                        multiple = TRUE)
            
        } else {
            
            return(NULL)
            
        }
    })
    
    # change ranking table of policies, cases and deaths based on selected measure and customize option 
    rank_table_cum <- reactive({
        
        if (input$rank_table_measure == "Cumulative") {
            table <- rank_table %>% select(state, policy_comb, rank_policy,
                                  cum_cases_comb, cum_deaths_comb,
                                  rank_cum_cases, rank_cum_deaths) %>%
                rename(cases_comb = cum_cases_comb,
                       deaths_comb = cum_deaths_comb,
                       rank_cases = rank_cum_cases,
                       rank_deaths = rank_cum_deaths)
            
            if (input$rank_table_custom == "Yes") {
                
                table %>% filter(state %in% input$rank_table_states)
                
            } else if (input$rank_table_custom == "No") {
                
                table
                
            }
        } else if (input$rank_table_measure == "Cumulative per 100,000 people") {
            
            table <- rank_table %>% select(state, policy_comb, rank_policy,
                                  cum_cases_per_100thous_comb, 
                                  cum_deaths_per_100thous_comb,
                                  rank_cum_cases_per_100thous, 
                                  rank_cum_deaths_per_100thous) %>%
                rename(cases_comb = cum_cases_per_100thous_comb,
                       deaths_comb = cum_deaths_per_100thous_comb,
                       rank_cases = rank_cum_cases_per_100thous,
                       rank_deaths = rank_cum_deaths_per_100thous)
            
            if (input$rank_table_custom == "Yes") {
                
                table %>% filter(state %in% input$rank_table_states)
                
            } else if (input$rank_table_custom == "No") {
                
                table
                
            }
        }
    })
    
    # change ranking table further depending on selected variable and order
    output$rank_table_output <- renderTable({
        
        if (input$rank_table_variable == "Policies") {
            
            if (input$rank_table_order == "Ascending") {
                rank_table_cum() %>% 
                    arrange(rank_policy) %>%
                    select(-c(rank_policy, rank_cases, rank_deaths)) %>%
                    rename(State = state,
                           `Policies ranking (# passed)` = policy_comb,
                           `Cases ranking (# cases)` = cases_comb,
                           `Deaths ranking (# deaths)` = deaths_comb) %>%
                    head(n = input$rank_table_n) 
                
            } else if (input$rank_table_order == "Descending") {
                
                rank_table_cum() %>% 
                    arrange(desc(rank_policy)) %>%
                    select(-c(rank_policy, rank_cases, rank_deaths)) %>%
                    rename(State = state,
                           `Policies ranking (# passed)` = policy_comb,
                           `Cases ranking (# cases)` = cases_comb,
                           `Deaths ranking (# deaths)` = deaths_comb) %>%
                    head(n = input$rank_table_n) 
                    
            }
        } else if (input$rank_table_variable == "Cases") {
            
            if (input$rank_table_order == "Ascending") {
                rank_table_cum() %>% 
                    arrange(rank_cases) %>%
                    select(-c(rank_policy, rank_cases, rank_deaths)) %>%
                    rename(State = state,
                           `Policies ranking (# passed)` = policy_comb,
                           `Cases ranking (# cases)` = cases_comb,
                           `Deaths ranking (# deaths)` = deaths_comb) %>%
                    head(n = input$rank_table_n) 
                
            } else if (input$rank_table_order == "Descending") {
                
                rank_table_cum() %>% 
                    arrange(desc(rank_cases)) %>%
                    select(-c(rank_policy, rank_cases, rank_deaths)) %>%
                    rename(State = state,
                           `Policies ranking (# passed)` = policy_comb,
                           `Cases ranking (# cases)` = cases_comb,
                           `Deaths ranking (# deaths)` = deaths_comb) %>%
                    head(n = input$rank_table_n) 
                
            }
            
        } else if (input$rank_table_variable == "Deaths") {
            
            if (input$rank_table_order == "Ascending") {
                
                rank_table_cum() %>% 
                    arrange(rank_deaths) %>%
                    select(-c(rank_policy, rank_cases, rank_deaths)) %>%
                    rename(State = state,
                           `Policies ranking (# passed)` = policy_comb,
                           `Cases ranking (# cases)` = cases_comb,
                           `Deaths ranking (# deaths)` = deaths_comb) %>%
                    head(n = input$rank_table_n)  
                
            } else if (input$rank_table_order == "Descending") {
                
                rank_table_cum() %>% 
                    arrange(desc(rank_deaths)) %>%
                    select(-c(rank_policy, rank_cases, rank_deaths)) %>%
                    rename(State = state,
                           `Policies ranking (# passed)` = policy_comb,
                           `Cases ranking (# cases)` = cases_comb,
                           `Deaths ranking (# deaths)` = deaths_comb) %>%
                    head(n = input$rank_table_n) 
                
            }
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
