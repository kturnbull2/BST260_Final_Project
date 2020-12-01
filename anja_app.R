library(shiny)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(rvest)
library(stringr)
library(zoo)
library(directlabels)

# download state-level cases and deaths data from nytimes
url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
covid <- read_csv(url)

# download policy data
url <- "https://healthdata.gov/sites/default/files/state_policy_updates_20201125_0719.csv"
tmp_filename <- tempfile()
download.file(url, tmp_filename)
policies <- read_csv(tmp_filename)
file.remove(tmp_filename)

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
tidy_df <- covid %>% 
    gather(measure, cum_count, cases:deaths)

# create 7 day rolling average of cases and deaths
tidy_df <- tidy_df %>% 
    group_by(state, measure) %>%
    arrange(state, measure, date) %>%
    mutate(new_count = cum_count - lag(cum_count),
           new_count_7dayavg = rollmean(new_count, k = 7, fill = NA)) %>%
    ungroup()

# join population data to covid data frame
tidy_df <- tidy_df %>% left_join(population, by = "state")

# create per 10 million population variables for 7 day average variables
tidy_df <- tidy_df %>% 
    mutate(new_count_7dayavg_per_1mil = new_count_7dayavg / (population / 1000000),
           cum_count_per_100thous = cum_count / (population / 100000))

# remove county-level policies data to keep only state-level policies 
policies <- policies %>% 
    filter(policy_level == "state") %>%
    rename(state = state_id)

# removed row that was incorrectly labeled as state-level policy
policies <- policies %>% 
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

policies <- policies %>% 
    mutate(state = recode(state, !!!key))

# join combined covid and population data with policies data
tidy_df <- tidy_df %>% left_join(policies, by = c("state", "date"))

# create wide_df for summary tables in app 
wide_df <- tidy_df %>%
    pivot_wider(names_from = measure,
                values_from = c(cum_count, new_count,
                                new_count_7dayavg,
                                new_count_7dayavg_per_1mil, 
                                cum_count_per_100thous))

# create table with ranks for policies, cum_cases, cum_deaths, cum_cases_per_100thous, and cum_deaths_per_100thous
policies_rank <- wide_df %>%
    filter(!is.na(policy_type), start_stop == "start") %>%
    group_by(state) %>% 
    summarise(n_policy = n()) %>%
    arrange(desc(n_policy)) %>% 
    mutate(rank_policy = as.numeric(rownames(.)), 
           policy_comb = paste0(rank_policy, " (", n_policy, ")")) %>%
    ungroup()

cum_ranks <- wide_df %>%
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

rank_table <- policies_rank %>% left_join(cum_ranks, by = "state")

# Define UI 
ui <- navbarPage(
    theme = shinytheme("yeti"),
    "Covid-19 App",
    tabPanel("Comparing States",
             sidebarPanel(),
             mainPanel()),
    navbarMenu("Exploring State-Wide Policies",
               tabPanel("Time Trends",
                   sidebarLayout(
                       sidebarPanel(width = 5,
                                    p("Filter your selected states' state-wide policy table further by clicking",
                                      "on the point corresponding to your policy of interest on the plot,",
                                      "modifying the date range or searching the table!"),
                                    pickerInput(inputId = "state", 
                                                label = "Select state:",
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
                       mainPanel(width = 7,
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
                                            label = "Select # states to display:",
                                            min = 1, 
                                            max = 55, 
                                            value = 15, 
                                            ticks = FALSE),
                                pickerInput(inputId = "rank_table_custom", 
                                            label = "Customize which states to display:",   
                                            choices = c("No", "Yes"), 
                                            selected = c("Ascending"),
                                            multiple = FALSE),
                                uiOutput(outputId = "rank_table_dynamic")
                            ),
                            mainPanel(tableOutput(outputId = "rank_table_output"))
                        )
               )
    )
)

# Define server
server <- function(input, output) {
    
    output$policies_summary_table <- renderTable({
        policies_rank %>%
            select(state, n_policy) %>%
            rename(State = state, `# passed` = n_policy) %>%
            head(n = 5)
    }, caption = "Top 5 states with most policies passed:",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    width = "100%"
    )
    
    policies_plot_df <- reactive({
        tidy_df %>%
            filter(state == input$state,
                   date >= input$policies_plot_date_range[1] & date <= input$policies_plot_date_range[2])
    })
    
    policies_filtered <- reactive({
        policies_plot_df() %>% 
            filter(!is.na(policy_type)) %>%
            mutate(plot_position = 15000)
    })
    
    policies_click_table <- reactive({
        policies_filtered() %>% 
            pivot_wider(names_from = measure, 
                        values_from = c(cum_count, new_count, 
                                        new_count_7dayavg,
                                        new_count_7dayavg_per_1mil,
                                        cum_count_per_100thous))
    })
    
    output$policies_plot <- renderPlot({
        policies_plot_df() %>%
            ggplot(aes(x = date)) +
            scale_x_date(date_breaks = "1 month", date_labels = "%b") +
            geom_vline(data = policies_filtered(),
                       aes(xintercept = date), color = "darkgrey", lty = 2) +
            geom_point(data = policies_filtered(), 
                       aes(y = plot_position, color = start_stop),
                       size = 2) +
            geom_line(aes(y = new_count_7dayavg, color = measure), 
                      show.legend = FALSE) +
            geom_dl(aes(y = new_count_7dayavg, label = measure, color = measure), 
                    method = list("last.points", cex = 0.90)) +
            scale_y_log10(limits = c(1, 15000)) +
            ggtitle(paste("New cases and deaths in", input$state, "over time")) +
            xlab("Date") +
            ylab("Count (7-day average) (log10 scale)") +
            theme_bw() +
            scale_colour_brewer(type = "qual", palette = "Paired",
                                name = "Policy started or stopped?",
                                   breaks = c("start", "stop"),
                                   labels = c("Started", "Stopped")) +
            theme(legend.position = "bottom")
    })
    
    output$policies_click_table_caption <- renderText({
        general_cap <-  paste("State-wide policies for", input$state)
        
        if (is.null(input$policies_plot_click)) {
            paste(general_cap, "from", 
                  format(input$policies_plot_date_range[1], 
                         format ="%b %d %Y"),
                  "to", format(input$policies_plot_date_range[2], 
                               format ="%b %d %Y")) 
        } else {
            click_info <- nearPoints(policies_click_table(),
                       input$policies_plot_click,
                       xvar = "date", yvar = "plot_position") %>%
                arrange(date)
            clicked_date <- unique(click_info$date)
            
            if (length(clicked_date) != 1) {
                paste(general_cap, "from", 
                      format(first(clicked_date), format ="%b %d %Y"), 
                      "to", format(last(clicked_date), format ="%b %d %Y"))
            } else {
                paste(general_cap, "on", 
                      format(clicked_date, format ="%b %d %Y")) 
            }
        }
    })
    
    output$policies_click_table <- renderDataTable({
        if (is.null(input$policies_plot_click)) {
            policies_click_table() %>%
                select(date, new_count_7dayavg_cases, 
                       new_count_7dayavg_deaths,
                       policy_type, start_stop, comments) %>%
                rename(Date = date, Cases = new_count_7dayavg_cases,
                       Deaths = new_count_7dayavg_deaths, 
                       `Policy Type` = policy_type,
                       `Started/\nstopped` = start_stop, Comments = comments)
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
    })
    
    output$rank_table_dynamic <- renderUI({
        if (input$rank_table_custom == "Yes") {
            pickerInput(inputId = "rank_table_states",
                        label = "Select states to compare",
                        options = list(`actions-box` = TRUE, 
                                       `none-selected-text` = "Please select some states!"),
                        choices = as.list(levels(factor(sort(rank_table$state)))),
                        multiple = TRUE)
        } else {
            return(NULL)
        }
    })
    
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
