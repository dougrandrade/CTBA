#rm(list=ls())
#setwd('C:/Users/dougr/Data_Science_Projects/R/U.S. Names/Shiny')

# Load necessary packages
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(forecast)

url <- 'https://www.ssa.gov/oact/babynames/names.zip'

read_and_add_year <- function(url) {
  temp <- tempfile()
  tryCatch({
    download.file(url, temp)
    temp_dir <- tempdir()
    unzip(temp, exdir = temp_dir)
    files <- list.files(temp_dir, pattern = '*.txt', full.names = TRUE)
    data_list <- lapply(files, function(file) {
      year <- as.numeric(sub('yob(\\d{4}).txt', '\\1', basename(file)))
      data <- read.csv(file, header = FALSE, stringsAsFactors = FALSE)
      data$Year <- year
      colnames(data) <- c('Name', 'Gender', 'Records', 'Year')
      return(data)
    })
    combined_data <- do.call(rbind, data_list)
    return(combined_data)
  }, error = function(e) {
    showNotification("Failed to load data: Check URL or your connection.", type = "error")
    return(NULL)
  })
}

all_data <- read_and_add_year(url)

ui <- fluidPage(
  titlePanel('U.S. Baby Name Popularity'),
  
  sidebarLayout(
    sidebarPanel(
      textInput('name', 'Enter the first name you are interested in:', value = "Jane"),
      uiOutput('dynamic_gender_ui'),
      sliderInput('forecast_years', 'Years to Forecast:', min = 1, max = 20, value = 10),
      numericInput('year', paste('Enter the year of interest (', min(all_data$Year), '-', max(all_data$Year), '):', sep = ''), 
                   value = 2000, min = min(all_data$Year), max = max(all_data$Year), 
                   step = 1)
    ),
    
    mainPanel(
      htmlOutput('dashboard_explanation'),  # Explanation text
      plotOutput('forecast'),
      plotOutput('unique_names'),
      tags$div(HTML("Data source: <a href='https://www.ssa.gov/oact/babynames/names.zip' target='_blank'>U.S. Social Security Administration Baby Names Dataset</a>"))
    )
  )
)

server <- function(input, output, session) {
  
  output$dashboard_explanation <- renderUI({
    HTML("
      <p>This dashboard provides an interactive way to explore the popularity of baby names in the United States, as recorded by the U.S. Social Security Administration.</p>
      <p>Enter a name in the input field to view its popularity trends over the years. You can also forecast the popularity of the name for future years using the slider.</p>
      <p>The plots will display the historical popularity of the name and its uniqueness compared to other names in the dataset.</p>
    ")
  })
  
  gender_choices <- reactive({
    unique(all_data$Gender)
  })
  
  output$dynamic_gender_ui <- renderUI({
    radioButtons('gender', 'Select gender (options currently available in the database):', choices = gender_choices(), inline = TRUE)
  })
  
  filtered_data <- reactive({
    req(input$name, input$gender)
    all_data %>%
      filter(Name == input$name, Gender == input$gender)
  })
  
  unique_names <- reactive({
    all_data %>%
      filter(Gender == input$gender) %>%
      group_by(Year) %>%
      summarise(unique_records = n_distinct(Name))
  })
  
  top_names <- reactive({
    req(input$gender)
    all_data %>%
      filter(Gender == input$gender) %>%
      group_by(Year) %>%
      top_n(1, Records) %>%
      ungroup()
  })
  
  output$forecast <- renderPlot({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      ggplot() +
        labs(title = 'No data found', x = '', y = '') +
        theme_minimal()
    } else {
      MaxRec <- max(data$Records)
      year_max <- data$Year[which.max(data$Records)]
      curr_yr <- max(data$Year)
      input_rec <- data$Records[data$Year == input$year]
      
      ts_data <- ts(data$Records, start = min(data$Year), end = max(data$Year), frequency = 1)
      arima_fit <- auto.arima(ts_data, approximation = FALSE, stepwise = FALSE, seasonal = FALSE)
      forecasted <- forecast(arima_fit, h = input$forecast_years)
      
      forecast_df <- data.frame(
        Year = seq(max(data$Year) + 1, by = 1, length.out = input$forecast_years),
        Records = pmax(as.numeric(forecasted$mean), 0),
        Lower = pmax(as.numeric(forecasted$lower[,2]), 0),
        Upper = as.numeric(forecasted$upper[,2])
      )
      
      ggplot(data, aes(x = Year, y = Records)) +
        
        theme_dark() +
        
        geom_line(color = 'darkgreen', size = 1) +
        
        geom_vline(xintercept = year_max, linetype = 'dashed', color = 'darkgray', size = 0.4) +
        annotate('text', x = year_max, y = MaxRec, 
                 label = paste(year_max, ': ', MaxRec, ' records', sep = ''), 
                 vjust = 0.5, hjust = 1.1, color = 'white', size = 5) +
        
        geom_vline(xintercept = input$year, linetype = 'dashed', color = 'darkgray', size = 0.4) +
        annotate('text', x = input$year, y = input_rec, 
                 label = paste(input$year, ': ', input_rec, ' records', sep = ''), 
                 vjust = 1.5, hjust = 1.1, color = 'white', size = 5) +
        
        geom_line(data = forecast_df, aes(x = Year, y = Records), color = 'blue', linetype = 'dashed') +
        geom_ribbon(data = forecast_df, aes(x = Year, ymin = Lower, ymax = Upper), fill = 'blue', alpha = 0.2)  +
        annotate('text', x = (curr_yr + input$forecast_years), y = round(forecast_df[input$forecast_years, 2]), 
                 label = paste(curr_yr + input$forecast_years, ':\n', round(forecast_df[input$forecast_years, 2]), ' records', sep = ''), 
                 vjust = -1, hjust = .5, color = 'white', size = 5) +
        
        labs(title = paste(input$name, ' (', input$gender, '): Forecast & Popularity Over Time (1880-', curr_yr + input$forecast_years, ')', sep = ''),
             y = paste('Records of ', input$name, '(', input$gender, ')', sep = '')) +
        
        scale_x_continuous(breaks = seq(from = floor(min(data$Year) / 10) * 10, 
                                        to = ceiling(max(forecast_df$Year) / 10) * 10, 
                                        by = 10),
                           limits = c(-1 + floor(min(data$Year) / 10) * 10,
                                      1 + ceiling(max(forecast_df$Year) / 10) * 10)) +
        scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5)) +
        
        theme(plot.title = element_text(size = 15, hjust = 0.5, color = 'darkgreen', face = 'bold'),
              axis.title = element_text(size = 12),
              plot.title.position = "plot")

    }
  })
  
  output$unique_names <- renderPlot({
    unique_names_data <- unique_names()
    top_names_data <- top_names()
    filtered_data <- filtered_data()
    
    ggplot(unique_names_data, aes(x = Year, y = unique_records)) +
      
      theme_dark() +
      
      geom_line(color = 'darkblue', size = 1) +
      geom_point(data = unique_names_data[unique_names_data$Year == input$year, ], 
                 aes(x = Year, y = unique_records), 
                 color = 'white', size = 3.5) +
      annotate('text', x = min(unique_names_data$Year) + (max(unique_names_data$Year) - min(unique_names_data$Year)) * 0.1, 
               y = max(unique_names_data$unique_records) * 0.9, 
               label = paste('In ', input$year, ':\n', 
                             input$name, ' - ', filtered_data[filtered_data$Year == input$year, ]$Records, ' records', '\n',
                             top_names_data[top_names_data$Year == input$year, ]$Name, ' - most popular name with ', 
                             top_names_data[top_names_data$Year == input$year, ]$Records, ' records\n',
                             'Unique names - ',unique_names_data[unique_names_data$Year == input$year, ]$unique_records, 
                             sep = ''), 
               vjust = 0.5, hjust = 0, color = 'white', size = 5) +
      labs(title = paste('Unique Name Records Over Time (', input$gender, ')', sep = ''),
           y = paste('Unique Names (', input$gender, ')', sep = '')) +

      scale_x_continuous(breaks = seq(from = floor(min(unique_names_data$Year) / 10) * 10, 
                                      to = ceiling(max(unique_names_data$Year + input$forecast_years) / 10) * 10, 
                                      by = 10),
                         limits = c(-1 + floor(min(unique_names_data$Year) / 10) * 10,
                                    1 + ceiling(max(unique_names_data$Year + input$forecast_years) / 10) * 10)) +
      scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5)) +
      
      theme(plot.title = element_text(size = 15, hjust = 0.5, color = 'darkblue', face = 'bold'),
            axis.title = element_text(size = 12),
            plot.title.position = "plot")
  })
}

shinyApp(ui = ui, server = server)
