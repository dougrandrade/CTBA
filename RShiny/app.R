#rm(list=ls())
# Load necessary packages
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(forecast)
library(rsconnect)
################################################################################
#### Retrieve the Social Security Administration Names Database
################################################################################
# Define the URL and the destination file path
url <- 'https://www.ssa.gov/oact/babynames/names.zip'
# Function to read data directly from the URL and add the year column
read_and_add_year <- function(url) {
  temp <- tempfile()
  tryCatch({
    # Download the ZIP file to the temporary file
    download.file(url, temp)
    # Extract the files from the ZIP into a temporary directory
    temp_dir <- tempdir()
    unzip(temp, exdir = temp_dir)
    # Read all the data files in the extracted directory
    files <- list.files(temp_dir, 
                        pattern = '*.txt', 
                        full.names = TRUE)
    # Process each file and add the year column
    data_list <- lapply(files, function(file) {
      year <- as.numeric(sub('yob(\\d{4}).txt', '\\1', basename(file)))
      data <- read.csv(file, 
                       header = FALSE, 
                       stringsAsFactors = FALSE)
      data$Year <- year
      colnames(data) <- c('Name', 'Gender', 'Records', 'Year')
      return(data)})
    # Combine the data from all years into a single data frame
    combined_data <- do.call(rbind, data_list)
    # Return the combined data
    return(combined_data)
  }, error = function(e) {
    showNotification('Failed to load data: Check URL or your connection.', type = 'error')
    return(NULL)}
  )}
all_data <- read_and_add_year(url)
# Helper function to calculate rank for a given name, gender, and year
calculate_rank <- function(data, year, name, gender) {
  year_data <- data %>%
    filter(Year == year, Gender == gender) %>%
    arrange(desc(Records))
  rank <- which(year_data$Name == name)
  if (length(rank) == 0) {
    return(NA)
  } else {
    return(rank)
  }
}
################################################################################
#### Build the User Interface (UI)
################################################################################
ui <- fluidPage(
  tags$head(
    # Add Open Graph meta tags for sharing on social media
    tags$meta(property = "og:title", content = 'Rshiny Dashboard Analysis of Registered U.S. Baby Names'),
    tags$meta(property = "og:description", content = 'This dashboard provides an interactive way to explore the popularity of baby names in the United States, as recorded by the U.S. Social Security Administration.'),
    tags$meta(property = "og:image", content = 'https://www.momswhothink.com/wp-content/uploads/old-baby-picture-3-e1613427463319-360x229.jpg'),
    tags$meta(property = "og:url", content = 'https://drandrade.shinyapps.io/shiny/'),
    tags$meta(property = "og:type", content = 'website')
  ),
  # Create a dashboard title
  titlePanel('U.S. Social Security Administration Baby Name Registration Analysis'),
  # Create the dashboard interface Outline
  sidebarLayout(
    # Create the sidebar section for user input - name, gender, forecast length, year
    sidebarPanel(
      # User input for name of interest
      textInput('name', 'Enter the first name you are interested in:', value = "Jane"),
      # User input for gender of interest (selection limited to what is available in the database)
      uiOutput('dynamic_gender_ui'),
      # User input for name forecast length
      sliderInput('forecast_years', 'Years to Forecast:', min = 1, max = 20, value = 10),
      # User input for year of interest
      numericInput('year',
                   paste('Enter the year of interest (', min(all_data$Year), '-', max(all_data$Year), '):', sep = ''),
                   value = 2000, # default value
                   min = min(all_data$Year), # first year of name records
                   max = max(all_data$Year), # last year of name records
                   step = 1)), # increment by year
    mainPanel(
      # Dashboard's description section
      htmlOutput('dashboard_explanation'),
      # Dashboard's forecast plot section
      plotOutput('forecast'),
      # Dashboard's unique names plot section
      plotOutput('unique_names'),
      # Dashboard's database source link section
      tags$div(HTML("Data source: <a href='https://www.ssa.gov/oact/babynames/names.zip' target='_blank'>U.S. Social Security Administration Baby Names Dataset</a>"))))
)
################################################################################
#### Define the Server Logic (UI output, data set filtering, forecasting, plots)
################################################################################
server <- function(input, output, session) {
  output$dashboard_explanation <- renderUI({
    HTML("
      <p>This dashboard provides an interactive way to explore the popularity of baby names in the United States, as recorded by the U.S. Social Security Administration.</p>
      <p>Enter a name in the input field to view its popularity trends over the years. You can also forecast the popularity of the name for future years using the slider.</p>
      <p>The plots will display the historical popularity of the name and its uniqueness compared to other names in the dataset.</p>
    ")
  })
  # Gender choice dynamically updated based on unique categories in the database
  gender_choices <- reactive({
    unique(all_data$Gender)
  })
  # Gender input configuration - radio buttons
  output$dynamic_gender_ui <- renderUI({
    radioButtons('gender', 'Select gender (options currently available in the database):', choices = gender_choices(), inline = TRUE)
  })
  # Filter the name database based on the input name and gender
  filtered_data <- reactive({
    req(input$name, input$gender)
    all_data %>%
      filter(Name == input$name, Gender == input$gender)
  })
  # Filter out the number of unique names for each year based on the input gender
  unique_names <- reactive({
    req(input$gender)
    all_data %>%
      filter(Gender == input$gender) %>%
      group_by(Year) %>%
      summarise(unique_records = n_distinct(Name))
  })
  # Filter out the gender of interest most popular name for each year
  top_names <- reactive({
    req(input$gender)
    all_data %>%
      filter(Gender == input$gender) %>%
      group_by(Year) %>%
      top_n(1, Records) %>%
      ungroup()
  })
  # Get each year's least common name
  bottom_names <- reactive({
    req(input$gender)
    all_data %>%
      filter(Gender == input$gender) %>%
      group_by(Year) %>%
      top_n(-1, Records) %>%
      ungroup()
  })
  # Calculate the ranks for min, max, and input years
  # Calculate the ranks for min, max, and input years
  ranks <- reactive({
    req(input$name, input$gender, input$year)
    # Calculate rank for the input year
    rank_input <- calculate_rank(all_data, input$year, input$name, input$gender)
    return(list(rank_input = rank_input))  # Ensure to return a list with rank_input
  })
  # Render the forecast plot
  output$forecast <- renderPlot({
    # Reference the name and gender filtered database
    data <- filtered_data()
    
    rank_info <- ranks()
    
    # Check in case there is no names for the gender selected
    if (nrow(data) == 0) {
      ggplot() +
        labs(title = 'No data found', x = '', y = '') +
        theme_minimal()
      # Continue with full plot if there is data for the name and gender
    } else {
      # Retrieve the highest record of the name of interest
      MaxRec <- max(data$Records)
      # Retrieve the year associated with the highest records of name of interest
      year_max <- data$Year[which.max(data$Records)]
      # Retrieve the most current (latest) year of records in the database
      curr_yr <- max(data$Year)
      # Retrieve the number of records for the name of interest of the year of interest
      input_rec <- data$Records[data$Year == input$year]
      
      # Convert the filtered data to time series
      ts_data <- ts(data$Records, start = min(data$Year), end = max(data$Year), frequency = 1)
      # Run an auto arima analysis of the time series (auto select the best parameters)
      arima_fit <- auto.arima(ts_data, approximation = FALSE, stepwise = FALSE, seasonal = FALSE)
      # Apply the arima model to forecast the future number of records for the name and gender of interest
      forecasted <- forecast(arima_fit, h = input$forecast_years)
      # Consolidate the forecast records into a data frame for plotting, with confidence intervals
      forecast_df <- data.frame(
        Year = seq(max(data$Year) + 1, by = 1, length.out = input$forecast_years),
        Records = pmax(as.numeric(forecasted$mean), 0),
        Lower = pmax(as.numeric(forecasted$lower[, 2]), 0), # 95% confidence interval
        Upper = as.numeric(forecasted$upper[, 2])  # 95% confidence interval
      )
      # Create the plot, beginning with the historical data of the name and gender of interest
      ggplot(data, aes(x = Year, y = Records)) +
        # Set the plot theme
        theme_dark() +
        # Historical data line plot
        geom_line(color = 'darkgreen', size = 1) +
        # Vertical dashed line to mark the year with the highest number of records
        geom_vline(xintercept = year_max, linetype = 'dashed', color = 'darkgray', size = 0.4) +
        annotate('text', x = year_max, y = MaxRec, 
                 label = paste(year_max, ': ', MaxRec, ' records', sep = ''), 
                 vjust = 0.5, hjust = 1.1, color = 'white', size = 5) +
        # Vertical dashed line to mark the year of interest and its records
        geom_vline(xintercept = input$year, linetype = 'dashed', color = 'darkgray', size = 0.4) +
        annotate('text', x = input$year, y = input_rec, 
                 label = paste(input$year, ': ', input_rec, ' records', sep = ''), 
                 vjust = 1.5, hjust = 1.1, color = 'white', size = 5) +
        # Append the forecast data a blue line, with shaded confidence intervals 
        geom_line(data = forecast_df, aes(x = Year, y = Records), color = 'blue', linetype = 'dashed') +
        geom_ribbon(data = forecast_df, aes(x = Year, ymin = Lower, ymax = Upper), fill = 'blue', alpha = 0.2)  +
        annotate('text', x = (curr_yr + input$forecast_years), y = round(forecast_df[input$forecast_years, 2]), 
                 label = paste(curr_yr + input$forecast_years, ':\n', round(forecast_df[input$forecast_years, 2]), ' records', sep = ''), 
                 vjust = -1, hjust = .5, color = 'white', size = 5) +
        # Title and axis labels
        labs(title = paste(input$name, ' (', input$gender, '): Forecast & Popularity Over Time (1880-', curr_yr + input$forecast_years, ')', sep = ''),
             y = paste('Records of ', input$name, '(', input$gender, ')', sep = '')) +
        # Customized scales based on range of filtered data
        scale_x_continuous(breaks = seq(from = floor(min(data$Year) / 10) * 10, 
                                        to = ceiling(max(forecast_df$Year) / 10) * 10, 
                                        by = 10),
                           limits = c(-1 + floor(min(data$Year) / 10) * 10,
                                      1 + ceiling(max(forecast_df$Year) / 10) * 10)) +
        scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5)) +
        # Adjusted plot theme features 
        theme(plot.title = element_text(size = 15, hjust = 0.5, color = 'darkgreen', face = 'bold'),
              axis.title = element_text(size = 12),
              plot.title.position = "plot")
    }
  })
  # Render the unique names plot
  output$unique_names <- renderPlot({
    rank_data <- ranks()
    print(str(rank_data))
    # Reference the unique names filter data set
    unique_names_data <- unique_names()
    # Reference the top names filtered data set
    top_names_data <- top_names()
    # Reference the bottom names filtered data set
    bottom_names_data <- bottom_names()
    # Reference the namd and gender filtered data set
    filtered_data <- filtered_data()
    # Ensure we have data
    validate(need(nrow(unique_names_data) > 0, "No unique name data available."))
    # Create the unique names line plot
    ggplot(unique_names_data, aes(x = Year, y = unique_records)) +
      # Set the plot theme
      theme_dark() +
      # Unique names line plot
      geom_line(color = 'darkblue', size = 1) +
      # Append a white dot marker to highlight the number of unique names records for the year of interest
      geom_point(data = unique_names_data[unique_names_data$Year == input$year, ], 
                 aes(x = Year, y = unique_records), 
                 color = 'white', size = 3.5) +
      annotate('text', x = min(unique_names_data$Year) + (max(unique_names_data$Year) - min(unique_names_data$Year)) * 0.1, 
               y = max(unique_names_data$unique_records), 
               label = paste('In ', input$year, ':\n', 
                             # Plot text of the number of unique names for the year and gender of interest
                             '  Unique names - ', unique_names_data[unique_names_data$Year == input$year, ]$unique_records, '\n',
                             # Plot text of the number of records for name and gender and year of interest
                             '  ', input$name, ' - ranks ', ranks()$rank_input, ' of ', unique_names_data[unique_names_data$Year == input$year, ]$unique_records, ', with ', filtered_data[filtered_data$Year == input$year, ]$Records, ' records', '\n',
                             # Plot text of the most popular name for the year and gender of interest
                             '  ', head(top_names_data[top_names_data$Year == input$year, ]$Name, n = 1), ' - most popular name with ', 
                             head(top_names_data[top_names_data$Year == input$year, ]$Records, n = 1), ' records\n',
                             # Plot text of the least popular name for the year and gender of interest
                             '  ', tail(bottom_names_data[bottom_names_data$Year == input$year, ]$Name, n = 1), ' - least popular name with ', 
                             tail(bottom_names_data[bottom_names_data$Year == input$year, ]$Records, n = 1), ' records\n',
                             sep = ''),
               vjust = 1, hjust = 0, color = 'white', size = 5) +
      # Title and axis labels
      labs(title = paste('Unique Name Records Over Time (', input$gender, ')', sep = ''),
           y = paste('Unique Names (', input$gender, ')', sep = '')) +
      # Customized scale based on the range of filtered data that matches the forecast's range of years
      scale_x_continuous(breaks = seq(from = floor(min(unique_names_data$Year) / 10) * 10, 
                                      to = ceiling(max(unique_names_data$Year + input$forecast_years) / 10) * 10, 
                                      by = 10),
                         limits = c(-1 + floor(min(unique_names_data$Year) / 10) * 10,
                                    1 + ceiling(max(unique_names_data$Year + input$forecast_years) / 10) * 10)) +
      scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5)) +
      # Adjusted plot theme features
      theme(plot.title = element_text(size = 15, hjust = 0.5, color = 'darkblue', face = 'bold'),
            axis.title = element_text(size = 12),
            plot.title.position = "plot")
  })
}
################################################################################
#### Run the RShiny App
shinyApp(ui = ui, server = server)