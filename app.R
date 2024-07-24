rm(list=ls())

setwd('C:/Users/dougr/Data_Science_Projects/R/U.S. Names')

# Load necessary packages
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)  # For pretty_breaks()

# Define the URL and the destination file path
url <- 'https://www.ssa.gov/oact/babynames/names.zip'
destfile <- 'names.zip'

# Download the ZIP file
download.file(url, destfile)

# Unzip the file
unzip(destfile, exdir = 'names')

# List the files in the extracted folder
files <- list.files('names', pattern = '*.txt', full.names = TRUE)

# Function to read data and add the year column
read_and_add_year <- function(directory) {
  files <- list.files(directory, pattern = '*.txt', full.names = TRUE)
  data_list <- lapply(files, function(file) {
    year <- as.numeric(sub('yob(\\d{4}).txt', '\\1', basename(file)))
    data <- read.csv(file, header = FALSE, stringsAsFactors = FALSE)
    data$Year <- year
    colnames(data) <- c('Name', 'Gender', 'Records', 'Year')
    return(data)
  })
  combined_data <- do.call(rbind, data_list)
  combined_data
}

# Define UI for application
ui <- fluidPage(
  titlePanel('Baby Name Popularity'),
  sidebarLayout(
    sidebarPanel(
      textInput('name', 'Enter the first name you are interested in:'),
      selectInput('gender', 'Select gender:', choices = c('Male' = 'M', 'Female' = 'F'))
    ),
    mainPanel(
      plotOutput('plot')
    )
  )
)

# Define server logic
server <- function(input, output) {
  filtered_data <- reactive({
    # Read data
    data <- read_and_add_year('names')

    # Filter data based on inputs
    filtered <- data %>%
      filter(Name == input$name, Gender == input$gender)
  })
  
  # Plot Output
  output$plot <- renderPlot({
    req(input$name, input$gender)
    
    filtered_data <- filtered_data()
    
    if (nrow(filtered_data) == 0) {
    #if (is.null(filtered_data)) {
      return(ggplot() +
               labs(title = 'No data found', x = '', y = '') +
               theme_minimal())
    }
    
    # Find the year with the maximum count
    MaxRec <- max(filtered_data$Records)
    year_max <- filtered_data$Year[which.max(filtered_data$Records)]
    # FInd the current year records count
    curr_yr <- max(filtered_data$Year)
    last_rec <- filtered_data$Records[which.max(filtered_data$Year)]
    
    ggplot(filtered_data, aes(x = Year, y = Records)) +
      geom_line(color = 'darkgreen', size = 1) +
      
      geom_vline(xintercept = year_max, linetype = 'dashed', color = 'white', size = .4) +
      annotate('text', x = year_max, y = MaxRec, 
               label = paste(MaxRec, ' records in ', year_max, sep = ''), 
               vjust = -.5,
               hjust = 1.1,
               color = 'white') +
      
      geom_vline(xintercept = curr_yr, linetype = 'dashed', color = 'white', size = .4) +
      annotate('text', x = curr_yr, y = last_rec, 
               label = paste(last_rec, ' records in ', curr_yr, sep = ''), 
               vjust = 1.5, 
               hjust = 1.1, 
               color = 'white') +
      
      labs(title = paste(input$name, ' (', input$gender, ') Popularity Over Time (1880-2019)', sep = ''),
           x = 'Year',
           y = 'Total Records') +
      theme_dark() +
      theme(plot.title = element_text(size = 20, hjust = 0.5),
            axis.title = element_text(size = 12)) +
      scale_x_continuous(breaks = seq(from = floor(min(filtered_data$Year) / 10) * 10, 
                                      to = ceiling(max(filtered_data$Year) / 10) * 10, 
                                      by = 10),
                         limits = c(-1 + floor(min(filtered_data$Year) / 10) * 10,
                                    1 + ceiling(max(filtered_data$Year) / 10) * 10)) +
      scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 5))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
