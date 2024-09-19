rm(list=ls())
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)  # For pretty_breaks()


# Define the URL and the destination file path
url <- 'https://www.ssa.gov/oact/babynames/names.zip'
########
# Function to read data directly from the URL and add the year column
read_and_add_year <- function(url) {
  # Create a temporary file
  temp <- tempfile()
  
  tryCatch({
    # Download the ZIP file to the temporary file
    download.file(url, temp)
    
    # Extract the files from the ZIP into a temporary directory
    temp_dir <- tempdir()
    unzip(temp, exdir = temp_dir)
    
    # Read all the data files in the extracted directory
    files <- list.files(temp_dir, pattern = '*.txt', full.names = TRUE)
    
    # Process each file and add the year column
    data_list <- lapply(files, function(file) {
      year <- as.numeric(sub('yob(\\d{4}).txt', '\\1', basename(file)))
      data <- read.csv(file, header = FALSE, stringsAsFactors = FALSE)
      data$Year <- year
      colnames(data) <- c('Name', 'Gender', 'Records', 'Year')
      return(data)
    })
    # Combine the data from all years into a single data frame
    combined_data <- do.call(rbind, data_list)
    
    # Return the combined data
    return(combined_data)
  }, error = function(e) {
    showNotification("Failed to load data: Check URL or your connection.", type = "error")
    return(NULL)
  })
}
all_data <- read_and_add_year(url)


# Get user input
#YourName <- readline(prompt = 'Enter the first name you are interested in: ')
YourName <- 'Douglas'
#YourGender <- readline(prompt = 'Enter if male or female (M or F): ')
YourGender <- 'M'

# Filter the data based on the user input
filtered_data <- all_data %>%
  filter(Name == YourName, Gender == YourGender)

summary(filtered_data)

# Find the year with the maximum count
MaxRec <- max(filtered_data$Records)
year_max <- filtered_data$Year[which.max(filtered_data$Records)]
curr_yr <- max(filtered_data$Year)
last_rec <- filtered_data$Records[which.max(filtered_data$Year)]

# Plot the data
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
  
  labs(title = paste(YourName, ' (', YourGender, ') Popularity Over Time (1880-2019)', sep = ''),
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
  

library(forecast)
records <- ts(filtered_data[3], start = 1880, frequency = 1)
head(records)

#library(urca)
#ur.kpss(records) # no differencing required (high p-value)

#autoplot(diff(log(records)))
records_arima <- auto.arima(records, approximation = FALSE, stepwise = FALSE, seasonal = FALSE)
#records_arima
autoplot(forecast(records_arima))
checkresiduals(records_arima)

############################################
# Ensure the filtered_data contains records over time
filtered_data <- filtered_data()

if (nrow(filtered_data) > 0) {
  # Step 1: Convert data to a time series object
  ts_data <- ts(filtered_data$Records, start = min(filtered_data$Year), end = max(filtered_data$Year), frequency = 1)
  
  # Step 2: Fit an ARIMA model
  arima_fit <- auto.arima(ts_data, approximation = FALSE, stepwise = FALSE, seasonal = FALSE)
  
  # Step 3: Forecast for the next 10 years (adjust as needed)
  forecasted <- forecast(arima_fit, h = 10)
  
  # Step 4: Prepare the forecast data for plotting
  forecast_df <- data.frame(
    Year = seq(max(filtered_data$Year) + 1, by = 1, length.out = 10),
    Records = as.numeric(forecasted$mean),
    Lower = as.numeric(forecasted$lower[,2]), # 95% confidence interval
    Upper = as.numeric(forecasted$upper[,2])  # 95% confidence interval
  )
  
  # Step 5: Plot the original data
  ggplot(filtered_data, aes(x = Year, y = Records)) +
    geom_line(color = 'darkgreen', size = 1) +
    
    geom_vline(xintercept = year_max, linetype = 'dashed', color = 'white', size = 0.4) +
    annotate('text', x = year_max, y = MaxRec, 
             label = paste(year_max, ':\n', MaxRec, ' records', sep = ''), 
             vjust = 0.5, hjust = 1.1, color = 'white', size = 2.5) +
    
    geom_vline(xintercept = curr_yr, linetype = 'dashed', color = 'white', size = 0.4) +
    annotate('text', x = curr_yr, y = last_rec, 
             label = paste(curr_yr, ':\n', last_rec, ' records', sep = ''), 
             vjust = 1.5, hjust = 1.1, color = 'white', size = 2.5) +
    
    # Add forecasted data
    geom_line(data = forecast_df, aes(x = Year, y = Records), color = 'blue', linetype = 'dashed') +
    geom_ribbon(data = forecast_df, aes(x = Year, ymin = Lower, ymax = Upper), fill = 'blue', alpha = 0.2) +
    
    labs(title = paste(YourName, ' (', YourGender, ') Popularity Over Time (1880-', curr_yr + 10, ')', sep = ''),
         x = 'Year', y = 'Total Records') +
    theme_dark() +
    theme(plot.title = element_text(size = 20, hjust = 0.5),
          axis.title = element_text(size = 12)) +
    scale_x_continuous(breaks = seq(from = floor(min(filtered_data$Year) / 10) * 10, 
                                    to = ceiling(max(forecast_df$Year) / 10) * 10, 
                                    by = 10),
                       limits = c(-1 + floor(min(filtered_data$Year) / 10) * 10,
                                  1 + ceiling(max(forecast_df$Year) / 10) * 10)) +
    scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5)) +
    
    annotate('text', x = (curr_yr + 10), y = round(forecast_df[10, 2]), 
             label = paste(curr_yr + 10, ':\n', round(forecast_df[10, 2]), ' records', sep = ''), 
             vjust = -1.5, hjust = -.1, color = 'white', size = 2.5)
}

########################
# General Name Analysis

library(ggplot2)

library(dplyr)

head(all_data)

#year_input <- readline(prompt = 'Enter the year you are interested in: ')
year_input <- 1985

# Get each year's most common name
top_names <- all_data %>%
  group_by(Year) %>%
  top_n(1, Records) %>%
  ungroup()

# Get the total number of unique names for each year
unique_names <- all_data %>%
  group_by(Year) %>%
  summarise(unique_records = n_distinct(Name))

ggplot(unique_names, aes(x = Year, y = unique_records)) +
  geom_line()  +
  labs(title = "Name Records Over Time",
       x = "Year",
       y = "Number of Unique Records") +
 
  geom_point(data = unique_names[unique_names$Year == year_input, ], 
             aes(x = Year, y = Unique_Names), 
             color = 'white', size = 3)  +
  annotate('text', x = year_input, y = unique_names[unique_names$Year == year_input, ]$unique_records, 
           label = paste(year_input, ':\n', 
                         top_names[top_names$Year == year_input, ]$Records, ' records of ', 
                         top_names[top_names$Year == year_input, ]$Name, sep = ''), 
           vjust = 0.5, hjust = 1.1, color = 'white', size = 2.5) +
  
  theme_dark()
