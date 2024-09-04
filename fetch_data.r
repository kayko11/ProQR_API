# Load required packages
library(httr)
library(jsonlite)  # For working with JSON data
library(dplyr)     # For data manipulation
library(tidyr)     # For handling missing values

# Define the API endpoint
api_url <- "http://localhost:5000/api/notices/"

# Make a GET request to the API endpoint
response <- GET(api_url)

# Check if the request was successful
if (status_code(response) == 200) {
  # Parse the response content as JSON
  data <- content(response, "parsed", simplifyVector = TRUE)
  
  # Convert JSON data to a data frame
  df <- as.data.frame(data)
  
  # Example manipulations
  
  # 1. Filter data where item_awardedValue > 50000000
  high_value_notices <- df %>% filter(item_awardedValue > 50000000)
  
  # 2. Select specific columns
  selected_columns <- df %>% select(item_awardedSupplier, item_awardedValue, item_publishedDate)
  
  # 3. Mutate data: Create a new column for the duration between published and awarded date
  df <- df %>%
    mutate(
      item_publishedDate = as.Date(item_publishedDate),
      item_awardedDate = as.Date(item_awardedDate),
      duration_days = as.numeric(difftime(item_awardedDate, item_publishedDate, units = "days"))
    )
  
  # 4. Handle missing values: Replace NA with a default value
  df <- df %>% replace_na(list(item_awardedSupplier = "Unknown"))
  
  # Save the manipulated data to CSV files
  write.csv(high_value_notices, "high_value_notices.csv", row.names = FALSE)
  write.csv(selected_columns, "selected_columns.csv", row.names = FALSE)
  write.csv(df, "manipulated_data.csv", row.names = FALSE)

} else {
  # Print an error message if the request was not successful
  print(paste("Failed to fetch data. Status code:", status_code(response)))
}
