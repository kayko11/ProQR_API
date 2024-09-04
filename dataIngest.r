library(httr)
library(dplyr)
library(jsonlite)

# Global variable to store cached data
cached_data <- NULL

# Function to fetch data from the API
fetch_data <- function(force_refresh = FALSE) {
  if (!is.null(cached_data) && !force_refresh) {
    return(cached_data)
  }
  
  tryCatch({
    response <- GET("http://localhost:5000/api/notices/")
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text"))
      
      # Check if expected columns exist
      required_columns <- c("item_awardedSupplier", "item_awardedValue")
      if (!all(required_columns %in% names(data))) {
        stop("API response is missing required columns")
      }
      
      # Ensure item_awardedValue is numeric
      data$item_awardedValue <- as.numeric(data$item_awardedValue)
      
      cached_data <<- data
      return(data)
    } else {
      stop(paste("API request failed with status code:", status_code(response)))
    }
  }, error = function(e) {
    stop(paste("Error fetching data from API:", e$message))
  })
}

# Function to get total contracts by supplier
get_total_contracts <- function(supplier_name) {
  my_data <- fetch_data()
  total_contracts <- my_data %>%
    filter(grepl(supplier_name, item_awardedSupplier, ignore.case = TRUE)) %>%
    summarise(total = n())
  return(total_contracts$total)
}

# Function to track performance over time for suppliers

get_supplier_performance <- function(supplier_name) {
  data <- fetch_data()
  performance <- data %>%
    filter(grepl(supplier_name, item_awardedSupplier, ignore.case = TRUE)) %>%
    group_by(contract_date = as.Date(item_awardedDate)) %>%
    summarise(total_value = sum(item_awardedValue, na.rm = TRUE),
              contract_count = n())
  return(performance)
}


# Function to get all suppliers matching a pattern
get_matching_suppliers <- function(pattern) {
  my_data <- fetch_data()
  matched_suppliers <- my_data %>%
    filter(grepl(pattern, item_awardedSupplier, ignore.case = TRUE)) %>%
    select(item_awardedSupplier) %>%
    distinct()
  return(matched_suppliers$item_awardedSupplier)
}

# Function to get average contract value for a supplier
get_average_contract_value <- function(supplier_name) {
  my_data <- fetch_data()
  avg_contract_value <- my_data %>%
    filter(grepl(supplier_name, item_awardedSupplier, ignore.case = TRUE)) %>%
    summarise(average_value = mean(item_awardedValue, na.rm = TRUE))
  return(avg_contract_value$average_value)
}

get_contract_trends <- function() {
  data <- fetch_data()
  trends <- data %>%
    mutate(year = as.numeric(format(as.Date(item_awardedDate), "%Y"))) %>%
    group_by(year) %>%
    summarise(total_value = sum(item_awardedValue, na.rm = TRUE),
              contract_count = n())
  return(trends)
}



# Function to refresh the cached data
refresh_data <- function() {
  fetch_data(force_refresh = TRUE)
}