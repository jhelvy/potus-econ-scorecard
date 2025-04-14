###############################################
# Data Gathering for Presidential Market Performance Comparison App
# This script fetches and processes market data for later use in the app
###############################################

# Load required libraries for data gathering and processing
library(dplyr)
library(lubridate)
library(quantmod)
library(tidyr)
library(zoo)  # For na.locf function needed for interpolating quarterly data

#-------------------------------------------
# Data Setup and Helper Functions
#-------------------------------------------

# Improved error handling for data loading
tryCatch({
  presidents_data <- read.csv(
    "https://raw.githubusercontent.com/jhelvy/presidential-econ-tracker/refs/heads/main/presidents_data.csv", 
    stringsAsFactors = FALSE
  )
  presidents_data$inauguration_date <- as.Date(presidents_data$inauguration_date)
  presidents_data$election_date <- as.Date(presidents_data$election_date)
}, error = function(e) {
  stop(paste("Failed to load presidents data:", e$message))
})

# Define market indices to fetch from Yahoo Finance
market_indices <- data.frame(
  symbol = c("^GSPC", "^DJI", "^IXIC"),
  name = c("S&P 500", "Dow Jones", "NASDAQ"),
  id = c("sp500", "djia", "nasdaq"),
  stringsAsFactors = FALSE
)

# Define FRED economic indicators to fetch (now includes DXY as DTWEXBGS)
fred_indicators <- data.frame(
  symbol = c("UNRATE", "CPIAUCSL", "DGS10", "CSUSHPISA", "GDPC1", "GFDEGDQ188S", "CIVPART", "DTWEXBGS"),
  name = c("Unemployment Rate", "Consumer Price Index", "10-Year Treasury Yield", 
           "Home Price Index", "Real GDP", "Federal Debt to GDP", "Labor Force Participation", "US Dollar Index"),
  id = c("unemployment", "inflation", "treasury10yr", "housing", "gdp", "debt_gdp", "labor_participation", "dxy"),
  frequency = c("monthly", "monthly", "daily", "monthly", "quarterly", "quarterly", "monthly", "daily"),
  stringsAsFactors = FALSE
)

# Function to fetch market data for all indices with improved error handling and retry logic
fetch_market_data <- function(max_retries = 3, retry_delay = 5) {
  # Calculate the earliest date needed
  earliest_date <- min(presidents_data$election_date) - days(30)
  
  # Create a list to store market data
  all_data <- list()
  
  # Loop through each market index
  for (i in 1:nrow(market_indices)) {
    symbol <- market_indices$symbol[i]
    index_id <- market_indices$id[i]
    index_name <- market_indices$name[i]
    
    # Add retry logic
    for (attempt in 1:max_retries) {
      tryCatch({
        # Get symbol and ID
        message(paste("Fetching data for", index_name, "(Attempt", attempt, "of", max_retries, ")"))
        
        # Fetch data
        getSymbols(symbol, from = earliest_date, to = Sys.Date() + days(1), src = "yahoo")
        
        # Get the object based on the symbol
        obj_name <- gsub("\\^", "", symbol)
        idx_obj <- get(obj_name)
        
        # Create data frame
        idx_data <- data.frame(
          date = index(idx_obj),
          value = as.numeric(idx_obj[, ncol(idx_obj)]),  # Use the last column (Adjusted)
          index_id = index_id,
          index_name = index_name,
          stringsAsFactors = FALSE
        )
        
        # Store in list
        all_data[[index_id]] <- idx_data
        
        # Success, break the retry loop
        break
        
      }, error = function(e) {
        warning(paste("Error fetching data for", index_name, "on attempt", attempt, ":", e$message))
        if (attempt < max_retries) {
          message(paste("Retrying in", retry_delay, "seconds..."))
          Sys.sleep(retry_delay)
        } else {
          warning(paste("Failed to fetch data for", index_name, "after", max_retries, "attempts"))
        }
      })
    }
  }
  
  # Check if we have any data
  if (length(all_data) == 0) {
    stop("Failed to fetch any market data after multiple attempts")
  }
  
  return(all_data)
}

# Function to fetch data from FRED with improved error handling and retry logic
fetch_fred_data <- function(max_retries = 3, retry_delay = 5) {
  # Calculate the earliest date needed
  earliest_date <- min(presidents_data$election_date) - days(30)
  
  # Create a list to store market data
  all_data <- list()
  
  # Loop through each FRED indicator
  for (i in 1:nrow(fred_indicators)) {
    symbol <- fred_indicators$symbol[i]
    index_id <- fred_indicators$id[i]
    index_name <- fred_indicators$name[i]
    frequency <- fred_indicators$frequency[i]
    
    # Add retry logic
    for (attempt in 1:max_retries) {
      tryCatch({
        # Fetch data
        message(paste("Fetching data for", index_name, "from FRED (Attempt", attempt, "of", max_retries, ")"))
        getSymbols(symbol, src = "FRED", from = earliest_date, to = Sys.Date() + days(1))
        
        # Get the object based on the symbol
        idx_obj <- get(symbol)
        
        # Create data frame
        idx_data <- data.frame(
          date = index(idx_obj),
          value = as.numeric(idx_obj[, 1]),
          index_id = index_id,
          index_name = index_name,
          stringsAsFactors = FALSE
        )
        
        # Special handling for specific indicators
        
        # For CPI, convert to year-over-year percent change (inflation rate)
        if (index_id == "inflation") {
          # Calculate year-over-year percent change for CPI
          idx_data <- idx_data %>%
            arrange(date) %>%
            mutate(
              prior_year_value = lag(value, 12),
              value = ((value / prior_year_value) - 1) * 100  # YoY percent change
            ) %>%
            filter(!is.na(value)) %>%  # Remove first year where we don't have YoY comparison
            select(-prior_year_value)  # Remove the helper column
          
          # Update the index name to clarify it's YoY inflation
          idx_data$index_name <- "Inflation Rate (YoY)"
        }
        
        # For quarterly data, interpolate to daily to create smoother charts
        if (frequency == "quarterly") {
          # Order by date
          idx_data <- idx_data %>%
            arrange(date)
          
          # Get date range
          min_date <- min(idx_data$date)
          max_date <- max(idx_data$date)
          
          # Create a complete date sequence at daily level
          date_range <- seq.Date(from = min_date, to = max_date, by = "day")
          
          # Create a skeleton data frame with all dates
          daily_data <- data.frame(
            date = date_range,
            stringsAsFactors = FALSE
          )
          
          # Merge with original data
          merged_data <- daily_data %>%
            left_join(idx_data, by = "date")
          
          # Fill missing values using last observation carried forward
          merged_data <- merged_data %>%
            mutate(
              value = na.locf(value, na.rm = FALSE),
              index_id = first(na.omit(index_id)),
              index_name = first(na.omit(index_name))
            ) %>%
            filter(!is.na(value))
          
          # Replace the original data
          idx_data <- merged_data
        }
        
        # Store in list
        all_data[[index_id]] <- idx_data
        
        # Success, break the retry loop
        break
        
      }, error = function(e) {
        warning(paste("Error fetching data for", index_name, "from FRED on attempt", attempt, ":", e$message))
        if (attempt < max_retries) {
          message(paste("Retrying in", retry_delay, "seconds..."))
          Sys.sleep(retry_delay)
        } else {
          warning(paste("Failed to fetch data for", index_name, "from FRED after", max_retries, "attempts"))
        }
      })
    }
  }
  
  # Check if we have any data
  if (length(all_data) == 0) {
    stop("Failed to fetch any FRED data after multiple attempts")
  }
  
  return(all_data)
}

#-------------------------------------------
# Main Data Collection Process
#-------------------------------------------

# Add overall error handling for the main process
tryCatch({
  # Fetch all market data
  message("Starting market data collection...")
  market_data <- fetch_market_data()
  
  # Fetch all FRED data
  message("Starting FRED data collection...")
  fred_data <- fetch_fred_data()
  
  # Combine all data
  message("Combining all data...")
  all_data <- c(market_data, fred_data)
  combined_data <- bind_rows(all_data)
  
  # Add metadata about when the data was retrieved
  combined_data$data_retrieved <- Sys.Date()
  
  # # Create a backup of existing data if it exists
  # existing_data_file <- "market_data.csv"
  # if (file.exists(existing_data_file)) {
  #   backup_file <- paste0("market_data_backup_", format(Sys.Date(), "%Y%m%d"), ".csv")
  #   file.copy(existing_data_file, backup_file, overwrite = TRUE)
  #   message(paste("Created backup of existing data:", backup_file))
  # }
  
  # Load existing data or create empty data frame if file doesn't exist
  if (file.exists(existing_data_file)) {
    existing_data <- tryCatch({
      read.csv(existing_data_file, stringsAsFactors = FALSE)
    }, error = function(e) {
      warning(paste("Error reading existing data file:", e$message))
      # Return empty data frame with same structure
      return(data.frame(
        date = character(0),
        value = numeric(0),
        index_id = character(0),
        index_name = character(0),
        data_retrieved = character(0),
        stringsAsFactors = FALSE
      ))
    })
  } else {
    # Try loading from GitHub if local file doesn't exist
    existing_data <- tryCatch({
      read.csv(
        "https://raw.githubusercontent.com/jhelvy/presidential-econ-tracker/refs/heads/main/market_data.csv",
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      warning(paste("Could not load data from GitHub:", e$message))
      # Return empty data frame with same structure
      return(data.frame(
        date = character(0),
        value = numeric(0),
        index_id = character(0),
        index_name = character(0),
        data_retrieved = character(0),
        stringsAsFactors = FALSE
      ))
    })
  }
  
  # Convert date columns to Date objects for comparison
  if (nrow(existing_data) > 0) {
    existing_data$date <- as.Date(existing_data$date)
    existing_data$data_retrieved <- as.Date(existing_data$data_retrieved)
  }
  
  # For new indices, we need to get all historical data
  existing_indices <- unique(existing_data$index_id)
  new_indices <- setdiff(unique(combined_data$index_id), existing_indices)
  
  if (length(new_indices) > 0) {
    message(paste("Found new indices:", paste(new_indices, collapse = ", ")))
    
    # For new indices, keep all data
    new_indices_data <- combined_data %>%
      filter(index_id %in% new_indices)
    
    # For existing indices, identify new dates only
    existing_indices_data <- combined_data %>%
      filter(index_id %in% existing_indices)
    
    # Create a unique identifier for each row by combining index_id and date
    existing_data$row_id <- paste(existing_data$index_id, existing_data$date, sep = "_")
    existing_indices_data$row_id <- paste(existing_indices_data$index_id, existing_indices_data$date, sep = "_")
    
    # Find new data points for existing indices
    new_data_existing_indices <- existing_indices_data %>%
      filter(!(row_id %in% existing_data$row_id)) %>%
      select(-row_id)  # Remove the temporary identifier
    
    # Remove the temporary identifier from existing data too
    existing_data <- existing_data %>% select(-row_id)
    
    # Combine new index data with new data for existing indices
    new_data <- bind_rows(new_indices_data, new_data_existing_indices)
    
    if (nrow(new_data) > 0) {
      message(paste("Found", nrow(new_data), "new data points to add"))
      
      # Merge existing and new data
      updated_data <- bind_rows(existing_data, new_data) %>%
        arrange(index_id, date)
      
      # Save the updated data
      write.csv(updated_data, existing_data_file, row.names = FALSE)
      message(paste("Data collection complete. Updated", existing_data_file, "with new data."))
    } else {
      message("No new data found for existing indices.")
      
      # Still need to add the new indices
      updated_data <- bind_rows(existing_data, new_indices_data) %>%
        arrange(index_id, date)
      
      # Save the updated data
      write.csv(updated_data, existing_data_file, row.names = FALSE)
      message(paste("Added new indices to", existing_data_file))
    }
  } else {
    # No new indices, just check for new dates
    # Create a unique identifier for each row by combining index_id and date
    existing_data$row_id <- paste(existing_data$index_id, existing_data$date, sep = "_")
    combined_data$row_id <- paste(combined_data$index_id, combined_data$date, sep = "_")
    
    # Identify new data (dates not in existing data)
    new_data <- combined_data %>%
      filter(!(row_id %in% existing_data$row_id)) %>%
      select(-row_id)  # Remove the temporary identifier
    
    # Remove the temporary identifier from existing data too
    existing_data <- existing_data %>% select(-row_id)
    
    if (nrow(new_data) > 0) {
      message(paste("Found", nrow(new_data), "new data points to add"))
      
      # Merge existing and new data
      updated_data <- bind_rows(existing_data, new_data) %>%
        arrange(index_id, date)
      
      # Save the updated data
      write.csv(updated_data, existing_data_file, row.names = FALSE)
      message(paste("Data collection complete. Updated", existing_data_file, "with new data."))
    } else {
      message(paste("No new data found. Keeping existing", existing_data_file, "file."))
    }
  }
  
}, error = function(e) {
  stop(paste("Error in data collection process:", e$message))
})