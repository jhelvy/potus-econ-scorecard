###############################################
# Data Gathering for Presidential Market Performance Comparison App
# This script fetches and processes market data for later use in the app
###############################################

# Load required libraries for data gathering and processing
library(dplyr)
library(lubridate)
library(tidyquant)
library(tidyr)

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

# Define FRED economic indicators to fetch
fred_indicators <- data.frame(
    symbol = c("UNRATE", "CPIAUCSL", "DGS10", "CSUSHPISA", "GDPC1", "GFDEGDQ188S", "CIVPART", "DTWEXBGS"),
    name = c("Unemployment Rate", "Consumer Price Index", "10-Year Treasury Yield", 
             "Home Price Index", "Real GDP", "Federal Debt to GDP", "Labor Force Participation", "US Dollar Index"),
    id = c("unemployment", "inflation", "treasury10yr", "housing", "gdp", "debt_gdp", "labor_participation", "dxy"),
    stringsAsFactors = FALSE
)

# Function to fetch market data from Yahoo Finance
fetch_market_data <- function() {
    tryCatch({
        # Calculate the earliest date needed
        earliest_date <- min(presidents_data$election_date) - days(30)
        
        # Get the market data using tidyquant
        message("Fetching market data from Yahoo Finance...")
        market_data <- tq_get(
            market_indices$symbol,
            get = "stock.prices",
            from = earliest_date,
            to = Sys.Date()
        )
        
        # Transform the data to match our expected format
        market_data <- market_data %>%
            left_join(
                market_indices %>% select(symbol, id, name),
                by = c("symbol")
            ) %>%
            rename(value = adjusted) %>%
            select(date, value, index_id = id, index_name = name)
        
        return(market_data)
        
    }, error = function(e) {
        warning(paste("Error fetching market data:", e$message))
        return(data.frame()) # Return empty dataframe in case of error
    })
}

# Function to fetch data from FRED
fetch_fred_data <- function() {
    tryCatch({
        # Calculate the earliest date needed
        earliest_date <- min(presidents_data$election_date) - days(30)
        
        # Get all FRED indicators using tidyquant
        message("Fetching economic data from FRED...")
        fred_data <- tq_get(
            fred_indicators$symbol,
            get = "economic.data",
            from = earliest_date,
            to = Sys.Date()
        )
        
        # Transform the data to match our expected format
        fred_data <- fred_data %>%
            left_join(
                fred_indicators %>% select(symbol, id, name),
                by = c("symbol")
            ) %>%
            rename(value = price) %>%
            select(date, value, index_id = id, index_name = name)
        
        # For CPI, convert to year-over-year percent change (inflation rate)
        inflation_data <- fred_data %>%
            filter(index_id == "inflation") %>%
            arrange(date) %>%
            mutate(
                prior_year_value = lag(value, 12),
                value = ((value / prior_year_value) - 1) * 100  # YoY percent change
            ) %>%
            filter(!is.na(value)) %>%  # Remove first year where we don't have YoY comparison
            select(-prior_year_value) %>%  # Remove the helper column
            mutate(index_name = "Inflation Rate (YoY)")
        
        # Replace the original inflation data
        fred_data <- fred_data %>%
            filter(index_id != "inflation") %>%
            bind_rows(inflation_data)
        
        return(fred_data)
        
    }, error = function(e) {
        warning(paste("Error fetching FRED data:", e$message))
        return(data.frame()) # Return empty dataframe in case of error
    })
}

#-------------------------------------------
# Main Data Collection Process
#-------------------------------------------

# Fetch all market data
message("Starting market data collection...")
market_data <- fetch_market_data()
market_date_range <- ""
if (nrow(market_data) > 0) {
    min_date <- format(min(market_data$date), "%Y-%m-%d")
    max_date <- format(max(market_data$date), "%Y-%m-%d")
    market_date_range <- paste("(date range:", min_date, "to", max_date, ")")
}
message(paste("Retrieved", nrow(market_data), "rows of market data", market_date_range))

# Fetch all FRED data
message("Starting FRED data collection...")
fred_data <- fetch_fred_data()
fred_date_range <- ""
if (nrow(fred_data) > 0) {
    min_date <- format(min(fred_data$date), "%Y-%m-%d")
    max_date <- format(max(fred_data$date), "%Y-%m-%d")
    fred_date_range <- paste("(date range:", min_date, "to", max_date, ")")
}
message(paste("Retrieved", nrow(fred_data), "rows of FRED data", fred_date_range))

# Check if we have data from both sources
if (nrow(market_data) == 0 && nrow(fred_data) == 0) {
    stop("Failed to fetch any data from either Yahoo Finance or FRED")
}

# Combine all data
message("Combining all data...")
combined_data <- bind_rows(market_data, fred_data)

# Add metadata about when the data was retrieved
combined_data$data_retrieved <- Sys.Date()

# Calculate overall date range
combined_date_range <- ""
if (nrow(combined_data) > 0) {
    min_date <- format(min(combined_data$date), "%Y-%m-%d")
    max_date <- format(max(combined_data$date), "%Y-%m-%d")
    combined_date_range <- paste("(date range:", min_date, "to", max_date, ")")
}

# Save the data
write.csv(combined_data, "market_data.csv", row.names = FALSE)
message("Data collection complete. Updated market_data.csv file.")
message(paste("Total rows in final dataset:", nrow(combined_data), combined_date_range))
