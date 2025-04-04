# Script to download and save all market data (Yahoo Finance and FRED)
# Run this script separately to create the data files

library(quantmod)
library(dplyr)
library(lubridate)

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
    dir.create("data")
}

# Define market indices to fetch
market_indices <- data.frame(
    symbol = c("^GSPC", "^DJI", "^IXIC", "CPIAUCSL", "UNRATE"),
    name = c("S&P 500", "Dow Jones", "NASDAQ", "Inflation (CPI)", "Unemployment Rate"),
    id = c("sp500", "djia", "nasdaq", "inflation", "unemployment"),
    source = c("yahoo", "yahoo", "yahoo", "fred", "fred"),
    stringsAsFactors = FALSE
)

# Calculate earliest date needed (going back further to ensure we have enough history)
earliest_date <- as.Date("1950-01-01")

# Download data for each market index
for (i in 1:nrow(market_indices)) {
    symbol <- market_indices$symbol[i]
    index_id <- market_indices$id[i]
    index_name <- market_indices$name[i]
    source <- market_indices$source[i]
    
    message(paste("Downloading", index_name, "data from", source, "..."))
    
    tryCatch({
        if (source == "yahoo") {
            # Download Yahoo Finance data
            getSymbols(symbol, from = earliest_date, src = "yahoo")
            
            # Get the object based on the symbol
            obj_name <- gsub("\\^", "", symbol)
            idx_obj <- get(obj_name)
            
            # Create data frame
            market_data <- data.frame(
                date = index(idx_obj),
                value = as.numeric(idx_obj[, paste0(obj_name, ".Adjusted")]),
                index_id = index_id,
                index_name = index_name,
                stringsAsFactors = FALSE
            )
        } else if (source == "fred") {
            # Download FRED data
            getSymbols(symbol, src = "FRED", from = earliest_date)
            
            # Get the object
            idx_obj <- get(symbol)
            
            # Create data frame
            market_data <- data.frame(
                date = index(idx_obj),
                value = as.numeric(idx_obj[, 1]),
                index_id = index_id,
                index_name = index_name,
                stringsAsFactors = FALSE
            )
        }
        
        # Remove NA values
        market_data <- market_data[!is.na(market_data$value),]
        
        # Save dataset
        file_path <- paste0("data/", index_id, "_data.rds")
        saveRDS(market_data, file_path)
        
        # Print summary
        message(paste("  -", nrow(market_data), "data points downloaded"))
        message(paste("  - Date range:", min(market_data$date), "to", max(market_data$date)))
        
    }, error = function(e) {
        message(paste("Error downloading data for", index_name, ":", e$message))
    })
}

# Print confirmation message
message("\nAll data downloaded and saved to the 'data' folder")
message("To update this data in the future, simply run this script again")