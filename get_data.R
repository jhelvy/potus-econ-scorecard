###############################################
# Data Gathering for Presidential Market Performance Comparison App
# This script fetches and processes market data for later use in the app
###############################################

# Load required libraries for data gathering and processing
library(dplyr)
library(lubridate)
library(quantmod)
library(tidyr)

#-------------------------------------------
# Data Setup and Helper Functions
#-------------------------------------------

# Define presidential data
presidents_data <- data.frame(
  president = c(
    "Eisenhower (1957)", "Kennedy (1961)", "Johnson (1963)", "Nixon (1969)", 
    "Nixon (1973)", "Ford (1974)", "Carter (1977)", "Reagan (1981)",
    "Reagan (1985)", "Bush Sr. (1989)", "Clinton (1993)", "Clinton (1997)",
    "Bush Jr. (2001)", "Bush Jr. (2005)", "Obama (2009)", "Obama (2013)",
    "Trump (2017)", "Biden (2021)", "Trump (2025)"
  ),
  inauguration_date = as.Date(c(
    "1957-01-20", "1961-01-20", "1963-11-22", "1969-01-20",
    "1973-01-20", "1974-08-09", "1977-01-20", "1981-01-20",
    "1985-01-20", "1989-01-20", "1993-01-20", "1997-01-20",
    "2001-01-20", "2005-01-20", "2009-01-20", "2013-01-20",
    "2017-01-20", "2021-01-20", "2025-01-20"
  )),
  election_date = as.Date(c(
    "1956-11-06", "1960-11-08", "1960-11-08", "1968-11-05",
    "1972-11-07", "1972-11-07", "1976-11-02", "1980-11-04",
    "1984-11-06", "1988-11-08", "1992-11-03", "1996-11-05",
    "2000-11-07", "2004-11-02", "2008-11-04", "2012-11-06",
    "2016-11-08", "2020-11-03", "2024-11-05"
  )),
  party = c(
    "Republican", "Democratic", "Democratic", "Republican",
    "Republican", "Republican", "Democratic", "Republican",
    "Republican", "Republican", "Democratic", "Democratic",
    "Republican", "Republican", "Democratic", "Democratic",
    "Republican", "Democratic", "Republican"
  ),
  stringsAsFactors = FALSE
)

# Define market indices to fetch
market_indices <- data.frame(
  symbol = c("^GSPC", "^DJI", "^IXIC"),
  name = c("S&P 500", "Dow Jones", "NASDAQ"),
  id = c("sp500", "djia", "nasdaq"),
  stringsAsFactors = FALSE
)

# Function to fetch market data for all indices
fetch_market_data <- function() {
  # Calculate the earliest date needed
  earliest_date <- min(presidents_data$election_date) - days(30)
  
  # Create a list to store market data
  all_data <- list()
  
  # Loop through each market index
  for (i in 1:nrow(market_indices)) {
    tryCatch({
      # Get symbol and ID
      symbol <- market_indices$symbol[i]
      index_id <- market_indices$id[i]
      index_name <- market_indices$name[i]
      
      # Fetch data
      message(paste("Fetching data for", index_name))
      getSymbols(symbol, from = earliest_date, to = Sys.Date() + days(1), src = "yahoo")
      
      # Get the object based on the symbol
      obj_name <- gsub("\\^", "", symbol)
      idx_obj <- get(obj_name)
      
      # Create data frame
      idx_data <- data.frame(
        date = index(idx_obj),
        value = as.numeric(idx_obj[, paste0(obj_name, ".Adjusted")]),
        index_id = index_id,
        index_name = index_name,
        stringsAsFactors = FALSE
      )
      
      # Store in list
      all_data[[index_id]] <- idx_data
      
    }, error = function(e) {
      warning(paste("Error fetching data for", index_name, ":", e$message))
    })
  }
  
  return(all_data)
}

#-------------------------------------------
# Main Data Collection Process
#-------------------------------------------

# Fetch all market data
market_data <- fetch_market_data()

# Create a combined dataset for all indices and dates
combined_data <- bind_rows(market_data)

# Add metadata about when the data was retrieved
combined_data$data_retrieved <- Sys.Date()

# Save the data to CSV
write.csv(combined_data, "market_data.csv", row.names = FALSE)

# Save the presidents data to CSV as well
write.csv(presidents_data, "presidents_data.csv", row.names = FALSE)

message("Data collection complete. Files saved as market_data.csv and presidents_data.csv")
