# Load required libraries
library(shiny)
library(bslib)      # Modern UI components and theming
library(bsicons)    # Icons for bslib
library(ggplot2)
library(dplyr)
library(lubridate)
library(quantmod)
library(scales)
library(ggrepel)
library(DT)
library(plotly)
library(tidyr)
library(htmlwidgets)

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
    symbol = c("^GSPC", "^DJI", "^IXIC", "CPIAUCSL", "UNRATE"),
    name = c("S&P 500", "Dow Jones", "NASDAQ", "Inflation (CPI)", "Unemployment Rate"),
    id = c("sp500", "djia", "nasdaq", "inflation", "unemployment"),
    source = c("yahoo", "yahoo", "yahoo", "fred", "fred"),
    icon = c("chart-line", "building-fill", "laptop", "currency-dollar", "people"),
    stringsAsFactors = FALSE
)

# Color scheme for political parties - will be overridden by theme
party_colors <- c("Democratic" = "#0000FF", "Republican" = "#FF0000")

# Function to check if data files exist and create them if not
check_data_files <- function() {
    # Check if data directory exists
    if (!dir.exists("data")) {
        dir.create("data")
    }
    
    # Initialize flag to track if we need to download any data
    need_download <- FALSE
    
    # Check all market indices
    for (i in 1:nrow(market_indices)) {
        index_id <- market_indices$id[i]
        index_name <- market_indices$name[i]
        file_path <- paste0("data/", index_id, "_data.rds")
        
        if (!file.exists(file_path)) {
            message(paste("Data file for", index_name, "does not exist"))
            need_download <- TRUE
        } else {
            # Check if data is recent enough (within the last 30 days)
            current_date <- Sys.Date()
            market_data <- readRDS(file_path)
            
            if (nrow(market_data) > 0) {
                last_data_date <- max(market_data$date)
                if (current_date - last_data_date > 30) {
                    message(paste("Data for", index_name, "is over 30 days old"))
                    need_download <- TRUE
                }
            } else {
                message(paste("Data file for", index_name, "is empty"))
                need_download <- TRUE
            }
        }
    }
    
    if (need_download) {
        download_missing_data()
    } else {
        message("All data files are up-to-date")
    }
}

# Function to download missing or outdated data
download_missing_data <- function() {
    message("Downloading missing or outdated market data...")
    
    # Calculate earliest date needed
    earliest_date <- min(presidents_data$election_date) - days(30)
    
    # Loop through each market index
    for (i in 1:nrow(market_indices)) {
        # Get index info
        symbol <- market_indices$symbol[i]
        index_id <- market_indices$id[i]
        index_name <- market_indices$name[i]
        source <- market_indices$source[i]
        file_path <- paste0("data/", index_id, "_data.rds")
        
        # Check if file exists and is up-to-date
        need_download <- FALSE
        if (!file.exists(file_path)) {
            need_download <- TRUE
        } else {
            # Check if data is recent enough
            market_data <- readRDS(file_path)
            if (nrow(market_data) > 0) {
                last_data_date <- max(market_data$date)
                if (Sys.Date() - last_data_date > 30) {
                    need_download <- TRUE
                }
            } else {
                need_download <- TRUE
            }
        }
        
        # Download if needed
        if (need_download) {
            message(paste("Downloading data for", index_name, "from", source))
            
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
                saveRDS(market_data, file_path)
                message(paste("  -", nrow(market_data), "data points saved"))
                
            }, error = function(e) {
                message(paste("Error downloading data for", index_name, ":", e$message))
                
                # Create empty placeholder if file doesn't exist
                if (!file.exists(file_path)) {
                    empty_df <- data.frame(
                        date = as.Date(character()),
                        value = numeric(),
                        index_id = index_id,
                        index_name = index_name,
                        stringsAsFactors = FALSE
                    )
                    saveRDS(empty_df, file_path)
                }
            })
        } else {
            message(paste("Using existing data for", index_name))
        }
    }
}

# Function to fetch market data
fetch_market_data <- function() {
    # Check and update data files if needed
    check_data_files()
    
    # Create a list to store market data
    all_data <- list()
    
    # Load data for each market index
    for (i in 1:nrow(market_indices)) {
        index_id <- market_indices$id[i]
        index_name <- market_indices$name[i]
        file_path <- paste0("data/", index_id, "_data.rds")
        
        message(paste("Loading data for", index_name))
        
        if (file.exists(file_path)) {
            # Load data from file
            market_data <- readRDS(file_path)
            
            # Store in list
            all_data[[index_id]] <- market_data
        } else {
            warning(paste("Data file for", index_name, "not found"))
        }
    }
    
    return(all_data)
}

# Function to process market data for selected presidents and reference date
process_market_data <- function(market_data, 
                                selected_index,
                                selected_presidents,
                                reference_type,
                                party_filter,
                                days_to_show) {
    
    # Check if market data is available
    if (is.null(market_data) || length(market_data) == 0) {
        return(data.frame())
    }
    
    # Get index data
    if (selected_index == "all") {
        # Use all indices
        index_data_list <- market_data
    } else {
        # Use only the selected index
        index_data_list <- list(market_data[[selected_index]])
        names(index_data_list) <- selected_index
    }
    
    # Filter presidents by party and selection
    filtered_presidents <- presidents_data %>%
        filter(party %in% party_filter, president %in% selected_presidents)
    
    # If no presidents selected, return empty dataframe
    if (nrow(filtered_presidents) == 0) {
        return(data.frame())
    }
    
    # Process data for each selected president and index
    result_data <- data.frame()
    
    for (pres_i in 1:nrow(filtered_presidents)) {
        pres_row <- filtered_presidents[pres_i, ]
        
        # Determine reference date based on user selection
        ref_date <- if (reference_type == "inauguration") {
            pres_row$inauguration_date
        } else {
            pres_row$election_date
        }
        
        # Process each index
        for (idx_id in names(index_data_list)) {
            index_data <- index_data_list[[idx_id]]
            
            # Skip if the data is empty
            if (is.null(index_data) || nrow(index_data) == 0) {
                next
            }
            
            # Find closest trading day to reference date
            closest_date_rows <- index_data %>%
                filter(date >= ref_date) %>%
                arrange(date)
            
            # Skip if no suitable date found
            if (nrow(closest_date_rows) == 0) {
                next
            }
            
            closest_ref_date <- closest_date_rows %>%
                slice(1) %>%
                pull(date)
            
            # Get the reference value
            ref_value <- index_data %>%
                filter(date == closest_ref_date) %>%
                pull(value)
            
            # Calculate days from reference and percent change
            pres_data <- index_data %>%
                filter(date >= closest_ref_date) %>%
                mutate(
                    president = pres_row$president,
                    party = pres_row$party,
                    day = as.numeric(difftime(date, closest_ref_date, units = "days")),
                    reference_value = ref_value,
                    percent_change = (value / ref_value - 1) * 100
                ) %>%
                filter(day <= days_to_show)
            
            result_data <- rbind(result_data, pres_data)
        }
    }
    
    return(result_data)
}