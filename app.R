###############################################
# POTUS Economic Scorecard
# A Shiny app to compare economic performance under different US presidents
###############################################

# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(ggrepel)
library(plotly)
library(tidyr)
# Use bslib for nicer UI
library(bslib)

#-------------------------------------------
# Data Loading and Processing Functions
#-------------------------------------------

# Function to load prepared data
load_data <- function() {
  # URL for the data files in GitHub
  market_data_url <- "https://raw.githubusercontent.com/jhelvy/potus-econ-scorecard/refs/heads/main/market_data.csv"
  presidents_data_url <- "https://raw.githubusercontent.com/jhelvy/potus-econ-scorecard/refs/heads/main/presidents_data.csv"
  
  # Load data
  tryCatch({
    market_data_raw <- read.csv(market_data_url)
    presidents_data <- read.csv(presidents_data_url, stringsAsFactors = FALSE)
    
    # Convert date column to Date type
    market_data_raw$date <- as.Date(market_data_raw$date)
    presidents_data$inauguration_date <- as.Date(presidents_data$inauguration_date)
    presidents_data$election_date <- as.Date(presidents_data$election_date)
    
    # Organize market data by index
    market_data <- split(market_data_raw, market_data_raw$index_id)
    
    return(list(
      market_data = market_data,
      presidents_data = presidents_data
    ))
  }, error = function(e) {
    # Return empty data frames if loading fails
    warning("Error loading data: ", e$message)
    return(list(
      market_data = list(),
      presidents_data = data.frame()
    ))
  })
}

# Function to process market data for selected presidents and reference date
process_market_data <- function(market_data, 
                                presidents_data,
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
  index_data_list <- list(market_data[[selected_index]])
  names(index_data_list) <- selected_index
  
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
      
      # Find closest trading day to reference date
      closest_ref_date <- index_data %>%
        filter(date >= ref_date) %>%
        arrange(date) %>%
        slice(1) %>%
        pull(date)
      
      if (length(closest_ref_date) == 0) {
        # Skip if no suitable reference date found
        next
      }
      
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

# Function to process economic indicator data that shouldn't use percent change
process_econ_data <- function(market_data, 
                              presidents_data,
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
  index_data_list <- list(market_data[[selected_index]])
  names(index_data_list) <- selected_index
  
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
      
      # Find closest day to reference date
      closest_ref_date <- index_data %>%
        filter(date >= ref_date) %>%
        arrange(date) %>%
        slice(1) %>%
        pull(date)
      
      if (length(closest_ref_date) == 0) {
        # Skip if no suitable reference date found
        next
      }
      
      # For economic indicators, we show absolute values (not percent change)
      pres_data <- index_data %>%
        filter(date >= closest_ref_date) %>%
        mutate(
          president = pres_row$president,
          party = pres_row$party,
          day = as.numeric(difftime(date, closest_ref_date, units = "days"))
        ) %>%
        filter(day <= days_to_show)
      
      result_data <- rbind(result_data, pres_data)
    }
  }
  
  return(result_data)
}

#-------------------------------------------
# UI Definition
#-------------------------------------------

ui <- page_sidebar(
  title = "POTUS Economic Scorecard",
  theme = bs_theme(bootswatch = "flatly"),
  
  sidebar = sidebar(
    # Index selection
    radioButtons("selected_index", "Select Economic Indicator:",
                 choices = list(
                   "S&P 500" = "sp500", 
                   "Dow Jones" = "djia",
                   "NASDAQ" = "nasdaq",
                   "Unemployment Rate" = "unemployment",
                   "Inflation Rate" = "inflation"
                 ),
                 selected = "sp500"),
    
    # Baseline selection
    radioButtons("reference_date", "Reference Date:",
                 choices = list(
                   "Inauguration Day" = "inauguration", 
                   "Day Before Election" = "election"
                 ),
                 selected = "inauguration"),
    
    # Time period selection
    sliderInput("time_period", "Days to Display:",
                min = 10, max = 365*4, value = 100, step = 10),
    
    # Party filter
    checkboxGroupInput("party_filter", "Filter by Party:",
                       choices = c("Democratic", "Republican"),
                       selected = c("Democratic", "Republican")),
    
    # President selection buttons
    tags$div(
      style = "display: flex; justify-content: space-between; margin-bottom: 10px;",
      actionButton("select_all", "Select All", class = "btn-sm"),
      actionButton("deselect_all", "Deselect All", class = "btn-sm")
    ),
    
    # President checkboxes (dynamic)
    uiOutput("president_selection")
  ),
  
  # Main content
  card(
    full_screen = TRUE,
    card_header("Economic Performance Comparison"),
    plotlyOutput("economic_plot", height = "600px"),
    card_footer(
      class = "d-flex justify-content-end",
      downloadButton("download_plot", "Download Plot", class = "btn-sm me-2"),
      downloadButton("download_data", "Download Data", class = "btn-sm")
    )
  )
)

#-------------------------------------------
# Server Logic
#-------------------------------------------

server <- function(input, output, session) {
  
  # Reactive values to store loaded data
  loaded_data <- reactiveVal(NULL)
  
  # Show loading message
  showModal(modalDialog(
    title = "Loading Data",
    "Loading economic data. This may take a moment...",
    footer = NULL,
    easyClose = FALSE
  ))
  
  # Load data on startup
  observe({
    # Get data
    data_list <- load_data()
    
    # Store the data
    loaded_data(data_list)
    
    # Remove loading message
    removeModal()
  })
  
  # Dynamic UI for president selection
  output$president_selection <- renderUI({
    data <- loaded_data()
    
    if (is.null(data) || nrow(data$presidents_data) == 0) {
      return(NULL)
    }
    
    # Get presidents from data
    presidents <- data$presidents_data$president
    
    # Return checkbox group input
    checkboxGroupInput("selected_presidents", 
                       "Select Presidents:", # Now with a label
                       choices = presidents,
                       selected = tail(presidents, 4)) # Select the last 4 presidents by default
  })
  
  # Process data based on user selections
  processed_data <- reactive({
    # Get data
    data <- loaded_data()
    
    # Make sure data is loaded
    req(data, data$market_data, input$selected_presidents)
    
    # Get user selections
    selected_index <- input$selected_index
    reference_type <- input$reference_date
    selected_presidents <- input$selected_presidents
    party_filter <- input$party_filter
    days_to_show <- input$time_period
    
    # Choose the appropriate processing function based on the selected index
    if (selected_index %in% c("unemployment", "inflation")) {
      # For economic indicators, use absolute values
      return(process_econ_data(
        data$market_data,
        data$presidents_data,
        selected_index,
        selected_presidents,
        reference_type,
        party_filter,
        days_to_show
      ))
    } else {
      # For market indices, use percent change from reference date
      return(process_market_data(
        data$market_data,
        data$presidents_data,
        selected_index,
        selected_presidents,
        reference_type,
        party_filter,
        days_to_show
      ))
    }
  })
  
  # Render the plot
  output$economic_plot <- renderPlotly({
    # Get processed data
    plot_data <- processed_data()
    
    # Make sure there's data to plot
    req(nrow(plot_data) > 0)
    
    # Reference type for title
    ref_type <- if (input$reference_date == "inauguration") {
      "Inauguration Day"
    } else {
      "Day Before Election"
    }
    
    # Define party colors
    party_colors <- c("Democratic" = "blue", "Republican" = "red")
    
    if (input$selected_index %in% c("unemployment", "inflation")) {
      # Plot for economic indicators (absolute values)
      index_name <- unique(plot_data$index_name)[1]
      
      # Get end points for each president
      end_points <- plot_data %>% 
        group_by(president, party) %>% 
        filter(day == max(day)) %>%
        mutate(label = paste0(president, " (", round(value, 1), ")"))
      
      # Create base ggplot
      p <- ggplot(plot_data, aes(x = day, y = value, color = party, group = president)) +
        geom_line(size = 1, alpha = 0.8) +
        geom_point(data = end_points, size = 3) +
        scale_color_manual(values = party_colors) +
        labs(
          title = paste(index_name, "Since", ref_type),
          subtitle = paste0("Showing first ", input$time_period, " days"),
          x = paste("Days Since", ref_type),
          y = index_name
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank(),
          legend.position = "bottom"
        ) +
        # Add extra space on right for labels
        coord_cartesian(xlim = c(0, max(plot_data$day) * 1.3))
      
      # For unemployment and inflation, we might want to set y-limits
      if (input$selected_index == "unemployment") {
        p <- p + ylim(0, max(plot_data$value) * 1.1)  # Ensure we start at 0 for unemployment
      }
      
      # Convert to plotly
      p_ly <- ggplotly(p, tooltip = c("y", "x")) %>%
        layout(
          legend = list(orientation = "h", y = -0.2),
          # Add more space on the right margin for labels
          margin = list(r = 100)
        )
      
      # Add annotations for each end point
      for (i in 1:nrow(end_points)) {
        point <- end_points[i, ]
        
        p_ly <- p_ly %>% 
          add_annotations(
            x = point$day,
            y = point$value,
            text = point$label,
            showarrow = TRUE,
            arrowhead = 0,
            arrowsize = 0.7,
            arrowwidth = 1,
            arrowcolor = "gray70",
            xanchor = "left",
            yanchor = "middle",
            textangle = 0,
            font = list(color = "black", size = 11),
            ax = 50,
            ay = 0
          )
      }
      
      return(p_ly)
      
    } else {
      # Plot for a single market index (percent change)
      index_name <- unique(plot_data$index_name)[1]
      
      # Get end points for each president
      end_points <- plot_data %>% 
        group_by(president, party) %>% 
        filter(day == max(day)) %>%
        mutate(label = paste0(president, " (", round(percent_change, 1), "%)"))
      
      # Create base ggplot
      p <- ggplot(plot_data, aes(x = day, y = percent_change, color = party, group = president)) +
        geom_line(size = 1, alpha = 0.8) +
        geom_point(data = end_points, size = 3) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        scale_color_manual(values = party_colors) +
        labs(
          title = paste(index_name, "Performance Since", ref_type),
          subtitle = paste0("Showing first ", input$time_period, " days (0% = value on reference date)"),
          x = paste("Days Since", ref_type),
          y = "Percent Change (%)"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank(),
          legend.position = "bottom"
        ) +
        # Add extra space on right for labels
        coord_cartesian(xlim = c(0, max(plot_data$day) * 1.3))
      
      # Convert to plotly
      p_ly <- ggplotly(p, tooltip = c("text", "y", "x")) %>%
        layout(
          legend = list(orientation = "h", y = -0.2),
          # Add more space on the right margin for labels
          margin = list(r = 100)
        )
      
      # Add zero line
      p_ly <- p_ly %>% 
        add_segments(x = 0, xend = max(plot_data$day) * 1.3, 
                     y = 0, yend = 0,
                     line = list(dash = "dash", color = "gray", width = 1),
                     showlegend = FALSE, 
                     hoverinfo = "none")
      
      # Add annotations manually for each end point
      for (i in 1:nrow(end_points)) {
        point <- end_points[i, ]
        
        p_ly <- p_ly %>% 
          add_annotations(
            x = point$day,
            y = point$percent_change,
            text = point$label,  # Include percentage in label
            showarrow = TRUE,
            arrowhead = 0,
            arrowsize = 0.7,
            arrowwidth = 1,
            arrowcolor = "gray70",
            xanchor = "left",
            yanchor = "middle",
            textangle = 0,
            font = list(color = "black", size = 11),
            ax = 50,  # Increased offset for better visibility
            ay = 0
          )
      }
      
      return(p_ly)
    }
  })
  
  # Handle select all button
  observeEvent(input$select_all, {
    data <- loaded_data()
    req(data, data$presidents_data)
    
    updateCheckboxGroupInput(session, "selected_presidents",
                             choices = data$presidents_data$president,
                             selected = data$presidents_data$president)
  })
  
  # Handle deselect all button
  observeEvent(input$deselect_all, {
    data <- loaded_data()
    req(data, data$presidents_data)
    
    updateCheckboxGroupInput(session, "selected_presidents",
                             choices = data$presidents_data$president,
                             selected = character(0))
  })
  
  # Download handlers for the plot
  output$download_plot <- downloadHandler(
    filename = function() {
      index_name <- switch(input$selected_index,
                           "sp500" = "SP500",
                           "djia" = "DowJones",
                           "nasdaq" = "NASDAQ",
                           "unemployment" = "Unemployment",
                           "inflation" = "Inflation")
      paste(index_name, "_presidential_comparison_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Get processed data
      plot_data <- processed_data()
      
      # Make sure there's data
      req(nrow(plot_data) > 0)
      
      # Reference type for title
      ref_type <- if (input$reference_date == "inauguration") {
        "Inauguration Day"
      } else {
        "Day Before Election"
      }
      
      # Define party colors
      party_colors <- c("Democratic" = "blue", "Republican" = "red")
      
      if (input$selected_index %in% c("unemployment", "inflation")) {
        # Plot for economic indicators
        index_name <- unique(plot_data$index_name)[1]
        
        # Create end labels data
        end_labels <- plot_data %>% 
          group_by(president) %>% 
          filter(day == max(day)) %>%
          mutate(label = paste0(president, " (", round(value, 1), ")"))
        
        # Create ggplot
        p <- ggplot(plot_data, aes(x = day, y = value, color = party, group = president)) +
          geom_line(size = 1) +
          geom_point(data = end_labels, size = 3) +
          # Add end labels
          geom_text_repel(
            data = end_labels,
            aes(label = label),
            direction = "y",
            hjust = 0,
            vjust = 0,
            nudge_x = 5,
            force = 3,
            segment.size = 0.2,
            box.padding = 0.5,
            show.legend = FALSE
          ) +
          scale_color_manual(values = party_colors) +
          labs(
            title = paste(index_name, "Since", ref_type),
            subtitle = paste0("Showing first ", input$time_period, " days"),
            x = paste("Days Since", ref_type),
            y = index_name,
            caption = paste("Generated on", Sys.Date())
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(face = "bold", size = 16),
            plot.subtitle = element_text(size = 12),
            panel.grid.minor = element_blank(),
            legend.position = "bottom",
            legend.title = element_text(face = "bold"),
            axis.title = element_text(face = "bold")
          ) +
          coord_cartesian(xlim = c(0, max(plot_data$day) * 1.15))
        
        # Set y-limits for unemployment to start at 0
        if (input$selected_index == "unemployment") {
          p <- p + ylim(0, max(plot_data$value) * 1.1)
        }
        
        # Save with appropriate dimensions
        ggsave(file, plot = p, width = 12, height = 8, dpi = 300)
        
      } else {
        # Plot for a single market index
        index_name <- unique(plot_data$index_name)[1]
        
        # Create end labels data
        end_labels <- plot_data %>% 
          group_by(president) %>% 
          filter(day == max(day)) %>%
          mutate(label = paste0(president, " (", round(percent_change, 1), "%)"))
        
        # Create ggplot
        p <- ggplot(plot_data, aes(x = day, y = percent_change, color = party, group = president)) +
          geom_line(size = 1) +
          geom_point(data = end_labels, size = 3) +
          # Add end labels
          geom_text_repel(
            data = end_labels,
            aes(label = label),
            direction = "y",
            hjust = 0,
            vjust = 0,
            nudge_x = 5,
            force = 3,
            segment.size = 0.2,
            box.padding = 0.5,
            show.legend = FALSE
          ) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
          scale_color_manual(values = party_colors) +
          labs(
            title = paste(index_name, "Performance Since", ref_type),
            subtitle = paste0("Showing first ", input$time_period, " days (0% = value on reference date)"),
            x = paste("Days Since", ref_type),
            y = "Percent Change (%)",
            caption = paste("Generated on", Sys.Date())
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(face = "bold", size = 16),
            plot.subtitle = element_text(size = 12),
            panel.grid.minor = element_blank(),
            legend.position = "bottom",
            legend.title = element_text(face = "bold"),
            axis.title = element_text(face = "bold")
          ) +
          coord_cartesian(xlim = c(0, max(plot_data$day) * 1.15))
        
        # Save with appropriate dimensions
        ggsave(file, plot = p, width = 12, height = 8, dpi = 300)
      }
    }
  )
  
  # Download handler for data
  output$download_data <- downloadHandler(
    filename = function() {
      index_name <- switch(input$selected_index,
                           "sp500" = "SP500",
                           "djia" = "DowJones",
                           "nasdaq" = "NASDAQ",
                           "unemployment" = "Unemployment",
                           "inflation" = "Inflation")
      paste(index_name, "_presidential_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Get processed data
      data <- processed_data()
      
      # Add metadata columns to make the CSV more useful
      export_data <- data %>%
        mutate(
          reference_type = if(input$reference_date == "inauguration") "Inauguration Day" else "Day Before Election",
          data_generated = Sys.Date()
        )
      
      write.csv(export_data, file, row.names = FALSE)
    }
  )
}

#-------------------------------------------
# Run the application
#-------------------------------------------

shinyApp(ui = ui, server = server)