###############################################
# Presidential Market Performance Comparison App
# A Shiny app to compare stock market performance 
# under different US presidential administrations
###############################################

# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(lubridate)
library(quantmod)
library(scales)
library(ggrepel)
library(DT)
library(plotly)
library(tidyr)
library(htmlwidgets)  # Add this library for onRender

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

# Color scheme for political parties
party_colors <- c("Democratic" = "blue", "Republican" = "red")

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

#-------------------------------------------
# UI Definition
#-------------------------------------------

ui <- dashboardPage(
    dashboardHeader(title = "Presidential Market Performance"),
    
    dashboardSidebar(
        width = 300,
        
        # Index selection
        radioButtons("selected_index", "Select Market Index:",
                     choices = list(
                         "S&P 500" = "sp500", 
                         "Dow Jones" = "djia",
                         "NASDAQ" = "nasdaq",
                         "All Indices" = "all"
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
        
        # Party filter - moved to top
        checkboxGroupInput("party_filter", "Filter by Party:",
                           choices = c("Democratic", "Republican"),
                           selected = c("Democratic", "Republican")),
        
        # President selection - Select/Deselect buttons at the top
        tags$div(
            tags$label("Select Presidents:"),
            tags$div(
                style = "display: flex; justify-content: space-between; margin-bottom: 10px;",
                actionButton("select_all", "Select All", width = "48%", style = "margin-right: 4%;"),
                actionButton("deselect_all", "Deselect All", width = "48%")
            )
        ),
        
        # President checkboxes
        checkboxGroupInput("selected_presidents", NULL, # No label since we put it above
                           choices = presidents_data$president,
                           selected = c("Obama (2013)", "Trump (2017)", "Biden (2021)", "Trump (2025)"))
    ),
    
    dashboardBody(
        # Initial loading message
        tags$div(
            id = "loading-content",
            tags$h2("Loading market data..."),
            tags$p("This may take a moment. The app is fetching historical data for all market indices."),
            style = "display: none;" # Hidden by default, shown via JS
        ),
        
        # Main content (hidden during loading)
        tags$div(
            id = "app-content",
            fluidRow(
                box(
                    width = 12,
                    title = "Market Performance Comparison",
                    status = "primary",
                    solidHeader = TRUE,
                    plotlyOutput("market_plot", height = "600px"),
                    footer = tags$div(
                        style = "display: flex; justify-content: flex-end; padding: 10px;",
                        downloadButton("download_plot", "Download Plot", icon = icon("download")),
                        tags$div(style = "width: 10px;"), # Spacer
                        downloadButton("download_data", "Download Data", icon = icon("table"))
                    )
                )
            ),
            fluidRow(
                box(
                    width = 12,
                    title = "Performance Summary",
                    status = "info",
                    solidHeader = TRUE,
                    DTOutput("summary_table")
                )
            )
        ),
        
        # Custom JavaScript for loading screen
        tags$script(HTML("
      $(document).ready(function() {
        $('#loading-content').show();
        $('#app-content').hide();
        
        var checkExist = setInterval(function() {
          if ($('#market_plot').length) {
            $('#loading-content').hide();
            $('#app-content').show();
            clearInterval(checkExist);
          }
        }, 100);
      });
    "))
    )
)

#-------------------------------------------
# Server Logic
#-------------------------------------------

server <- function(input, output, session) {
    
    # Reactive value to store all market data
    market_data <- reactiveVal(NULL)
    
    # Initialize data on startup
    observe({
        # Fetch market data
        data_list <- tryCatch({
            fetch_market_data()
        }, error = function(e) {
            showNotification(paste("Error fetching data:", e$message), type = "error")
            return(NULL)
        })
        
        # Store the data
        market_data(data_list)
        
        # Show notification
        if (!is.null(data_list) && length(data_list) > 0) {
            showNotification("Market data loaded successfully!", type = "message")
        }
    })
    
    # Process data based on user selections
    processed_data <- reactive({
        # Make sure data is loaded
        req(market_data())
        
        # Get user selections
        selected_index <- input$selected_index
        reference_type <- input$reference_date
        selected_presidents <- input$selected_presidents
        party_filter <- input$party_filter
        days_to_show <- input$time_period
        
        # Process data
        if (selected_index == "all") {
            # For "all" indices view, process each index separately and combine
            all_data <- data.frame()
            
            for (idx in c("sp500", "djia", "nasdaq")) {
                # Get index data
                index_data <- market_data()[[idx]]
                
                # Skip if no data
                if (is.null(index_data) || nrow(index_data) == 0) {
                    next
                }
                
                # Filter presidents by party and selection
                filtered_presidents <- presidents_data %>%
                    filter(party %in% party_filter, president %in% selected_presidents)
                
                # If no presidents selected, skip
                if (nrow(filtered_presidents) == 0) {
                    next
                }
                
                # Process data for each selected president for this index
                for (i in 1:nrow(filtered_presidents)) {
                    pres_row <- filtered_presidents[i, ]
                    
                    # Determine reference date based on user selection
                    ref_date <- if (reference_type == "inauguration") {
                        pres_row$inauguration_date
                    } else {
                        pres_row$election_date
                    }
                    
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
                    
                    # Add to result
                    all_data <- rbind(all_data, pres_data)
                }
            }
            
            return(all_data)
        } else {
            # For single index view
            return(process_market_data(
                market_data(), 
                selected_index, 
                selected_presidents, 
                reference_type, 
                party_filter, 
                days_to_show
            ))
        }
    })
    
    # Render the market plot
    output$market_plot <- renderPlotly({
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
        
        if (input$selected_index == "all") {
            # Create a plot for all indices
            
            # Get the end points for each president and index
            end_points <- plot_data %>% 
                group_by(president, party, index_name) %>% 
                filter(day == max(day)) %>%
                mutate(label = paste0(president, " (", round(percent_change, 1), "%)"))
            
            # Create base ggplot (without labels - we'll add them in plotly)
            p <- ggplot(plot_data, aes(x = day, y = percent_change, 
                                       color = party, 
                                       group = interaction(president, index_name))) +
                geom_line(size = 1, alpha = 0.5) +  # Add transparency with alpha
                geom_point(data = end_points, size = 2) +
                # Add facets by index - HORIZONTAL layout (3 columns)
                facet_wrap(~ index_name, ncol = 3) +
                geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
                scale_color_manual(values = party_colors) +
                labs(
                    title = paste("Market Indices Performance Since", ref_type),
                    subtitle = paste0("Showing first ", input$time_period, " days (0% = value on reference date)"),
                    x = paste("Days Since", ref_type),
                    y = "Percent Change (%)"
                ) +
                theme_minimal() +
                theme(
                    plot.title = element_text(face = "bold"),
                    panel.grid.minor = element_blank(),
                    legend.position = "bottom",
                    strip.text = element_text(face = "bold", size = 11),
                    strip.background = element_rect(fill = "lightgray", color = NA)
                ) +
                # Add extra space on right for labels
                coord_cartesian(xlim = c(0, max(plot_data$day) * 1.3))
            
            # Convert to plotly with customized tooltip
            p_ly <- ggplotly(p, tooltip = c("y", "x")) %>%
                layout(
                    legend = list(orientation = "h", y = -0.2),
                    # Add more space on the right margin for labels
                    margin = list(r = 100)
                )
            
            # Customize tooltips to show president names
            for (i in seq_along(p_ly$x$data)) {
                if (i <= nrow(end_points)) {
                    pres_data <- end_points[i, ]
                    if (!is.na(pres_data$president)) {
                        p_ly$x$data[[i]]$name <- pres_data$president
                        p_ly$x$data[[i]]$hoverinfo <- "text+y+name"
                        p_ly$x$data[[i]]$text <- paste("Day:", round(p_ly$x$data[[i]]$x, 1))
                    }
                }
            }
            
            # Add annotations manually for each end point
            for (i in 1:nrow(end_points)) {
                point <- end_points[i, ]
                # Get facet index (1-based)
                facet_idx <- match(point$index_name, unique(end_points$index_name))
                
                # Add annotation
                p_ly <- p_ly %>% 
                    add_annotations(
                        x = point$day,
                        y = point$percent_change,
                        text = point$president,
                        xref = paste0("x", facet_idx),
                        yref = paste0("y", facet_idx),
                        showarrow = TRUE,
                        arrowhead = 0,
                        arrowsize = 0.7,
                        arrowwidth = 1,
                        arrowcolor = "gray70",
                        xanchor = "left",
                        yanchor = "middle",
                        textangle = 0,
                        font = list(color = "black", size = 10),
                        ax = 40,
                        ay = 0
                    )
            }
            
            return(p_ly)
            
        } else {
            # Plot for a single index
            index_name <- unique(plot_data$index_name)[1]
            
            # Get end points for each president
            end_points <- plot_data %>% 
                group_by(president, party) %>% 
                filter(day == max(day)) %>%
                mutate(label = paste0(president, " (", round(percent_change, 1), "%)"))
            
            # Get a set of distinct colors for each president (not by party)
            all_presidents <- unique(plot_data$president)
            num_pres <- length(all_presidents)
            pres_colors <- setNames(
                colorRampPalette(c("blue", "red", "green", "orange", "purple"))(num_pres),
                all_presidents
            )
            
            # Create base ggplot (without labels - we'll add them in plotly)
            p <- ggplot(plot_data, aes(x = day, y = percent_change, color = party, group = president)) +
                geom_line(size = 1, alpha = 0.5) +  # Add transparency with alpha
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
            
            # Add hover effects for lines - make them more visible on hover
            # This requires a callback to handle the hover event
            p_ly <- p_ly %>% onRender("
        function(el, x) {
          el.on('plotly_hover', function(d) {
            var curveNumber = d.points[0].curveNumber;
            var traceOpacity = 1;
            var otherOpacity = 0.1;
            
            // Get all plotly traces (lines)
            var traces = document.querySelectorAll('.scatterlayer .trace');
            
            // Set opacity for all traces
            for (var i = 0; i < traces.length; i++) {
              if (i === curveNumber) {
                traces[i].style.opacity = traceOpacity;
              } else {
                traces[i].style.opacity = otherOpacity;
              }
            }
          });
          
          // Reset opacities on mouseout
          el.on('plotly_unhover', function(d) {
            var traces = document.querySelectorAll('.scatterlayer .trace');
            for (var i = 0; i < traces.length; i++) {
              traces[i].style.opacity = 0.5; // Reset to default alpha
            }
          });
        }
      ")
            
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
    
    # Generate summary table
    output$summary_table <- renderDT({
        # Get processed data
        data <- processed_data()
        
        # Make sure there's data
        req(nrow(data) > 0)
        
        # Generate simplified summary statistics
        summary_data <- data %>%
            group_by(president, party, index_name) %>%
            summarize(
                Percent_Change = round(last(percent_change), 2),
                Days_Tracked = max(day),
                .groups = "drop"
            ) %>%
            arrange(index_name, desc(Percent_Change))
        
        # Return formatted table
        datatable(summary_data, 
                  options = list(
                      pageLength = 15, 
                      autoWidth = TRUE,
                      dom = 'ft',  # Simplified display with just filter and table
                      columnDefs = list(list(className = 'dt-center', targets = "_all"))
                  ),
                  rownames = FALSE,
                  caption = paste0("Performance Summary (reference: ", 
                                   if(input$reference_date == "inauguration") "Inauguration Day" else "Day Before Election", ")")) %>%
            formatStyle('Percent_Change',
                        color = styleInterval(0, c('red', 'forestgreen')),
                        fontWeight = 'bold') %>%
            formatStyle('party',
                        backgroundColor = styleEqual(
                            c("Democratic", "Republican"), 
                            c("lightskyblue", "lightpink")
                        ))
    })
    
    # Handle select all button
    observeEvent(input$select_all, {
        updateCheckboxGroupInput(session, "selected_presidents",
                                 choices = presidents_data$president,
                                 selected = presidents_data$president)
    })
    
    # Handle deselect all button
    observeEvent(input$deselect_all, {
        updateCheckboxGroupInput(session, "selected_presidents",
                                 choices = presidents_data$president,
                                 selected = character(0))
    })
    
    # Download handlers for the plot
    output$download_plot <- downloadHandler(
        filename = function() {
            index_name <- switch(input$selected_index,
                                 "sp500" = "SP500",
                                 "djia" = "DowJones",
                                 "nasdaq" = "NASDAQ",
                                 "all" = "AllIndices")
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
            
            # Plot settings
            if (input$selected_index == "all") {
                # Plot for all indices
                
                # Create end labels data
                end_labels <- plot_data %>% 
                    group_by(president, index_name) %>% 
                    filter(day == max(day)) %>%
                    mutate(label = paste0(president, " (", round(percent_change, 1), "%)"))
                
                # Create ggplot with direct labels for static output
                p <- ggplot(plot_data, aes(x = day, y = percent_change, 
                                           color = party, 
                                           group = interaction(president, index_name))) +
                    geom_line(size = 1, alpha = 0.5) +  # Add transparency with alpha
                    geom_point(data = end_labels, size = 2) +
                    # Add facets by index - horizontal layout
                    facet_wrap(~ index_name, ncol = 3) +
                    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
                    # Add direct labels for static export
                    geom_text_repel(
                        data = end_labels,
                        aes(label = label),
                        direction = "y",
                        hjust = 0,
                        vjust = 0,
                        nudge_x = 10,
                        force = 3,
                        segment.size = 0.2,
                        box.padding = 0.5,
                        show.legend = FALSE
                    ) +
                    scale_color_manual(values = party_colors) +
                    labs(
                        title = paste("Market Indices Performance Since", ref_type),
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
                        strip.text = element_text(face = "bold", size = 12),
                        strip.background = element_rect(fill = "lightgray", color = NA)
                    ) +
                    coord_cartesian(xlim = c(0, max(plot_data$day) * 1.35))
                
                # Save with appropriate dimensions for faceted plot
                ggsave(file, plot = p, width = 12, height = 15, dpi = 300)
                
            } else {
                # Plot for a single index
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
                                 "all" = "AllIndices")
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