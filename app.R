###############################################
# Presidential Market Performance Comparison App
# A Shiny app to compare stock market performance 
# under different US presidential administrations
###############################################

source('functions.R')

#-------------------------------------------
# UI Definition
#-------------------------------------------

ui <- page_sidebar(
    title = "Presidential Economy Tracker",
    theme = bs_theme(version = 5, preset = "shiny"),
    
    # Add theme selector to header
    header = page_navbar(
        title = "Presidential Economy Tracker",
        bg = "#2c3e50",
        nav_spacer(),
        nav_item(
            input_dark_mode(id = "dark_mode", mode = "light")
        ),
        nav_item(
            downloadButton("download_plot", "Download Plot", class = "btn-sm")
        ),
        nav_item(
            downloadButton("download_data", "Download Data", class = "btn-sm")
        )
    ),
    
    # Sidebar inputs
    sidebar = sidebar(
        # Index selection
        card(
            card_header("Select Indicator"),
            selectInput("selected_index", NULL,
                        choices = list(
                            "S&P 500" = "sp500", 
                            "Dow Jones" = "djia",
                            "NASDAQ" = "nasdaq",
                            "Inflation (CPI)" = "inflation",
                            "Unemployment Rate" = "unemployment",
                            "All Indices" = "all"
                        ),
                        selected = "sp500"),
            class = "mb-3"
        ),
        
        # Baseline selection
        card(
            card_header("Reference Date"),
            radioButtons("reference_date", NULL,
                         choices = list(
                             "Inauguration Day" = "inauguration", 
                             "Day Before Election" = "election"
                         ),
                         selected = "inauguration"),
            class = "mb-3"
        ),
        
        # Time period selection
        card(
            card_header("Days to Display"),
            sliderInput("time_period", NULL,
                        min = 10, max = 365*4, value = 100, step = 10),
            class = "mb-3"
        ),
        
        # Party filter
        card(
            card_header("Party Filter"),
            checkboxGroupInput("party_filter", NULL,
                               choices = c("Democratic", "Republican"),
                               selected = c("Democratic", "Republican")),
            class = "mb-3"
        ),
        
        # President selection
        card(
            card_header("Select Presidents"),
            div(
                class = "d-grid gap-2 mb-3",
                actionButton("select_all", "Select All", class = "btn-sm"),
                actionButton("deselect_all", "Deselect All", class = "btn-sm")
            ),
            checkboxGroupInput("selected_presidents", NULL,
                               choices = presidents_data$president,
                               selected = c("Obama (2013)", "Trump (2017)", "Biden (2021)", "Trump (2025)"))
        )
    ),
    
    # Main content
    layout_columns(
        # Loading spinner overlay
        card(
            id = "loading-panel",
            card_body(
                class = "p-5 text-center",
                span(class = "spinner-border", role = "status"),
                h3("Loading market data...")
            ),
            height = "100%",
            full_screen = TRUE
        ),
        
        # Main data visualization
        card(
            id = "plot-panel",
            card_header("Performance Comparison"),
            plotlyOutput("market_plot", height = "600px")
        ),
        
        # Summary table
        card(
            id = "table-panel",
            card_header("Performance Summary"),
            DTOutput("summary_table")
        ),
        
        # Initial hide/show script
        tags$script(HTML("
            document.addEventListener('DOMContentLoaded', function() {
                document.getElementById('plot-panel').style.display = 'none';
                document.getElementById('table-panel').style.display = 'none';
            });
        "))
    )
)

#-------------------------------------------
# Server Logic
#-------------------------------------------

server <- function(input, output, session) {
    
    # Initialize theme based on dark mode toggle
    observe({
        session$setCurrentTheme(
            if (isTRUE(input$dark_mode)) {
                bs_theme(version = 5, preset = "darkly",
                         "primary" = "#375a7f",
                         "secondary" = "#444444")
            } else {
                bs_theme(version = 5, preset = "flatly",
                         "primary" = "#2c3e50",
                         "secondary" = "#95a5a6")
            }
        )
    })
    
    # Override party colors based on theme
    party_color_values <- reactive({
        if (isTRUE(input$dark_mode)) {
            c("Democratic" = "#3498db", "Republican" = "#e74c3c")
        } else {
            c("Democratic" = "#0000FF", "Republican" = "#FF0000")
        }
    })
    
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
        
        # Show notification and hide loading screen, show content
        if (!is.null(data_list) && length(data_list) > 0) {
            showNotification("Market data loaded successfully!", type = "message")
            
            # Show content, hide loading
            runjs("document.getElementById('loading-panel').style.display = 'none';")
            runjs("document.getElementById('plot-panel').style.display = 'block';")
            runjs("document.getElementById('table-panel').style.display = 'block';")
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
            
            for (idx in c("sp500", "djia", "nasdaq", "inflation", "unemployment")) {
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
                    closest_date_rows <- index_data %>%
                        filter(date >= ref_date) %>%
                        arrange(date)
                    
                    if (nrow(closest_date_rows) == 0) {
                        # Skip if no suitable reference date found
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
            
            # Create unique identifier for each line
            plot_data$line_id <- paste(plot_data$president, plot_data$index_name, sep = "_")
            end_points$line_id <- paste(end_points$president, end_points$index_name, sep = "_")
            
            # Create base ggplot (without labels - we'll add them in plotly)
            # Key change: Set the color mapping to president instead of party
            p <- ggplot(plot_data, aes(x = day, y = percent_change, 
                                       group = line_id,
                                       # Add president to the text for tooltip
                                       text = paste("President:", president, 
                                                    "<br>Party:", party,
                                                    "<br>Day:", day,
                                                    "<br>Change:", round(percent_change, 2), "%"))) +
                # Set the color to a fixed mapping outside the aes() call
                geom_line(aes(color = party), size = 1, alpha = 0.5) +
                geom_point(data = end_points, aes(color = party), size = 2) +
                # Add facets by index - HORIZONTAL layout (3 columns)
                facet_wrap(~ index_name, ncol = 3) +
                geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
                scale_color_manual(values = party_color_values()) +
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
            p_ly <- ggplotly(p, tooltip = "text") %>%
                layout(
                    legend = list(orientation = "h", y = -0.2),
                    # Add more space on the right margin for labels
                    margin = list(r = 100)
                )
            
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
            
            # Modify to highlight only the hovered line (not the whole party group)
            p_ly <- p_ly %>% onRender("
        function(el, x) {
          el.on('plotly_hover', function(d) {
            var curveNumber = d.points[0].curveNumber;
            var lineOpacity = 1;
            var otherOpacity = 0.1;
            
            // Get all plotly traces (lines)
            var traces = document.querySelectorAll('.scatterlayer .trace');
            
            // Set opacity for all traces
            for (var i = 0; i < traces.length; i++) {
              if (i === curveNumber) {
                traces[i].style.opacity = lineOpacity;
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
            
            return(p_ly)
            
        } else {
            # Plot for a single index
            index_name <- unique(plot_data$index_name)[1]
            
            # Get end points for each president
            end_points <- plot_data %>% 
                group_by(president, party) %>% 
                filter(day == max(day)) %>%
                mutate(label = paste0(president, " (", round(percent_change, 1), "%)"))
            
            # Create unique identifiers for each president's line
            plot_data$line_id <- plot_data$president
            
            # Create base ggplot (without labels - we'll add them in plotly)
            # Key change: Set the color mapping to president instead of party in the aes() function
            p <- ggplot(plot_data, aes(x = day, y = percent_change, 
                                       group = president,
                                       text = paste("President:", president, 
                                                    "<br>Party:", party,
                                                    "<br>Day:", day,
                                                    "<br>Change:", round(percent_change, 2), "%"))) +
                # Set the color to party outside the aes() call
                geom_line(aes(color = party), size = 1, alpha = 0.5) +
                geom_point(data = end_points, aes(color = party), size = 3) +
                geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
                scale_color_manual(values = party_color_values()) +
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
            p_ly <- ggplotly(p, tooltip = "text") %>%
                layout(
                    legend = list(orientation = "h", y = -0.2),
                    # Add more space on the right margin for labels
                    margin = list(r = 100)
                )
            
            # Add hover effects for individual lines - make them more visible on hover
            p_ly <- p_ly %>% onRender("
        function(el, x) {
          el.on('plotly_hover', function(d) {
            var curveNumber = d.points[0].curveNumber;
            var lineOpacity = 1;
            var otherOpacity = 0.1;
            
            // Get all plotly traces (lines)
            var traces = document.querySelectorAll('.scatterlayer .trace');
            
            // Set opacity for all traces
            for (var i = 0; i < traces.length; i++) {
              if (i === curveNumber) {
                traces[i].style.opacity = lineOpacity;
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
        
        # Determine colors based on theme
        pos_color <- if (isTRUE(input$dark_mode)) "#00bc8c" else "#18bc9c"
        neg_color <- if (isTRUE(input$dark_mode)) "#e74c3c" else "#e74c3c"
        dem_color <- if (isTRUE(input$dark_mode)) "#3498db" else "#0000FF" 
        rep_color <- if (isTRUE(input$dark_mode)) "#e74c3c" else "#FF0000"
        
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
                        color = styleInterval(0, c(neg_color, pos_color)),
                        fontWeight = 'bold') %>%
            formatStyle('party',
                        backgroundColor = styleEqual(
                            c("Democratic", "Republican"), 
                            c(dem_color, rep_color)
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
                                 "inflation" = "Inflation",
                                 "unemployment" = "Unemployment",
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
                    scale_color_manual(values = party_color_values()) +
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
                    scale_color_manual(values = party_color_values()) +
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
                                 "inflation" = "Inflation",
                                 "unemployment" = "Unemployment",
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