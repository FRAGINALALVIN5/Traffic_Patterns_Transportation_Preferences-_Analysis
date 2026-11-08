
# =====================================================
# PROJECT: Traffic Patterns and Transportation
#          Preferences Analysis
# FILE: app.R
# =====================================================

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)

# 1. LOAD DATASET

traffic_data <- read_csv(
  "traffic_patterns_transportation_preferences.csv",
  show_col_types = FALSE
)

traffic_data <- traffic_data %>%
  mutate(
    Travel_Date = as.Date(Travel_Date),
    Departure_Hour = hour(
      as.POSIXct(Departure_Time, format = "%H:%M")
    )
  )

# 2. USER INTERFACE

ui <- fluidPage(
  
  # CUSTOM CSS
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f3f6fa;
        font-family: Arial, sans-serif;
      }

      .main-title {
        color: #123b65;
        font-weight: bold;
        margin-bottom: 5px;
      }

      .subtitle {
        color: #64748b;
        margin-bottom: 25px;
      }

      .metric-card {
        background: white;
        padding: 20px;
        border-radius: 12px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.07);
        margin-bottom: 18px;
        min-height: 115px;
      }

      .metric-title {
        color: #64748b;
        font-size: 14px;
      }

      .metric-value {
        color: #123b65;
        font-size: 27px;
        font-weight: bold;
        margin-top: 10px;
      }

      .panel-box {
        background: white;
        padding: 18px;
        border-radius: 12px;
        margin-bottom: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.06);
      }

      .nav-tabs > li > a {
        font-weight: bold;
      }

      .btn-primary {
        background-color: #1769aa;
        border-color: #1769aa;
      }

      .sidebar {
        background-color: white;
        padding: 18px;
        border-radius: 12px;
      }
    "))
  ),
  
  # HEADER
  div(
    class = "main-title",
    h2("Traffic Patterns and Transportation Preferences Analysis")
  ),
  
  div(
    class = "subtitle",
    "Interactive Commuter Behavior and Traffic Analytics Dashboard"
  ),
  
  # FILTERS AND MAIN CONTENT
  sidebarLayout(
    
    sidebarPanel(
      class = "sidebar",
      
      h4("Dashboard Filters"),
      
      selectInput(
        "transport",
        "Transportation Mode:",
        choices = c("All", sort(unique(
          traffic_data$Transportation_Mode
        ))),
        selected = "All"
      ),
      
      selectInput(
        "traffic",
        "Traffic Level:",
        choices = c("All", "Low", "Moderate", "High"),
        selected = "All"
      ),
      
      selectInput(
        "purpose",
        "Purpose of Travel:",
        choices = c("All", sort(unique(
          traffic_data$Purpose_of_Travel
        ))),
        selected = "All"
      ),
      
      selectInput(
        "gender",
        "Gender:",
        choices = c("All", "Male", "Female"),
        selected = "All"
      ),
      
      sliderInput(
        "age_range",
        "Age Range:",
        min = min(traffic_data$Age, na.rm = TRUE),
        max = max(traffic_data$Age, na.rm = TRUE),
        value = range(traffic_data$Age, na.rm = TRUE)
      ),
      
      dateRangeInput(
        "date_range",
        "Travel Date:",
        start = min(traffic_data$Travel_Date, na.rm = TRUE),
        end = max(traffic_data$Travel_Date, na.rm = TRUE)
      ),
      
      actionButton(
        "reset",
        "Reset Filters",
        class = "btn-primary"
      ),
      
      hr(),
      
      downloadButton(
        "download_data",
        "Download Filtered CSV"
      )
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        # TAB 1: OVERVIEW
        
        tabPanel(
          "Overview",
          
          br(),
          
          fluidRow(
            column(
              3,
              div(
                class = "metric-card",
                div(class = "metric-title", "Total Commuters"),
                div(class = "metric-value",
                    textOutput("total_commuters"))
              )
            ),
            
            column(
              3,
              div(
                class = "metric-card",
                div(class = "metric-title", "Average Travel Time"),
                div(class = "metric-value",
                    textOutput("average_time"))
              )
            ),
            
            column(
              3,
              div(
                class = "metric-card",
                div(class = "metric-title", "Average Distance"),
                div(class = "metric-value",
                    textOutput("average_distance"))
              )
            ),
            
            column(
              3,
              div(
                class = "metric-card",
                div(class = "metric-title", "Most Used Transport"),
                div(class = "metric-value",
                    textOutput("popular_transport"))
              )
            )
          ),
          
          div(
            class = "panel-box",
            h4("Transportation Preferences"),
            plotOutput("transport_chart", height = "350px")
          ),
          
          div(
            class = "panel-box",
            h4("Traffic Condition Distribution"),
            plotOutput("traffic_chart", height = "350px")
          )
        ),
        
        # TAB 2: TRANSPORTATION ANALYSIS
        
        tabPanel(
          "Transportation Analysis",
          
          br(),
          
          div(
            class = "panel-box",
            h4("Transportation Mode Comparison"),
            plotOutput("mode_chart", height = "400px")
          ),
          
          div(
            class = "panel-box",
            h4("Transportation Usage Summary"),
            tableOutput("transport_table")
          )
        ),
        
        # TAB 3: TRAFFIC PATTERNS
        
        tabPanel(
          "Traffic Patterns",
          
          br(),
          
          div(
            class = "panel-box",
            h4("Peak Commuting Hours"),
            plotOutput("peak_chart", height = "400px")
          ),
          
          div(
            class = "panel-box",
            h4("Traffic Levels by Departure Hour"),
            plotOutput("hour_traffic_chart", height = "400px")
          )
        ),
        
        # TAB 4: TRAVEL TIME ANALYSIS
        
        tabPanel(
          "Travel Time",
          
          br(),
          
          div(
            class = "panel-box",
            h4("Average Travel Time by Transportation"),
            plotOutput("time_chart", height = "400px")
          ),
          
          div(
            class = "panel-box",
            h4("Travel Distance and Travel Time"),
            plotOutput("distance_chart", height = "400px")
          )
        ),
        
        # TAB 5: COMMUTER SATISFACTION
        
        tabPanel(
          "Satisfaction",
          
          br(),
          
          div(
            class = "panel-box",
            h4("Commuter Satisfaction Distribution"),
            plotOutput("satisfaction_chart", height = "400px")
          ),
          
          div(
            class = "panel-box",
            h4("Purpose of Travel"),
            plotOutput("purpose_chart", height = "400px")
          )
        ),
        
        # TAB 6: DATA RECORDS
        
        tabPanel(
          "Data Records",
          
          br(),
          
          div(
            class = "panel-box",
            h4("Commuter Records"),
            p("View the filtered commuter dataset below."),
            tableOutput("data_table")
          )
        )
      )
    )
  ),
  
  br(),
  
  tags$footer(
    style = "text-align:center; color:#64748b; padding:20px;",
    "Traffic Patterns and Transportation Preferences Analysis | R Shiny Project"
  )
)

# 3. SERVER

server <- function(input, output, session) {
  
  # RESET FILTERS
  
  observeEvent(input$reset, {
    
    updateSelectInput(
      session, "transport", selected = "All"
    )
    
    updateSelectInput(
      session, "traffic", selected = "All"
    )
    
    updateSelectInput(
      session, "purpose", selected = "All"
    )
    
    updateSelectInput(
      session, "gender", selected = "All"
    )
    
    updateSliderInput(
      session,
      "age_range",
      value = range(traffic_data$Age, na.rm = TRUE)
    )
    
    updateDateRangeInput(
      session,
      "date_range",
      start = min(traffic_data$Travel_Date, na.rm = TRUE),
      end = max(traffic_data$Travel_Date, na.rm = TRUE)
    )
  })
  
  # FILTERED DATA
  
  filtered_data <- reactive({
    
    req(input$age_range)
    req(input$date_range)
    
    data <- traffic_data
    
    if (input$transport != "All") {
      data <- data %>%
        filter(Transportation_Mode == input$transport)
    }
    
    if (input$traffic != "All") {
      data <- data %>%
        filter(Traffic_Level == input$traffic)
    }
    
    if (input$purpose != "All") {
      data <- data %>%
        filter(Purpose_of_Travel == input$purpose)
    }
    
    if (input$gender != "All") {
      data <- data %>%
        filter(Gender == input$gender)
    }
    
    data %>%
      filter(
        Age >= input$age_range[1],
        Age <= input$age_range[2],
        Travel_Date >= input$date_range[1],
        Travel_Date <= input$date_range[2]
      )
  })
  
  # EMPTY DATA CHECK
  
  has_data <- reactive({
    nrow(filtered_data()) > 0
  })
  
  # 4. KPI CARDS
  
  output$total_commuters <- renderText({
    nrow(filtered_data())
  })
  
  output$average_time <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) return("N/A")
    
    paste0(
      round(mean(data$Travel_Time_min, na.rm = TRUE), 1),
      " min"
    )
  })
  
  output$average_distance <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) return("N/A")
    
    paste0(
      round(mean(data$Travel_Distance_km, na.rm = TRUE), 1),
      " km"
    )
  })
  
  output$popular_transport <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) return("N/A")
    
    popular <- data %>%
      count(Transportation_Mode, sort = TRUE) %>%
      slice(1) %>%
      pull(Transportation_Mode)
    
    popular
  })
  
  # 5. TRANSPORTATION CHART
  
  output$transport_chart <- renderPlot({
    
    req(has_data())
    
    data <- filtered_data() %>%
      count(Transportation_Mode)
    
    ggplot(
      data,
      aes(
        x = reorder(Transportation_Mode, n),
        y = n
      )
    ) +
      geom_col(fill = "#2878B5") +
      coord_flip() +
      labs(
        x = "Transportation Mode",
        y = "Number of Commuters"
      ) +
      theme_minimal(base_size = 13)
  })
  
  # 6. TRAFFIC CHART
  
  output$traffic_chart <- renderPlot({
    
    req(has_data())
    
    data <- filtered_data() %>%
      count(Traffic_Level)
    
    ggplot(
      data,
      aes(
        x = Traffic_Level,
        y = n,
        fill = Traffic_Level
      )
    ) +
      geom_col() +
      scale_fill_manual(
        values = c(
          "Low" = "#2CA58D",
          "Moderate" = "#F4B942",
          "High" = "#E76F51"
        )
      ) +
      labs(
        x = "Traffic Level",
        y = "Number of Commuters"
      ) +
      theme_minimal(base_size = 13)
  })
  
  # 7. TRANSPORTATION COMPARISON
  
  output$mode_chart <- renderPlot({
    
    req(has_data())
    
    data <- filtered_data() %>%
      count(Transportation_Mode)
    
    ggplot(
      data,
      aes(
        x = reorder(Transportation_Mode, n),
        y = n
      )
    ) +
      geom_col(fill = "#20A4A8") +
      coord_flip() +
      labs(
        title = "Transportation Usage Comparison",
        x = "Transportation Mode",
        y = "Total Commuters"
      ) +
      theme_minimal()
  })
  
  output$transport_table <- renderTable({
    
    req(has_data())
    
    filtered_data() %>%
      count(Transportation_Mode, name = "Total_Commuters") %>%
      arrange(desc(Total_Commuters))
  })
  
  # 8. PEAK HOURS
  
  output$peak_chart <- renderPlot({
    
    req(has_data())
    
    data <- filtered_data() %>%
      count(Departure_Hour, name = "Total_Trips")
    
    ggplot(
      data,
      aes(
        x = Departure_Hour,
        y = Total_Trips
      )
    ) +
      geom_line(color = "#E76F51", linewidth = 1.2) +
      geom_point(color = "#2878B5", size = 3) +
      scale_x_continuous(breaks = 5:21) +
      labs(
        title = "Peak Commuting Hours",
        x = "Departure Hour",
        y = "Number of Trips"
      ) +
      theme_minimal()
  })
  
  # 9. TRAFFIC BY HOUR
  
  output$hour_traffic_chart <- renderPlot({
    
    req(has_data())
    
    data <- filtered_data() %>%
      count(Departure_Hour, Traffic_Level)
    
    ggplot(
      data,
      aes(
        x = Departure_Hour,
        y = n,
        color = Traffic_Level
      )
    ) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = 5:21) +
      labs(
        x = "Departure Hour",
        y = "Number of Commuters",
        color = "Traffic Level"
      ) +
      theme_minimal()
  })
  
  # 10. TRAVEL TIME CHART
  
  output$time_chart <- renderPlot({
    
    req(has_data())
    
    data <- filtered_data() %>%
      group_by(Transportation_Mode) %>%
      summarise(
        Average_Time = mean(
          Travel_Time_min,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    ggplot(
      data,
      aes(
        x = reorder(Transportation_Mode, Average_Time),
        y = Average_Time
      )
    ) +
      geom_col(fill = "#2878B5") +
      coord_flip() +
      labs(
        x = "Transportation Mode",
        y = "Average Travel Time (Minutes)"
      ) +
      theme_minimal()
  })
  
  # 11. DISTANCE AND TIME
  
  output$distance_chart <- renderPlot({
    
    req(has_data())
    
    ggplot(
      filtered_data(),
      aes(
        x = Travel_Distance_km,
        y = Travel_Time_min,
        color = Transportation_Mode
      )
    ) +
      geom_point(size = 3, alpha = 0.7) +
      labs(
        x = "Travel Distance (km)",
        y = "Travel Time (minutes)",
        color = "Transportation"
      ) +
      theme_minimal()
  })
  
  # 12. SATISFACTION CHART
  
  output$satisfaction_chart <- renderPlot({
    
    req(has_data())
    
    data <- filtered_data() %>%
      count(Satisfaction_Level)
    
    ggplot(
      data,
      aes(
        x = Satisfaction_Level,
        y = n,
        fill = Satisfaction_Level
      )
    ) +
      geom_col() +
      scale_fill_manual(
        values = c(
          "Satisfied" = "#2CA58D",
          "Neutral" = "#F4B942",
          "Dissatisfied" = "#E76F51"
        )
      ) +
      labs(
        x = "Satisfaction Level",
        y = "Number of Commuters"
      ) +
      theme_minimal()
  })
  
  # 13. PURPOSE OF TRAVEL
  
  output$purpose_chart <- renderPlot({
    
    req(has_data())
    
    data <- filtered_data() %>%
      count(Purpose_of_Travel)
    
    ggplot(
      data,
      aes(
        x = reorder(Purpose_of_Travel, n),
        y = n
      )
    ) +
      geom_col(fill = "#20A4A8") +
      coord_flip() +
      labs(
        x = "Purpose of Travel",
        y = "Number of Commuters"
      ) +
      theme_minimal()
  })
  
  # 14. DATA TABLE
  
  output$data_table <- renderTable({
    
    filtered_data() %>%
      select(
        Respondent_ID,
        Travel_Date,
        Age,
        Gender,
        Transportation_Mode,
        Travel_Distance_km,
        Departure_Time,
        Travel_Time_min,
        Traffic_Level,
        Purpose_of_Travel,
        Satisfaction_Level
      )
  })
  
  # 15. DOWNLOAD FILTERED DATA
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      paste0(
        "filtered_traffic_data_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )
  
}

# 16. RUN APPLICATION

shinyApp(
  ui = ui,
  server = server
)
