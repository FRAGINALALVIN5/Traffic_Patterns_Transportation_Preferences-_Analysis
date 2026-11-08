
# =====================================================
# PROJECT: Traffic Patterns and Transportation
#          Preferences Analysis
# FILE: analysis.R
# =====================================================

# 1. INSTALL AND LOAD PACKAGES

packages <- c("dplyr", "ggplot2", "readr", "lubridate")

for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}

library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)

# 2. IMPORT DATASET

traffic_data <- read_csv(
  "traffic_patterns_transportation_preferences.csv",
  show_col_types = FALSE
)

# 3. DATA CLEANING AND PREPROCESSING

traffic_data <- traffic_data %>%
  mutate(
    Travel_Date = as.Date(Travel_Date),
    Age = as.numeric(Age),
    Travel_Distance_km = as.numeric(Travel_Distance_km),
    Travel_Time_min = as.numeric(Travel_Time_min),
    Departure_Hour = hour(
      as.POSIXct(
        Departure_Time,
        format = "%H:%M"
      )
    )
  ) %>%
  filter(
    !is.na(Respondent_ID),
    !is.na(Age),
    !is.na(Transportation_Mode),
    !is.na(Travel_Time_min),
    Travel_Time_min > 0
  ) %>%
  distinct(Respondent_ID, .keep_all = TRUE)

# 4. DATASET OVERVIEW

cat("\n====================================\n")
cat("       DATASET OVERVIEW\n")
cat("====================================\n")

cat("Total Records:", nrow(traffic_data), "\n")
cat("Total Columns:", ncol(traffic_data), "\n")

# 5. SUMMARY STATISTICS

cat("\n===== SUMMARY STATISTICS =====\n")

summary_stats <- traffic_data %>%
  summarise(
    Total_Commuters = n(),
    Average_Age = round(mean(Age, na.rm = TRUE), 2),
    Average_Distance = round(
      mean(Travel_Distance_km, na.rm = TRUE), 2
    ),
    Average_Travel_Time = round(
      mean(Travel_Time_min, na.rm = TRUE), 2
    ),
    Maximum_Travel_Time = max(
      Travel_Time_min, na.rm = TRUE
    ),
    Minimum_Travel_Time = min(
      Travel_Time_min, na.rm = TRUE
    )
  )

print(summary_stats)

# 6. TRANSPORTATION PREFERENCES

transport_summary <- traffic_data %>%
  count(Transportation_Mode, name = "Total") %>%
  arrange(desc(Total))

cat("\n===== TRANSPORTATION PREFERENCES =====\n")
print(transport_summary)

# 7. TRAFFIC CONDITION ANALYSIS

traffic_summary <- traffic_data %>%
  count(Traffic_Level, name = "Total") %>%
  arrange(desc(Total))

cat("\n===== TRAFFIC CONDITIONS =====\n")
print(traffic_summary)

# 8. AVERAGE TRAVEL TIME BY TRANSPORTATION

travel_time_summary <- traffic_data %>%
  group_by(Transportation_Mode) %>%
  summarise(
    Average_Travel_Time = round(
      mean(Travel_Time_min, na.rm = TRUE), 2
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(Average_Travel_Time))

cat("\n===== AVERAGE TRAVEL TIME =====\n")
print(travel_time_summary)

# 9. PEAK COMMUTING HOURS

peak_hours <- traffic_data %>%
  count(Departure_Hour, name = "Total_Trips") %>%
  arrange(Departure_Hour)

cat("\n===== PEAK COMMUTING HOURS =====\n")
print(peak_hours)

# 10. TRANSPORTATION PURPOSE ANALYSIS

purpose_summary <- traffic_data %>%
  count(Purpose_of_Travel, name = "Total") %>%
  arrange(desc(Total))

cat("\n===== PURPOSE OF TRAVEL =====\n")
print(purpose_summary)

# 11. SATISFACTION ANALYSIS

satisfaction_summary <- traffic_data %>%
  count(Satisfaction_Level, name = "Total")

cat("\n===== COMMUTER SATISFACTION =====\n")
print(satisfaction_summary)

# 12. TRANSPORTATION PREFERENCE CHART

chart1 <- ggplot(
  transport_summary,
  aes(
    x = reorder(Transportation_Mode, Total),
    y = Total
  )
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Transportation Preferences",
    x = "Transportation Mode",
    y = "Number of Commuters"
  ) +
  theme_minimal()

print(chart1)

# 13. TRAFFIC CONDITION CHART

chart2 <- ggplot(
  traffic_summary,
  aes(
    x = Traffic_Level,
    y = Total,
    fill = Traffic_Level
  )
) +
  geom_col() +
  labs(
    title = "Traffic Condition Distribution",
    x = "Traffic Level",
    y = "Number of Commuters"
  ) +
  theme_minimal()

print(chart2)

# 14. PEAK COMMUTING HOURS CHART

chart3 <- ggplot(
  peak_hours,
  aes(
    x = Departure_Hour,
    y = Total_Trips
  )
) +
  geom_line(color = "darkorange", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  scale_x_continuous(breaks = 5:21) +
  labs(
    title = "Peak Commuting Hours",
    x = "Departure Hour",
    y = "Number of Trips"
  ) +
  theme_minimal()

print(chart3)

# 15. AVERAGE TRAVEL TIME CHART

chart4 <- ggplot(
  travel_time_summary,
  aes(
    x = reorder(
      Transportation_Mode,
      Average_Travel_Time
    ),
    y = Average_Travel_Time
  )
) +
  geom_col(fill = "seagreen") +
  coord_flip() +
  labs(
    title = "Average Travel Time by Transportation",
    x = "Transportation Mode",
    y = "Average Travel Time (Minutes)"
  ) +
  theme_minimal()

print(chart4)

# 16. PURPOSE OF TRAVEL CHART

chart5 <- ggplot(
  purpose_summary,
  aes(
    x = reorder(Purpose_of_Travel, Total),
    y = Total
  )
) +
  geom_col(fill = "coral") +
  coord_flip() +
  labs(
    title = "Purpose of Travel",
    x = "Travel Purpose",
    y = "Number of Commuters"
  ) +
  theme_minimal()

print(chart5)

# 17. SATISFACTION CHART

chart6 <- ggplot(
  satisfaction_summary,
  aes(
    x = Satisfaction_Level,
    y = Total,
    fill = Satisfaction_Level
  )
) +
  geom_col() +
  labs(
    title = "Commuter Satisfaction Levels",
    x = "Satisfaction Level",
    y = "Number of Commuters"
  ) +
  theme_minimal()

print(chart6)

# 18. SAVE CLEANED DATASET

write_csv(
  traffic_data,
  "cleaned_traffic_data.csv"
)

# 19. SAVE SUMMARY RESULTS

write_csv(
  transport_summary,
  "transportation_summary.csv"
)

write_csv(
  traffic_summary,
  "traffic_summary.csv"
)

write_csv(
  travel_time_summary,
  "travel_time_summary.csv"
)

# 20. COMPLETION MESSAGE

cat("\n====================================\n")
cat(" ANALYSIS COMPLETED SUCCESSFULLY!\n")
cat("====================================\n")

cat("Total Records Analyzed:", nrow(traffic_data), "\n")
cat("Charts Generated: 6\n")
cat("Cleaned Dataset Saved!\n")
cat("Summary Reports Saved!\n")

cat("\nReady for Interactive Dashboard!\n")

