library(dplyr)
library(ggplot2)

# 1. Import Dataset
data <- read.csv("match1_messy_telemetry.csv",
                 stringsAsFactors = FALSE,
                 na.strings = c("NA", "NULL", "", " "))

# 2. Structure
str(data)
summary(data)

# 3. Missing Values Check
colSums(is.na(data))

# 4. Before Cleaning Visualization
ggplot(data, aes(y = as.numeric(Damage))) +
  geom_boxplot(fill = "red") +
  labs(title = "Before Cleaning: Damage Boxplot", y = "Damage") +
  theme_minimal()

# 5. Remove Duplicates
data <- distinct(data)

# 6. Cleaning
data$Team_Name <- toupper(trimws(data$Team_Name))

data$Damage <- suppressWarnings(as.numeric(gsub(",", "", data$Damage)))
data$Assist <- suppressWarnings(as.numeric(gsub("O", "0", data$Assist)))
data$Play_Time <- as.numeric(gsub("[^0-9.]", "", data$Play_Time))

# 7. Outliers
data$Finishes[data$Finishes < 0] <- NA
data$Damage[data$Damage > 5000] <- NA

# 8. Imputation
data$Damage[is.na(data$Damage)] <- median(data$Damage, na.rm = TRUE)
data$Assist[is.na(data$Assist)] <- 0
data$Finishes[is.na(data$Finishes)] <- 0
data$Play_Time[is.na(data$Play_Time)] <- mean(data$Play_Time, na.rm = TRUE)

# Final Integrity
data <- na.omit(data)

# 9. Statistics
summary(data)
cat("Mean Damage:", mean(data$Damage), "\n")
cat("Median Damage:", median(data$Damage), "\n")
cat("SD Damage:", sd(data$Damage), "\n")

# VISUALIZATIONS AFTER CLEANING

# Histogram
ggplot(data, aes(x = Damage)) +
  geom_histogram(bins = 15, fill = "blue", color = "black") +
  labs(title = "Damage Distribution", x = "Damage", y = "Count") +
  theme_minimal()

# Boxplot
ggplot(data, aes(y = Damage)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Damage Boxplot", y = "Damage") +
  theme_minimal()

# Scatter Plot
ggplot(data, aes(x = Play_Time, y = Damage, color = Team_Name)) +
  geom_point(size = 3) +
  labs(title = "Play Time vs Damage", x = "Play Time", y = "Damage") +
  theme_minimal()

# Bar Chart
team_stats <- data %>%
  group_by(Team_Name) %>%
  summarize(Total = sum(Finishes))

ggplot(team_stats, aes(x = reorder(Team_Name, -Total),
                       y = Total,
                       fill = Team_Name)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Finishes by Team", x = "Team", y = "Finishes") +
  theme_minimal() +
  theme(legend.position = "none")

# Pie Chart
team_counts <- data %>%
  group_by(Team_Name) %>%
  summarize(count = n())

png("PieChart.png")
pie(team_counts$count,
    labels = team_counts$Team_Name,
    main = "Player Distribution",
    col = rainbow(nrow(team_counts)))
dev.off()
