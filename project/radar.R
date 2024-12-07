# Load necessary libraries
library(fmsb)
library(dplyr)

# Sample data
data <- data.frame(
  player = rep(c("Player A", "Player B", "Player C"), each = 5),
  season = rep(2018:2022, times = 3),
  goals = sample(0:20, 15, replace = TRUE),
  assists = sample(0:20, 15, replace = TRUE),
  passes = sample(0:20, 15, replace = TRUE),
  tackles = sample(0:20, 15, replace = TRUE),
  saves = sample(0:20, 15, replace = TRUE)
)



# Filter for selected players and seasons (replace with actual selections)
selected_data <- data %>%
  filter((player == "Player A" & season == 2022) |
        (player == "Player B" & season == 2022))

# Prepare data for radar chart
radar_data <- selected_data %>%
  select(goals, assists, passes, tackles, saves) %>%
  as.data.frame()

# Add min and max rows for fmsb radar chart
radar_data <- rbind(rep(20, 5), rep(0, 5), radar_data)

# Define colors for the radar chart
colors_border <- c("#40c9a242", "#A056B242", "#FFD16642")
colors_in <- c(rgb(0.25, 0.78, 0.64, 0.3), rgb(0.63, 0.34, 0.7, 0.3), rgb(1, 0.87, 0.34, 0.4))
legend_labels <- paste(selected_data$player, selected_data$season, sep = " - ")

img <- function(){
  png("radar_plot.png", width = 800, height = 800, bg = rgb(0.3,0.3,0.3,0.4))  # Adjust width and height as needed
  radarchart(radar_data, axistype = 1, cglty = 1, plty = 1,
             pcol = colors_border, pfcol = colors_in, plwd = 2,
             cglcol = "grey", axislabcol = "grey60", caxislabels = seq(0, 20, 5), cglwd = 1.5,
             vlcex = 0.8)
  legend("topright", legend = legend_labels, bty = "n", pch = 20, 
         col = colors_in, text.col = "grey20", cex = 1.2, pt.cex = 3)
  dev.off()
}

img()

