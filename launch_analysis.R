# Rocket Launch Data Analysis
# This script analyzes the launches.csv dataset to provide statistical summaries
# and visualizations of rocket launch data using base R functions.

# Read the data
launches <- read.csv("launches.csv")

# Display the structure of the data
cat("Structure of the dataset:\n")
str(launches)

# Convert Date to proper date format
launches$Date <- as.Date(launches$Date)

# Basic summary statistics for each variable (except Date)
cat("\n\nSummary statistics for each variable:\n")
summary(launches[, c("Vehicle", "SinceFirst", "Success")])

# Count of unique vehicles
cat("\n\nNumber of unique vehicle types:", length(unique(launches$Vehicle)))

# Overall proportion of successful launches
overall_success_rate <- mean(launches$Success)
cat("\n\nOverall proportion of successful launches:", 
    round(overall_success_rate * 100, 2), "%")

# Success rate for first attempts (where SinceFirst = 0)
first_attempts <- launches[launches$SinceFirst == 0, ]
first_attempt_success_rate <- mean(first_attempts$Success)
cat("\n\nProportion of successful launches among first attempts:", 
    round(first_attempt_success_rate * 100, 2), "%")

# Vehicle with the most launches
launch_counts <- table(launches$Vehicle)
most_launches <- names(launch_counts)[which.max(launch_counts)]
cat("\n\nVehicle type with the most launches:", most_launches, 
    "with", max(launch_counts), "launches")

# Success rates by vehicle type
vehicle_success <- aggregate(Success ~ Vehicle, data = launches, FUN = mean)
vehicle_counts <- as.data.frame(table(launches$Vehicle))
colnames(vehicle_counts) <- c("Vehicle", "Launches")

# Merge success rates with launch counts
vehicle_stats <- merge(vehicle_success, vehicle_counts, by = "Vehicle")
vehicle_stats$Successes <- round(vehicle_stats$Success * vehicle_stats$Launches)
vehicle_stats <- vehicle_stats[order(-vehicle_stats$Success, -vehicle_stats$Launches), ]

# Filter for vehicles with at least 5 launches for more reliable success rate
reliable_stats <- vehicle_stats[vehicle_stats$Launches >= 5, ]
cat("\n\nVehicle types with highest success rates (minimum 5 launches):\n")
print(head(reliable_stats, 10))

# Vehicle with the highest success rate (minimum 5 launches)
highest_success <- reliable_stats[which.max(reliable_stats$Success), ]
cat("\n\nVehicle type with the highest success rate (min 5 launches):", 
    highest_success$Vehicle, "with", 
    round(highest_success$Success * 100, 2), "% success rate")

# Create visualizations using base R - display directly in R console/window

# 1. Overall success vs failure pie chart
cat("\n\nCreating pie chart for overall success rate...\n")
success_counts <- c(sum(launches$Success), sum(1 - launches$Success))
labels <- c("Success", "Failure")
percentages <- round(100 * success_counts / sum(success_counts), 1)
labels <- paste(labels, "(", percentages, "%)", sep="")
pie(success_counts, labels = labels, main="Overall Launch Success Rate", 
    col=c("darkgreen", "firebrick"))

# Save the plot if needed (optional)
# To save: uncomment the next two lines
# pdf("overall_success_rate.pdf", width=8, height=6)
# dev.off()

# Wait for user to continue
cat("Press Enter to continue to the next plot...\n")
invisible(readline())

# 2. Success rates by vehicle type (for top 15 vehicles by launch count)
cat("\nCreating barplot for success rates by vehicle type...\n")
top_vehicles <- vehicle_stats[order(-vehicle_stats$Launches), ][1:15, ]
top_vehicles <- top_vehicles[order(top_vehicles$Success), ]  # Order by success rate for the plot

barplot(top_vehicles$Success, names.arg = top_vehicles$Vehicle, 
        main="Success Rates by Vehicle Type (Top 15 by Launch Count)",
        xlab="Vehicle Type", ylab="Success Rate",
        col=colorRampPalette(c("orange", "darkgreen"))(15),
        las=2, cex.names=0.7)
text(x = seq(from = 0.7, by = 1.2, length.out = 15),
     y = top_vehicles$Success + 0.03,
     labels = paste0(round(top_vehicles$Success*100, 1), "%"),
     cex = 0.8)

# Wait for user to continue
cat("Press Enter to continue to the next plot...\n")
invisible(readline())

# 3. First attempt success vs. later attempts
cat("\nCreating barplot for first attempts vs. later attempts...\n")
attempt_success <- c(
  mean(launches$Success[launches$SinceFirst == 0]),
  mean(launches$Success[launches$SinceFirst > 0])
)
barplot(attempt_success, names.arg = c("First Attempt", "Later Attempts"),
        main="Success Rate: First Attempts vs. Later Attempts",
        xlab="Attempt Type", ylab="Success Rate",
        col=c("skyblue", "darkblue"))
text(x = c(0.7, 1.9),
     y = attempt_success + 0.03,
     labels = paste0(round(attempt_success*100, 1), "%"))

# Wait for user to continue
cat("Press Enter to continue to the next plot...\n")
invisible(readline())

# 4. Launch counts by vehicle (top 15)
cat("\nCreating barplot for launch counts by vehicle...\n")
top_by_count <- vehicle_stats[order(-vehicle_stats$Launches), ][1:15, ]
barplot(top_by_count$Launches, names.arg = top_by_count$Vehicle,
        main="Number of Launches by Vehicle Type (Top 15)",
        xlab="Vehicle Type", ylab="Number of Launches",
        col=colorRampPalette(c("lightblue", "darkblue"))(15),
        las=2, cex.names=0.7)
text(x = seq(from = 0.7, by = 1.2, length.out = 15),
     y = top_by_count$Launches + 1,
     labels = top_by_count$Launches,
     cex = 0.8)

# Wait for user to continue
cat("Press Enter to continue to the next plot...\n")
invisible(readline())

# 5. Success rate over time (by year)
cat("\nCreating plot for success rate over time...\n")
launches$Year <- format(launches$Date, "%Y")
yearly_data <- aggregate(Success ~ Year, data = launches, FUN = mean)
yearly_counts <- as.data.frame(table(launches$Year))
colnames(yearly_counts) <- c("Year", "Launches")
yearly_stats <- merge(yearly_data, yearly_counts, by = "Year")
yearly_stats <- yearly_stats[yearly_stats$Launches >= 3, ]  # Only years with at least 3 launches
yearly_stats <- yearly_stats[order(yearly_stats$Year), ]

plot(1:nrow(yearly_stats), yearly_stats$Success, type="b", 
     xlab="Year", ylab="Success Rate", 
     main="Launch Success Rate Over Time",
     xaxt="n", ylim=c(0, 1))
axis(1, at=1:nrow(yearly_stats), labels=yearly_stats$Year, las=2, cex.axis=0.7)
points(1:nrow(yearly_stats), yearly_stats$Success, 
       cex=yearly_stats$Launches/10, pch=19, col="darkblue")
legend("bottomright", legend="Point size indicates number of launches", 
       cex=0.8, bty="n")

# Print a summary of the analysis
cat("\n\n=== SUMMARY OF FINDINGS ===\n")
cat("Total number of launches analyzed:", nrow(launches), "\n")
cat("Overall success rate:", round(overall_success_rate * 100, 2), "%\n")
cat("First attempt success rate:", round(first_attempt_success_rate * 100, 2), "%\n")
cat("Vehicle with most launches:", most_launches, "with", max(launch_counts), "launches\n")
cat("Vehicle with highest success rate (min 5 launches):", 
    highest_success$Vehicle, "with", 
    round(highest_success$Success * 100, 2), "% success rate\n")