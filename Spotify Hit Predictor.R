# Load necessary libraries
library(ggplot2)
library(dplyr)
# Read the dataset
data <- read.csv("C:/Users/avaishampayan2521/Documents/SEM 2/MIS 720/Project/DataSet/dataset-of-10s.csv", header = TRUE)

# Calculate summary statistics for all numerical columns
summary_stats <- data %>%
  dplyr::select(where(is.numeric)) %>%
  summary()

# Calculate variance and standard deviation for each numerical column
variance_sd <- data %>%
  dplyr::select(where(is.numeric)) %>%
  summarise(across(
    everything(),
    list(var = ~ var(., na.rm = TRUE),  # Variance
         sd = ~ sd(., na.rm = TRUE))    # Standard Deviation
  ))

# Print the summary statistics
cat("Summary Statistics:\n")
print(knitr::kable(summary_stats, format = "markdown"))

# Print variance and standard deviation
cat("\nVariance and Standard Deviation:\n")
print(knitr::kable(variance_sd, format = "markdown"))

# List of quantitative variables
quantitative_vars <- c("danceability", "energy", "key", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo", "duration_ms", "chorus_hit", "sections")

# Create the directory for storing boxplots if it doesn't exist
if (!dir.exists("BoxPlots")) {
  dir.create("BoxPlots")
}

# Ensure that 'target' is a factor with appropriate labels
data$target <- factor(data$target, levels = c(0, 1), labels = c("Flop", "Hit"))

# Loop through each quantitative variable to create and save a boxplot
for (var in quantitative_vars) {
  # Create the boxplot
  p <- ggplot(data, aes(x = target, y = get(var), fill = target)) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", var, "by Hit or Flop Status"), x = "Status", y = var) +
    theme_minimal(base_size = 14) +  # Set base font size
    theme(plot.background = element_rect(fill = "white"),  # Set plot background to white
          text = element_text(size = 14),  # Increase text size for better readability
          axis.title = element_text(size = 12)) +  # Increase axis title text size
    scale_fill_manual(values = c("#FF9999", "#9999FF"))  # Color coding for visual distinction
  
  # Save the plot to the 'BoxPlots' directory with a file name based on the variable
  ggsave(filename = paste("BoxPlots/boxplot_", gsub(" ", "_", var), ".png", sep=""), plot = p, width = 10, height = 8, units = "in")
}

# Create the directory for storing histograms if it doesn't exist
if (!dir.exists("Histograms")) {
  dir.create("Histograms")
}

# Create and save histograms for each quantitative variable
for (var in quantitative_vars) {
  p <- ggplot(data, aes(x = get(var))) +
    geom_histogram(bins = 30, fill = "#69b3a2", color = "black") +  # You can adjust the number of bins
    labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
    theme_minimal() +
    theme(text = element_text(size = 12))  # Increase text size for better readability
  
  # Save the plot in the specified directory
  ggsave(filename = paste("Histograms/histogram_", var, ".png", sep=""), plot = p, width = 10, height = 8, units = "in")
}

# List of qualitative variables
qualitative_vars <- c("track", "artist", "uri", "mode", "time_signature", "target")

# Create the directory for storing pie charts if it doesn't exist
if (!dir.exists("PieCharts")) {
  dir.create("PieCharts")
}

# Analyze and plot for each qualitative variable
for (var in qualitative_vars) {
  # Calculate the frequency of each level
  freq <- table(data[[var]])
  print(paste("Levels in", var, ":", length(freq)))  # Print the number of levels
  print(freq)  # Print the frequency of each level
  
  # Save the pie chart
  pie_filename <- paste("PieCharts/pie_chart_", gsub(" ", "_", var), ".png", sep="")  # Create filename
  png(file = pie_filename, width = 800, height = 600)  # Start png device
  pie(freq, main = paste("Pie Chart of", var), col = rainbow(length(freq)))  # Create pie chart
  dev.off()  # Close png device
}

# Analyze and print the number of levels for each qualitative variable
for (var in qualitative_vars) {
  freq <- table(data[[var]])
  print(paste("Levels in", var, ":", length(freq)))  # Print the number of levels
}

# Create the directory for storing bar plots if it doesn't exist
if (!dir.exists("BarPlots")) {
  dir.create("BarPlots")
}

# Create and save bar plots for each qualitative variable
for (var in qualitative_vars) {
  # Create the bar plot using ggplot2
  p <- ggplot(data, aes_string(x = var, fill = "factor(target)")) +
    geom_bar(position = "dodge") +
    labs(title = paste("Bar Plot of", var, "by Hit or Flop Status"), x = var, y = "Count") +
    scale_fill_manual(values = c("#FF9999", "#9999FF"), labels = c("Flop", "Hit")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x labels for better readability
  
  # Save the plot in the specified directory
  ggsave(filename = paste("BarPlots/barplot_", gsub(" ", "_", var), ".png", sep=""), plot = p, width = 10, height = 8, units = "in")
}

# Create the directory for storing density line plots if it doesn't exist
if (!dir.exists("DensityLinePlots")) {
  dir.create("DensityLinePlots")
}

# Loop through each quantitative variable to create and save a density plot
for (var in quantitative_vars) {
  # Create the density plot
  p <- ggplot(data, aes_string(x = var, color = "target")) + 
    geom_density(stat = "density", fill = NA) +  # Use lines only with no fill
    labs(title = paste("Density Plot of", var, "by Hit or Flop Status"), x = var, y = "Density") +
    scale_color_manual(values = c("red", "blue")) +  # Red for Flop, Blue for Hit
    theme_minimal() +  # Minimalist theme
    theme(plot.background = element_rect(fill = "white", colour = NA),  # Ensure background is white
          panel.background = element_rect(fill = "white", colour = NA),  # Ensure panel background is white
          legend.title = element_blank())  # Hide the legend title
  
  # Save the plot to the 'DensityLinePlots' directory with a file name based on the variable
  ggsave(filename = paste("DensityLinePlots/density_line_", gsub(" ", "_", var), ".png", sep=""), plot = p, width = 10, height = 8, units = "in")
}

# Select only numeric columns for correlation analysis
df <- data[, c('danceability', 'energy', 'key', 'loudness', 'mode',
               'speechiness', 'acousticness', 'instrumentalness', 'liveness',
               'valence', 'tempo', 'duration_ms', 'time_signature', 'chorus_hit',
               'sections')]

# Compute the correlation matrix
correlation_matrix <- cor(df, use = "complete.obs")  # Using 'complete.obs' to handle missing values

# Print the correlation matrix
print(correlation_matrix)

# Install corrplot package if not already installed
if (!require(corrplot)) {
  install.packages("corrplot")
}

library(corrplot)

# Generate the heatmap
corrplot(correlation_matrix, method = "color",
         title = "Pearson Correlation of Song Attributes",
         type = "upper", 
         order = "hclust",  # This will reorder the matrix according to the hclust result
         addCoef.col = "black",  # Color for the correlation coefficients
         tl.col = "black",  # Text label color
         tl.srt = 45,  # Text label rotation in degrees
         tl.cex = 0.6,  # Text label size
         cl.cex = 0.7,  # Color legend size
         number.cex = 0.6,  # Correlation coefficient text size
         col = colorRampPalette(c("blue", "white", "red"))(200))  # Using a blue-white-red palette


# Load required libraries
library(ggplot2)
library(ggpubr)
library(pheatmap)
library(e1071)
library(caret)
library(tidyverse)
library(corrplot)
library(dplyr)

# Read dataset
dataset <- read.csv("C:/Users/avaishampayan2521/Documents/SEM 2/MIS 720/Project/DataSet/dataset-of-10s.csv", header = TRUE)
head(dataset)

# Clean dataset
dataset$artist <- as.character(dataset$artist)
featuring <- strsplit(dataset$artist, "Featuring")
dataset$featuring <- sapply(featuring, "[", 2)
dataset$featuring <- as.factor(ifelse(is.na(dataset$featuring), 0, 1))

# Check for missing values
cat("Number of missing values in dataset:", sapply(dataset, function(x) sum(is.na(x))), "\n")

# Remove duplicates
dataset <- dataset[!duplicated(dataset[, c('track', 'artist')]), ]

# Turn off scientific notation
options(scipen = 999)

# Drop unnecessary columns
dataset <- within(dataset, rm('uri'))

# Define all quantitative variables
quantitative_vars <- c("danceability", "energy", "key", "loudness", 
                       "mode", "speechiness", "acousticness", 
                       "instrumentalness", "liveness", "valence", 
                       "tempo", "duration_ms", "time_signature", 
                       "chorus_hit", "sections")

# Filter quantitative variables from the dataset
quantitative_dataset <- dataset[, quantitative_vars]

# Display the structure of the quantitative dataset
str(quantitative_dataset)

# Print the list of quantitative variables
cat("Quantitative Variables:\n")
print(quantitative_vars)

# Convert target variable to factor
dataset$target <- factor(dataset$target)

# Create a new dataset with labels for better visualization
df.labels <- dataset[, 3:19]
df.labels$target <- ifelse(df.labels$target == 1, 'Hit', 'Flop')
df.labels$featuring <- ifelse(df.labels$featuring == 1, 'Yes', 'No')
df.labels$target <- factor(df.labels$target)

# Set seed for reproducibility
set.seed(10)

# Sample training data
train <- sample(nrow(dataset), nrow(dataset)/2)

#install.packages("ROCR")
library(ROCR)

# Tune SVM model with radial kernel
tune_radial <- tune(svm, target ~ ., data = dataset[train, 3:19], kernel = "radial", 
                    ranges = list(cost = c(1, 5), gamma = c(0.1, 1)))

# Extract best radial SVM model
best_radial_svm <- tune_radial$best.model

# Extract optimal parameters for radial SVM
opt_radial_gamma <- best_radial_svm$gamma
opt_radial_cost <- best_radial_svm$cost

# Make predictions with radial SVM
best_radial_pred <- predict(best_radial_svm, newdata = dataset[-train, 3:19])

# Calculate confusion matrix for radial SVM
conf_mat_radial <- confusionMatrix(best_radial_pred, dataset$target[-train])

# Calculate radial SVM test error
svm_radial_err <- with(dataset, mean(best_radial_pred != target[-train]))

# Extracting metrics from confusion matrix for radial SVM
accuracy_radial <- conf_mat_radial$overall['Accuracy']
precision_radial <- conf_mat_radial$byClass['Precision']
recall_radial <- conf_mat_radial$byClass['Recall']
f1_score_radial <- 2 * (precision_radial * recall_radial) / (precision_radial + recall_radial)

# Calculate misclassification rate for radial SVM
misclassification_radial <- 1 - accuracy_radial

# Print results for radial SVM
cat("Radial SVM Test Error:", svm_radial_err, "\n")
cat("Accuracy (Radial):", accuracy_radial, "\n")
cat("Precision (Radial):", precision_radial, "\n")
cat("Recall (Radial):", recall_radial, "\n")
cat("F1 Score (Radial):", f1_score_radial, "\n")
cat("Misclassification Rate (Radial):", misclassification_radial, "\n")
print(conf_mat_radial)

# Tune SVM model with linear kernel
tune_linear <- tune(svm, target ~ ., data = dataset[train, 3:19], kernel = "linear", 
                    ranges = list(cost = c(1, 5)))

# Extract best linear SVM model
best_linear_svm <- tune_linear$best.model

# Make predictions with linear SVM
best_linear_pred <- predict(best_linear_svm, newdata = dataset[-train, 3:19])

# Calculate confusion matrix for linear SVM
conf_mat_linear <- confusionMatrix(best_linear_pred, dataset$target[-train])

# Calculate linear SVM test error
svm_linear_err <- with(dataset, mean(best_linear_pred != target[-train]))

# Extracting metrics from confusion matrix for linear SVM
accuracy_linear <- conf_mat_linear$overall['Accuracy']
precision_linear <- conf_mat_linear$byClass['Precision']
recall_linear <- conf_mat_linear$byClass['Recall']
f1_score_linear <- 2 * (precision_linear * recall_linear) / (precision_linear + recall_linear)

# Calculate misclassification rate for linear SVM
misclassification_linear <- 1 - accuracy_linear

# Print results for linear SVM
cat("Linear SVM Test Error:", svm_linear_err, "\n")
cat("Accuracy (Linear):", accuracy_linear, "\n")
cat("Precision (Linear):", precision_linear, "\n")
cat("Recall (Linear):", recall_linear, "\n")
cat("F1 Score (Linear):", f1_score_linear, "\n")
cat("Misclassification Rate (Linear):", misclassification_linear, "\n")
print(conf_mat_linear)

# Tune SVM model with polynomial kernel
tune_poly <- tune(svm, target ~ ., data = dataset[train, 3:19], kernel = "polynomial", 
                  ranges = list(cost = c(1, 5), degree = c(2, 3)))

# Extract best polynomial SVM model
best_poly_svm <- tune_poly$best.model

# Extract optimal parameters for polynomial SVM
opt_poly_degree <- best_poly_svm$degree
opt_poly_cost <- best_poly_svm$cost

# Make predictions with polynomial SVM
best_poly_pred <- predict(best_poly_svm, newdata = dataset[-train, 3:19])

# Calculate confusion matrix for polynomial SVM
conf_mat_poly <- confusionMatrix(best_poly_pred, dataset$target[-train])

# Calculate polynomial SVM test error
svm_poly_err <- with(dataset, mean(best_poly_pred != target[-train]))

# Extracting metrics from confusion matrix for polynomial SVM
accuracy_poly <- conf_mat_poly$overall['Accuracy']
precision_poly <- conf_mat_poly$byClass['Precision']
recall_poly <- conf_mat_poly$byClass['Recall']
f1_score_poly <- 2 * (precision_poly * recall_poly) / (precision_poly + recall_poly)

# Calculate misclassification rate for polynomial SVM
misclassification_poly <- 1 - accuracy_poly

# Print results for polynomial SVM
cat("Polynomial SVM Test Error:", svm_poly_err, "\n")
cat("Accuracy (Polynomial):", accuracy_poly, "\n")
cat("Precision (Polynomial):", precision_poly, "\n")
cat("Recall (Polynomial):", recall_poly, "\n")
cat("F1 Score (Polynomial):", f1_score_poly, "\n")
cat("Misclassification Rate (Polynomial):", misclassification_poly, "\n")
print(conf_mat_poly)

# Ensure predictions and true labels have the same length
n_samples <- length(dataset$target[-train])

# Make predictions with each SVM model
best_radial_pred <- predict(best_radial_svm, newdata = dataset[-train, 3:19])
best_linear_pred <- predict(best_linear_svm, newdata = dataset[-train, 3:19])
best_poly_pred <- predict(best_poly_svm, newdata = dataset[-train, 3:19])

# Extract true labels
true_labels <- dataset$target[-train]

# Create prediction objects for each model
pred_obj_radial <- prediction(as.numeric(best_radial_pred), as.numeric(true_labels))
pred_obj_linear <- prediction(as.numeric(best_linear_pred), as.numeric(true_labels))
pred_obj_poly <- prediction(as.numeric(best_poly_pred), as.numeric(true_labels))

# Create performance objects for each model
perf_obj_radial <- performance(pred_obj_radial, "tpr", "fpr")
perf_obj_linear <- performance(pred_obj_linear, "tpr", "fpr")
perf_obj_poly <- performance(pred_obj_poly, "tpr", "fpr")

# Plot ROC curve for Radial SVM
plot(perf_obj_radial, col = "blue", lwd = 2, main = "ROC Curve for Radial SVM")
abline(a = 0, b = 1, lty = 2)

# Plot ROC curve for Linear SVM
plot(perf_obj_linear, col = "green", lwd = 2, main = "ROC Curve for Linear SVM")
abline(a = 0, b = 1, lty = 2)

# Plot ROC curve for Polynomial SVM
plot(perf_obj_poly, col = "red", lwd = 2, main = "ROC Curve for Polynomial SVM")
abline(a = 0, b = 1, lty = 2)

# Function to find outliers using the IQR method
find_outliers <- function(data, variable) {
  q1 <- quantile(data[[variable]], 0.25)
  q3 <- quantile(data[[variable]], 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  
  # Return a data frame of outliers
  outliers <- data[data[[variable]] < lower_bound | data[[variable]] > upper_bound, ]
  return(outliers)
}

# Loop through each quantitative variable to find outliers
list_outliers <- lapply(quantitative_vars, function(var) {
  outliers <- find_outliers(dataset, var)
  if (nrow(outliers) > 0) {
    cat("Outliers for", var, ":\n")
    print(head(outliers))
  } else {
    cat("No outliers detected for", var, "\n")
  }
  return(outliers)
})

# Optional: Store all outliers in a list for further examination or processing
outliers_list <- setNames(list_outliers, quantitative_vars)

# Visualize outliers using box plots
for (var in quantitative_vars) {
  # Create box plot for the current quantitative variable including outliers
  boxplot(dataset[[var]], main = paste("Box Plot with Outliers of", var), col = "skyblue", border = "black", outline = TRUE)
}

