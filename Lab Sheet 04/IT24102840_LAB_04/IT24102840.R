setwd("C:\\Users\\it24102840\\Desktop\\IT24102840_LAB_04")
branch_data <- read.table("Exercise.txt", header = TRUE, sep = ",")

# View the data
head(branch_data)

boxplot(branch_data$Sales, main="Boxplot of Sales", ylab="Sales", col="lightblue")

# Five number summary
fivenum(branch_data$Advertising)

# IQR calculation
IQR(branch_data$Advertising)

# Function to find outliers
find_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)  # First quartile (Q1)
  Q3 <- quantile(x, 0.75)  # Third quartile (Q3)
  iqr <- Q3 - Q1  # Interquartile range
  
  lower <- Q1 - 1.5 * iqr  # Lower bound for outliers
  upper <- Q3 + 1.5 * iqr  # Upper bound for outliers
  
  outliers <- x[x < lower | x > upper]  # Identify outliers
  return(outliers)
}

# Check for outliers in years variable
find_outliers(branch_data$Years)
# Additional analysis (Descriptive Statistics)
mean(branch_data$Years)
median(branch_data$Years)
sd(branch_data$Years)

# Stem and leaf plots
stem(branch_data$Sales)
stem(branch_data$Advertising)
stem(branch_data$Years)

