setwd("C:\\Users\\IT24103893\\Desktop\\IT24103893")
# 1. Import the dataset
branch_data <- read.table("Exercise.txt", header = TRUE, sep = ",")
print("Dataset Imported Successfully")
print(head(branch_data))

# 2. Identify variable types and scales
str(branch_data)

# 3. Boxplot for Sales (Sales_X1)
boxplot(branch_data$Sales_X1, main = "Boxplot of Sales", ylab = "Sales", 
        outline = TRUE, outpch = 8, horizontal = TRUE)

# 4. Five number summary and IQR for Advertising_X2
summary(branch_data$Advertising_X2)
iqr_advertising <- IQR(branch_data$Advertising_X2)
cat("IQR for Advertising:", iqr_advertising, "\n")

# 5. Function to find outliers in a numeric vector
find_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  outliers <- x[x < lower_bound | x > upper_bound]
  return(outliers)
}

outliers_years <- find_outliers(branch_data$Years_X3)
print("Outliers in Years:")
print(outliers_years)
