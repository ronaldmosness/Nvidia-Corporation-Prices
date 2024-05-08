rm(list = ls()) 
# Load required libraries
library(tidyverse)library(lubridate) 
library(ggplot2) 
library(TTR) 
library(reshape2) 
library(zoo) 
# Load the data from the CSV file, specifying the date format
nvda_data <- read.csv("NVDA.csv", stringsAsFactors = FALSE, colClasses = c("D
ate", rep("numeric", 6)), na.strings = "NA", sep = ",", dec = ".", strip.whit
e = TRUE, quote = "\"", header = TRUE) 
str(nvda_data) 
## 'data.frame': 1258 obs. of 7 variables: 
## $ Date : Date, format: "2018-10-15" "2018-10-16" ... 
## $ Open : num 61.5 60 62.1 61.5 60.4 ... 
## $ High : num 61.5 61.6 62.5 61.9 60.6 ... 
## $ Low : num 58.8 59.5 60.3 59.3 56.9 ... 
## $ Close : num 58.8 61.5 60.8 59.9 57.3 ... 
## $ Adj.Close: num 58.3 60.9 60.2 59.4 56.8 ... 
## $ Volume : num 44976000 40871200 32966800 52402000 61360800 ... 
summary(nvda_data) 
## Date Open High Low 
## Min. :2018-10-15 Min. : 31.62 Min. : 32.49 Min. : 31.11 
## 1st Qu.:2020-01-15 1st Qu.: 59.96 1st Qu.: 60.58 1st Qu.: 59.22 
## Median :2021-04-15 Median :137.72 Median :139.46 Median :134.60 
## Mean :2021-04-14 Mean :159.64 Mean :162.70 Mean :156.49 
## 3rd Qu.:2022-07-14 3rd Qu.:216.18 3rd Qu.:221.34 3rd Qu.:211.43 
## Max. :2023-10-13 Max. :502.16 Max. :502.66 Max. :489.58 
## Close Adj.Close Volume 
## Min. : 31.77 Min. : 31.53 Min. : 9788400 
## 1st Qu.: 60.00 1st Qu.: 59.77 1st Qu.: 32658800 
## Median :136.89 Median :136.62 Median : 44460400 
## Mean :159.72 Mean :159.50 Mean : 47395955 
## 3rd Qu.:217.37 3rd Qu.:217.12 3rd Qu.: 57866450 
## Max. :493.55 Max. :493.51 Max. :251152800 
#current trend plot
ggplot(data = nvda_data, aes(x = Date, y = `Adj.Close`)) +
 geom_line() +
 labs(title = "NVDA Adjusted Close Prices Over Time", 
 x = "Date", 
 y = "Adjusted Close Price") 
#Price vs. Volume Relationship
ggplot(data = nvda_data, aes(x = `Adj.Close`, y = Volume)) +
 geom_point() +
 labs(title = "NVDA Adjusted Close Price vs. Volume", 
 x = "Adjusted Close Price", 
 y = "Volume") 
#correlation matrx
cor_matrix <- cor(select(nvda_data, -Date)) 
cor_matrix 
## Open High Low Close Adj.Close 
## Open 1.00000000 0.99958210 0.99955255 0.99893598 0.99893428 
## High 0.99958210 1.00000000 0.99949370 0.99953047 0.99952831 
## Low 0.99955255 0.99949370 1.00000000 0.99953658 0.99953775 
## Close 0.99893598 0.99953047 0.99953658 1.00000000 0.99999960 
## Adj.Close 0.99893428 0.99952831 0.99953775 0.99999960 1.00000000 
## Volume -0.02737353 -0.01873219 -0.03489713 -0.02668293 -0.02663659 
## Volume 
## Open -0.02737353 
## High -0.01873219 
## Low -0.03489713 
## Close -0.02668293 
## Adj.Close -0.02663659 
## Volume 1.00000000 
# Calculate rolling mean and standard deviation
nvda_data <- nvda_data %>%
 mutate(rolling_mean = SMA(Adj.Close, n = 20), # 20-day rolling mean
 rolling_sd = rollapply(Adj.Close, width = 20, FUN = sd, fill = NA)) 
# 20-day rolling standard deviation
# Plot the rolling statistics with updated aesthetics
ggplot(data = nvda_data, aes(x = Date)) +
 geom_line(aes(y = Adj.Close), color = "blue", size = 1, show.legend = FALSE
) +
 geom_line(aes(y = rolling_mean), color = "red", size = 1, linetype = "dashe
d", show.legend = FALSE) +
 geom_ribbon(aes(ymin = rolling_mean - rolling_sd, ymax = rolling_mean + rol
ling_sd), fill = "gray", alpha = 0.3, show.legend = FALSE) +
 labs(title = "NVDA Adjusted Close Price with Rolling Mean and Standard Devi
ation", 
 x = "Date", 
 y = "Price") +
 theme_minimal() +
 theme(legend.position = "none") +
 scale_size_identity() 
## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0. 
## ℹ Please use `linewidth` instead. 
## This warning is displayed once every 8 hours. 
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was 
## generated. 
## Warning: Removed 19 rows containing missing values (`geom_line()`). 
#### END OF EDA
#prediction tests now
# Install and load required packages
# Load required packages
library(prophet) 
## Loading required package: Rcpp 
## Loading required package: rlang 
## 
## Attaching package: 'rlang' 
## 
## The following objects are masked from 'package:purrr': 
## 
## %@%, flatten, flatten_chr, flatten_dbl, flatten_int, flatten_lgl, 
## flatten_raw, invoke, splice 
# Load data from CSV file
stock_data <- read.csv("NVDA.csv") 
# Convert the 'Date' column to Date data type
stock_data$Date <- as.Date(stock_data$Date, format = "%Y-%m-%d") 
# Prepare data for Prophet
prophet_data <- data.frame(ds = stock_data$Date, y = stock_data$Adj.Close) 
# Create and fit Prophet model
prophet_model <- prophet(prophet_data, daily.seasonality = TRUE) 
# Make a dataframe for future dates
future <- make_future_dataframe(prophet_model, periods = 75) # Adjust the nu
mber of periods as needed
# Predict values
forecast <- predict(prophet_model, future) 
# Plot the forecast
plot(prophet_model, forecast) 
# Plot the forecast with customized labels and colors
ggplot() +
 geom_line(data = stock_data, aes(x = Date, y = Adj.Close), color = "blue") 
+
 geom_ribbon(data = forecast, aes(x = as.Date(ds), ymin = as.numeric(yhat_lo
wer), ymax = as.numeric(yhat_upper)), fill = "lightblue", alpha = 0.5) +
 geom_line(data = forecast, aes(x = as.Date(ds), y = as.numeric(yhat)), colo
r = "red") +
 labs(x = "Date", y = "Adj.Close Price", title = "Prophet Forecast of NVIDIA 
Prices till 2024") +
 theme_minimal() 
# Extract predicted value for a specific date
specific_date <- subset(forecast, as.Date(ds) == as.Date("2023-12-25")) 
predicted_value <- specific_date$yhat 
print(predicted_value) 
## [1] 558.9835 
# Extract predicted values for a specific time period
start_date <- as.Date("2023-12-01") 
end_date <- as.Date("2023-12-15") 
specific_period <- subset(forecast, as.Date(ds) >= start_date & as.Date(ds) <
= end_date) 
predicted_values <- specific_period$yhat 
print(predicted_values) 
## [1] 551.7973 554.4128 554.6942 552.7143 552.9221 553.0145 553.7412 553.24
70 
## [9] 555.6325 555.7343 553.6278 553.7640 553.8403 554.6061 554.2046 
##ML model
library(randomForest) 
library(caret) 
# Calculate moving averages (e.g., 20-day and 50-day)
stock_data$SMA20 <- SMA(stock_data$Adj.Close, n = 20) 
stock_data$SMA50 <- SMA(stock_data$Adj.Close, n = 50) 
# Calculate Relative Strength Index (RSI, period = 14)
delta <- c(NA, diff(stock_data$Adj.Close)) 
gain <- ifelse(delta > 0, delta, 0) 
loss <- ifelse(delta < 0, abs(delta), 0) 
avg_gain <- SMA(gain, n = 14, na.rm = TRUE) 
avg_loss <- SMA(loss, n = 14, na.rm = TRUE) 
rs <- avg_gain / avg_loss 
stock_data$RSI <- 100 - (100 / (1 + rs)) 
stock_data$RSI[is.na(stock_data$RSI)] <- 0 # Replace NA values with 0 for th
e first 14 days (RSI calculation period)
# Calculate Bollinger Bands
stock_data$SMA20 <- SMA(stock_data$Adj.Close, n = 20) 
stock_data$SD20 <- rollapply(stock_data$Adj.Close, width = 20, FUN = sd, fill 
= NA, align = "right") 
stock_data$UpperBB <- stock_data$SMA20 + 2 * stock_data$SD20 
stock_data$LowerBB <- stock_data$SMA20 - 2 * stock_data$SD20 
# Prepare features and target variable
features <- c("SMA20", "SMA50", "RSI", "UpperBB", "LowerBB") 
target <- "Adj.Close"
# Create a data frame with selected features
selected_data <- stock_data[c(features, target)] 
# Split the data into training and testing sets (80% training, 20% testing)
set.seed(123) # for reproducibility
train_index <- createDataPartition(selected_data$Adj.Close, p = 0.8, list = F
ALSE) 
train_data <- selected_data[train_index, ] 
test_data <- selected_data[-train_index, ] 
train_data <- na.omit(train_data) 
test_data <- na.omit(test_data) 
# Train Random Forest model
rf_model <- randomForest(Adj.Close ~ ., data = train_data) 
# Make predictions
predictions <- predict(rf_model, test_data) 
# Evaluate the model (Mean Absolute Error)
mae <- caret::MAE(predictions, test_data$Adj.Close) 
print(paste("Mean Absolute Error (MAE):", mae)) 
## [1] "Mean Absolute Error (MAE): 3.98672040610765" 
varImpPlot(rf_model)