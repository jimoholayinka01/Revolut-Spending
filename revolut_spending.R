
library(forecast)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyverse)
library(mice)
library(tsoutliers)
library(tsibble)
library(zoo)
library(nortest)
library(MASS)
library(dplyr)
library(tseries)
library(readr)
library(lmtest)
library(vars)
library(Metrics)
library(lpSolve)

# For time series forecasting models such as ARIMA, ETS, and generating forecasts
library(forecast)      
# For advanced data visualisation (e.g., plotting time series, trends)
library(ggplot2)       
# For data manipulation (filtering, grouping, summarising, etc.)
library(dplyr)         
# For reading tabular data (like CSV files) into R
library(readr)         
# Provides a complete set of tools for data science workflows
library(tidyverse)     
# For handling missing data using methods like multiple imputation
library(mice)          
# For detecting and handling outliers in time series data
library(tsoutliers)    
# Provides a tidy data structure for time series and temporal data analysis
library(tsibble)       
# For creating and manipulating ordered time series data using irregular or regular time indices
library(zoo)           
# For performing normality tests (e.g., Anderson-Darling test)
library(nortest)       
# Provides statistical functions and datasets; includes functions like `stepAIC` for model selection
library(MASS)          
# For performing statistical tests like the Augmented Dickey-Fuller test (ADF)
library(tseries)       
# For performing hypothesis tests on linear models (e.g., Granger causality)
library(lmtest)        
# For estimating and analysing Vector Auto Regression (VAR) models
library(vars)          
# Provides evaluation metrics like MAE, RMSE, and MAPE for model performance assessment
library(Metrics)       

spending <- read_csv("spending_datasets.csv")
View(spending)

spending_2 <- read_csv("spending_datasets2.csv", 
                       col_types = cols(Total = col_number(), 
                                        `18-34` = col_number(), `35-54` = col_number(), 
                                        `55+` = col_number()))
View(spending_2)

#Merging the two dataset
spend_data <- merge(spending_2, spending, by = "Date")
print(spend_data)

#colnames(spend_data)
spend_data <- spend_data[, -c(7,8)]
colnames(spend_data)

# Print the updated data frame
print(spend_data)


#Structure of the Spending  dataset 
str(spend_data)

#Setting the date column as date type
spend_data$Date <- as.Date(spend_data$Date, format = "%d %b %Y")
str(spend_data)

#Setting the Date column as the index
rownames(spend_data) <- spend_data$Date

print(spend_data)

#Creation of in-store column from online column provided in the dataset
spend_data$in_store <- 100 - spend_data$Online
print(spend_data)

#Renaming the Columns name
colnames(spend_data) <- c("Date", "Total", "Age_18_34", "Age_35_54", "Age_55+", "Online", "In_store")
print(spend_data)


#Checking for Nan, Null and Na 
sum(is.na(spend_data))

#Displaying missing data
md.pattern(spend_data)

# Using Mice library to fill na to interpolate
imp_data <- mice(spend_data, m = 5, method = 'pmm', seed = 500)

# Complete data
complete_data <- complete(imp_data, 1)
print(complete_data)

#Checking for Nan, Null and Na 
sum(is.na(complete_data))

#Check and removal of duplicates
complete_data <- complete_data[!duplicated(complete_data), ]


# Creating a complete sequence of expected daily dates
expected_dates <- seq(min(spend_data$Date), max(spend_data$Date), by = "day")

# Find missing dates
missing_dates <- setdiff(expected_dates, spend_data$Date)

# Output result
if (length(missing_dates) == 0) {
  cat("No missing dates.\n")
} else {
  cat("Missing dates found:\n")
  print(missing_dates)
}



#Identifying the outlier in the Total column
boxplot(complete_data$Total)
summary(complete_data)

# Calculate IQR and outlier thresholds
Q1 <- quantile(complete_data$Total, 0.25, na.rm = TRUE)
Q3 <- quantile(complete_data$Total, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1

# Define outlier bounds
lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

# View thresholds
cat("Lower Bound:", lower_bound, "\nUpper Bound:", upper_bound, "\n")

# Replace low outliers (below lower_bound)
complete_data$Total[complete_data$Total < lower_bound] <- round(Q1, 2)

# Replace high outliers (above upper_bound)
complete_data$Total[complete_data$Total > upper_bound] <- round(Q3, 2)


boxplot(complete_data[ , !(names(complete_data) %in% c("Date", "Online", "In_store"))])



str(complete_data)



# 1. Line plot of all categories over time
ggplot(complete_data, aes(x = Date, y = Total)) +
  geom_line(size = 1) +
  labs(title = "Spending Trends Over Time of the Total spending amount", x = "Month", y = "Spending Amount") +
  theme_minimal()
# 1. Line plot of all categories over time
ggplot(complete_data, aes(x = Date, y = Age_18_34)) +
  geom_line(size = 1) +
  labs(title = "Spending Trends Over Time of Age 18-34", x = "Month", y = "Spending Amount") +
  theme_minimal()
# 1. Line plot of all categories over time of Age 35-54
ggplot(complete_data, aes(x = Date, y = Age_35_54)) +
  geom_line(size = 1) +
  labs(title = "Spending Trends Over Time of Age 35-54", x = "Month", y = "Spending Amount") +
  theme_minimal()
# 1. Line plot of all categories over time of Age 55+
ggplot(complete_data, aes(x = Date, y = `Age_55+`)) +
  geom_line(size = 1) +
  labs(title = "Spending Trends Over Time of Age 55+", x = "Month", y = "Spending Amount") +
  theme_minimal()

# Reshape data to long format for easier plotting
spend_long <- complete_data %>%
  pivot_longer(cols = -Date, names_to = "Category", values_to = "Amount")
ggplot(spend_long, aes(x = Date, y = Amount, color = Category)) +
  geom_line(size = 1) +
  labs(title = "Spending Trends Over Time", x = "Month", y = "Spending Amount") +
  theme_minimal()

# 2. Distribution plots of each category
ggplot(spend_long, aes(x = Amount, fill = Category)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  facet_wrap(~ Category, scales = "free") +
  labs(title = "Distribution of Spending by Category") +
  theme_minimal()

# Loop through each unique category and plot its histogram separately
unique_categories <- unique(spend_long$Category)

for (cat in unique_categories) {
  p <- ggplot(subset(spend_long, Category == cat), aes(x = Amount, fill = Category)) +
    geom_histogram(bins = 30, alpha = 0.7, color = "black") +
    labs(title = paste("Distribution of Spending -", cat),
         x = "Amount", y = "Frequency") +
    theme_minimal() +
    theme(legend.position = "none")
  
  print(p)
}


# 3. Boxplot of spending by category
ggplot(spend_long, aes(x = Category, y = Amount, fill = Category)) +
  geom_boxplot() +
  labs(title = "Boxplot of Spending Amounts by Category", x = "", y = "Amount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convert daily data to monthly by taking the mean of each numeric column
month_data <- complete_data %>%
  mutate(YearMonth = floor_date(Date, unit = "month")) %>%
  group_by(YearMonth) %>%
  summarise(across(c(Total, Age_18_34, Age_35_54, `Age_55+`, Online, In_store), mean, na.rm = TRUE)) %>%
  ungroup()
month_data$YearMonth <- format(month_data$YearMonth, "%Y-%m")

print(month_data)


#Creating a timeseries object
spend_ts <- ts(month_data[ , -which(names(spend_data) == "Date")], frequency = 12)
#spend_ts <- ts(complete_data[ , -which(names(spend_data) == "Date")], frequency = 365)


head(spend_ts)

str(spend_ts)

#Check the property of each column
sapply(spend_ts, class)


#Plotting time series 
plot(spend_ts)         
#plot part of base R 
autoplot(spend_ts)     #autoplot part of ggplot2 


# Perform the Shapiro-Wilk test for normality
shapiro_test_total <- shapiro.test(month_data$Total)
shapiro_test_18_34 <- shapiro.test(month_data$`Age_18_34`)
shapiro_test_35_54 <- shapiro.test(month_data$`Age_35_54`)
shapiro_test_55 <- shapiro.test(month_data$`Age_55+`)
print(shapiro_test_total)
print(shapiro_test_18_34)
print(shapiro_test_35_54)
print(shapiro_test_55)


#Another test for normality
Anderson_test_total <- ad.test(spend_ts)
print(Anderson_test_total)

# Applying log transformation to transform the dataset
log_spend_ts <- log(spend_ts)

# Plot the original and log-transformed data
par(mfrow = c(1, 2))
plot(spend_ts, main = "Original Time Series", ylab = "Value", xlab = "Time")
plot(log_spend_ts, main = "Log-Transformed Time Series", ylab = "Log(Value)", xlab = "Time")

Anderson_test_total <- ad.test(log_spend_ts)
print(Anderson_test_total)


# Apply Box-Cox transformation
boxcox_result <- boxcox(spend_ts ~ 1, lambda = seq(-2, 2, by = 0.1))

# Find the lambda that maximizes the log-likelihood
best_lambda <- boxcox_result$x[which.max(boxcox_result$y)]

# Transform the data using the best lambda
boxcox_transformed <- (spend_ts^best_lambda - 1) / best_lambda

# Plot original vs. Box-Cox transformed data
par(mfrow = c(1, 2))
plot(spend_ts, main = "Original Time Series", ylab = "Value", xlab = "Time")
plot(boxcox_transformed, main = "Box-Cox Transformed Time Series", ylab = "Transformed Value", xlab = "Time")

Anderson_total <- ad.test(boxcox_transformed)
print(Anderson_total)

autoplot(log_spend_ts) +
  ggtitle("Actual Monthly Debit Card Spending Index") +
  ylab("Spending Index") +
  xlab("Time")

# Additive decomposition because it is flunctuates because the difference are not much
decomp_total <- decompose(log_spend_ts[, "Total"], type = "additive")
decomp_18_34 <- decompose(log_spend_ts[, "Age_18_34"], type = "additive")
decomp_35_54 <- decompose(log_spend_ts[, "Age_35_54"], type = "additive")
decomp_55 <- decompose(log_spend_ts[, "Age_55+"], type = "additive")
decomp_online <- decompose(log_spend_ts[, "Online"], type = "additive")
decomp_in_store <- decompose(log_spend_ts[, "In_store"], type = "additive")



options(repr.plot.width = 15, repr.plot.height = 12)

autoplot(decomp_total) +
  ggtitle("Seasonal Decomposition of Total Debit Card Spending (additive)")
autoplot(decomp_18_34) +
  ggtitle("Seasonal Decomposition of Age 18-34 Debit Card Spending (additive)")
autoplot(decomp_35_54) +
  ggtitle("Seasonal Decomposition of Age 35-54 Debit Card Spending (additive)")
autoplot(decomp_55) +
  ggtitle("Seasonal Decomposition of Age 55+ Debit Card Spending (additive)")
autoplot(decomp_online) +
  ggtitle("Seasonal Decomposition of Online Debit Card Spending (additive)")
autoplot(decomp_in_store) +
  ggtitle("Seasonal Decomposition of In_store Debit Card Spending (additive)")


# Run ADF Test
adf.test(log_spend_ts[, "Total"])
adf.test(log_spend_ts[, "Age_18_34"])
adf.test(log_spend_ts[, "Age_35_54"])
adf.test(log_spend_ts[, "Age_55+"])
adf.test(log_spend_ts[, "Online"])
adf.test(log_spend_ts[, "In_store"])

# Plot ACF For Total Debit Card
acf(log_spend_ts[, "Total"], main = "ACF Plot of Total Spending of Debit Card")
# Plot PACF For Total Debit Card 
pacf(log_spend_ts[, "Total"], main = "PACF Plot of TotalSpending of Debit card")

# Plot ACF For Age 18-34
acf(log_spend_ts[, "Age_18_34"], main = "ACF Plot of Debit Card Spending Between Age 18-34")
# Plot PACF For 18-34
pacf(log_spend_ts[, "Age_18_34"], main = "PACF Plot of Debit Card Spending Between Age 18-34")

# Plot ACF For Age 35-54
acf(log_spend_ts[, "Age_35_54"], main = "ACF Plot of Debit Card Spending Between Age 35-54")
# Plot PACF For 35-34
pacf(log_spend_ts[, "Age_35_54"], main = "PACF Plot of Debit Card Spending Between Age 35-34")

# Plot ACF For 55+
acf(log_spend_ts[, "Age_55+"], main = "ACF Plot of Debit Card Spending Between Age 55+")
# Plot PACF For 55+
pacf(log_spend_ts[, "Age_55+"], main = "PACF Plot of Debit Card Spending Between Age 55+")


# Run Ljung-Box test for Autocorrelation
Box.test(log_spend_ts[, "Total"], lag = 20, type = "Ljung-Box")
Box.test(log_spend_ts[, "Age_18_34"], lag = 20, type = "Ljung-Box")
Box.test(log_spend_ts[, "Age_35_54"], lag = 20, type = "Ljung-Box")
Box.test(log_spend_ts[, "Age_55+"], lag = 20, type = "Ljung-Box")



#  Determine Differencing Required
d_value <- ndiffs(log_spend_ts)  # Regular differencing
print(d_value)

# Apply Differencing if Needed (to make data stationary)
if (d_value > 0) {
  ts_data <- diff(log_spend_ts, differences=d_value)
}

#  Determine Differencing Required
d_value <- ndiffs(ts_data)  # Regular differencing
print(d_value)

autoplot(ts_data)

# Convert YearMonth (e.g., "2024-01") to Date (e.g., "2024-01-01")
month_data$YearMonth <- as.Date(paste0(month_data$YearMonth, "-01"))

# Check structure before creating zoo
str(month_data)

# Create zoo object with proper ordering by date
ts_zoo <- zoo(month_data[, -1], order.by = month_data$YearMonth)

# Confirm number of time points (rows)
cat("Number of time points:", nrow(ts_zoo), "\n")
print(tail(ts_zoo))


# Prepare Time Series from zoo object 
#
total_ts <- ts(ts_zoo$Total, start = c(2020, 1), frequency = 12)

# 2. Split into Train and Test
train_length <- length(total_ts) - 12  # Last 12 months as test
train_ts <- window(total_ts, end = c(2020 + (train_length - 1) %/% 12, (train_length - 1) %% 12 + 1))
test_ts <- window(total_ts, start = c(2020 + train_length %/% 12, train_length %% 12 + 1))


# Checking the value of Regular differencing for train dataset
d_value <- ndiffs(train_ts) 
print(d_value)

#Check Stationarity
acf(train_ts, main = "ACF of Training Data")
pacf(train_ts, main = "PACF of Training Data")

# Build SARIMA model

sarima_model <- Arima(train_ts, order = c(1,1,1), seasonal = list(order= c(1,1,0), period=12))
summary(sarima_model)

# Forecast
forecast_horizon <- length(test_ts)
sarima_forecast <- forecast(sarima_model, h = forecast_horizon)

# Accuracy Evaluation
summary(sarima_forecast, test_ts)


# Plot Forecast vs Actual
autoplot(sarima_forecast) +
  autolayer(test_ts, series = "Actual", color = "red") +
  ggtitle("SARIMA Forecast vs Actual") +
  ylab("Debit Card Spending (Index)") +
  xlab("Time") +
  theme_minimal()

#USing the Granger Casuality test to identify the exogenous variable that has the most effect on the target Total variable
grangertest(Total ~ Age_18_34, order = 5, data = month_data)
grangertest(Total ~ Age_35_54, order = 5, data = month_data)
grangertest(Total ~ `Age_55+`, order = 5, data = month_data)

#Preparing data for SARIMAX Model
month_data$YearMonth <- as.Date(paste0(month_data$YearMonth, "-01"))
ts_zoo <- zoo(month_data[, -1], order.by = month_data$YearMonth)
total_ts <- ts(ts_zoo$Total, frequency = 12, start = c(2020, 1))
age_18_34_ts <- ts(ts_zoo$Age_18_34, frequency = 12, start = c(2020, 1))
n <- length(total_ts)
train_size <- floor(0.8 * n)
train_total <- window(total_ts, end = c(2020 + (train_size - 1) %/% 12, (train_size - 1) %% 12 + 1))
test_total <- window(total_ts, start = c(2020 + train_size %/% 12, train_size %% 12 + 1))
train_exog <- window(age_18_34_ts, end = c(2020 + (train_size - 1) %/% 12, (train_size - 1) %% 12 + 1))
test_exog <- window(age_18_34_ts, start = c(2020 + train_size %/% 12, train_size %% 12 + 1))


# Fit SARIMAX model 
#sarimax_model <- Arima(train_total, order = c(1, 1, 1), seasonal = c(1, 1, 0), xreg = train_exog)
sarimax_model <-  Arima(train_total, order = c(1,1,1), seasonal = list(order= c(1,1,0), period=12), xreg = train_exog)

#  Forecast using the model and test exogenous variable
sarimax_forecast <- forecast(sarimax_model, xreg = test_exog, h = length(test_exog))

# Accuracy metrics
summary(sarimax_forecast, test_total)

#  Plot forecast vs actual
autoplot(sarimax_forecast) +
  autolayer(test_total, series = "Actual", color = "red") +
  ggtitle("SARIMAX Forecast vs Actual (with Age_18_34)") +
  ylab("Total Debit Card Spending") +
  xlab("Time") +
  theme_minimal()

#Pre-processing the data
#Convert to columns time series format
ts_data <- ts(ts_zoo[, c("Total", "Age_18_34")], frequency = 12, start = c(2020, 1))

n <- nrow(ts_data)
train_size <- floor(0.8 * n)

var_train_data <- ts_data[1:train_size, ]
var_test_data <- ts_data[(train_size + 1):n, ]


# Fit VAR model (select lag order)
lag_selection <- VARselect(var_train_data, lag.max = 12, type = "const")
best_lag <- lag_selection$selection["AIC(n)"]

var_model <- VAR(var_train_data, p = best_lag, type = "const")


# Forecast
forecast_var <- predict(var_model, n.ahead = nrow(test_data))

# Extract predictions for 'Total'
predicted_total <- forecast_var$fcst$Total[, "fcst"]
actual_total <- var_test_data[, "Total"]

# Plot impulse response functions (IRF)
irf_plot <- irf(var_model, impulse = "Age_18_34", response = "Total", n.ahead = 10, boot = TRUE)
plot(irf_plot)

# Create date sequence
dates <- time(ts(var_test_data[, "Total"], start = c(2020 + train_size %/% 12, (train_size %% 12) + 1), frequency = 12))

# Creating dataframe and plot
df_plot <- data.frame(
  Date = as.Date(as.yearmon(dates)),
  Predicted = predicted_total,
  Actual = actual_total
)

ggplot(df_plot, aes(x = Date)) +
  geom_line(aes(y = Actual, colour = "Actual")) +
  geom_line(aes(y = Predicted, colour = "Predicted")) +
  labs(
    title = "VAR Forecast vs Actual (with Age_18_34)",
    x = "Time",
    y = "Total Debit Card Spending",
    colour = "Legend"
  ) +
  theme_minimal()



# Step 4: Fit VAR model (select lag order)
lag_selection <- VARselect(train_data, lag.max = 12, type = "const")
best_lag <- lag_selection$selection["AIC(n)"]

var_model <- VAR(train_data, p = best_lag, type = "const")

# Step 5: Forecast
forecast_var <- predict(var_model, n.ahead = nrow(test_data))

# Step 6: Extract predictions for 'Total'
predicted_total <- forecast_var$fcst$Total[, "fcst"]
actual_total <- test_data[, "Total"]

# Step 7: Create date sequence
dates <- time(ts(test_data[, "Total"], start = c(2020 + train_size %/% 12, (train_size %% 12) + 1), frequency = 12))

# Creating dataframe and plot
df_plot <- data.frame(
  Date = as.Date(as.yearmon(dates)),
  Predicted = predicted_total,
  Actual = actual_total
)

ggplot(df_plot, aes(x = Date)) +
  geom_line(aes(y = Actual, colour = "Actual")) +
  geom_line(aes(y = Predicted, colour = "Predicted")) +
  labs(
    title = "VAR Forecast vs Actual (with Age_18_34)",
    x = "Time",
    y = "Total Debit Card Spending",
    colour = "Legend"
  ) +
  theme_minimal()

# Step 9: Accuracy
rmse_score <- rmse(actual_total, predicted_total)
mae_score <- mae(actual_total, predicted_total)

cat("RMSE:", round(rmse_score, 2), "\n")
cat("MAE:", round(mae_score, 2), "\n")


#  Create a dataframe with predicted and actual values
forecast_total <- forecast_var$fcst$Total[, "fcst"]
print(forecast_var)

#Exponential Smoothing State Space Model
# Fit the ETS model
ets_model <- ets(train_ts[, "Total"])


# Forecast for the next 180 days using the test dataset
ets_forecast <- forecast(ets_model, h = h)



# Step 1: Convert YearMonth to Date and create zoo object
month_data$YearMonth <- as.Date(paste0(month_data$YearMonth, "-01"))
ts_zoo <- zoo(month_data[, -1], order.by = month_data$YearMonth)

# Step 2: Create a time series object for 'Total'
total_ts <- ts(ts_zoo$Total, frequency = 12, start = c(2020, 1))

# Step 3: Split into training and testing
n <- length(total_ts)
train_size <- floor(0.8 * n)

train_total <- window(total_ts, end = c(2020 + (train_size - 1) %/% 12, (train_size - 1) %% 12 + 1))
test_total <- window(total_ts, start = c(2020 + train_size %/% 12, train_size %% 12 + 1))

# Fitting the ETS model
ets_model <- ets(train_total)

#  Forecasting with the ETS Model
ets_forecast <- forecast(ets_model, h = length(test_total))

# Plotting forecast vs actual
autoplot(ets_forecast) +
  autolayer(test_total, series = "Actual", color = "red") +
  labs(
    title = "ETS Forecast vs Actual",
    y = "Total Debit Card Spending",
    x = "Time"
  ) +
  theme_minimal()

# Evaluate forecast
rmse_score <- rmse(as.numeric(test_total), as.numeric(ets_forecast$mean))
mae_score <- mae(as.numeric(test_total), as.numeric(ets_forecast$mean))

cat("RMSE:", round(rmse_score, 2), "\n")
cat("MAE:", round(mae_score, 2), "\n")


summary(ets_forecast)

autoplot(ets_forecast) + ggtitle("ETS Forecast of Total Spending")

# Converting the train to zoo object again for further extraction
ts_data <- data.frame(
  Date = index(train_ts),
  Total = coredata(train_ts)[, "Total"]
)

# Creating a time index
ts_data$time_index <- 1:nrow(ts_data)

# Fit the linear model
lm_model <- lm(Total ~ time_index, data = ts_data)

# Summary of the Lm_model
summary(lm_model)


# Step 5: Predict values
lm_predict <- predict(lm_model)

# Step 6: Calculate error metrics
mae_val <- mae(ts_data$Total, lm_predict)
rmse_val <- rmse(ts_data$Total, lm_predict)
mape_val <- mape(ts_data$Total, lm_predict) * 100

cat("Linear Model Accuracy:\n")
cat("MAE:", mae_val, "\nRMSE:", rmse_val, "\nMAPE:", mape_val, "%\n")

# Step 7: Plot actual vs predicted
ggplot(ts_data, aes(x = Date)) +
  geom_line(aes(y = Total), color = "blue", size = 1.2, linetype = "solid") +
  geom_line(aes(y = Predicted), color = "red", size = 1.2, linetype = "dashed") +
  labs(title = "Linear Trend Model: Actual vs Predicted",
       y = "Total Spending", x = "Date") +
  theme_minimal()

ts_data$predicted <- predict(lm_model, newdata = test_ts)


# plot actual vs predicted
library(ggplot2)
ggplot(ts_data, aes(x = Date)) +
  geom_line(aes(y = Total), color = "blue") +
  geom_line(aes(y = predicted), color = "red", linetype = "dashed") +
  labs(title = "Linear Model on zoo Time Series",
       y = "Total", x = "Date",
       caption = "Blue = Actual, Red = Predicted") +
  theme_minimal()



# Assuming 'month_data' is already cleaned and loaded
# Ensure YearMonth is in Date format
month_data$YearMonth <- as.Date(paste0(month_data$YearMonth, "-01"))

# Split the data into train (80%) and test (20%)
n <- nrow(month_data)
train_size <- floor(0.8 * n)

train_data <- month_data[1:train_size, ]
test_data <- month_data[(train_size + 1):n, ]

# Build linear regression model
lm_model <- lm(Total ~ Age_18_34, data = train_data)

# Make predictions
lm_predictions <- predict(lm_model, newdata = test_data)

# Evaluate performance
mae <- mean(abs(lm_predictions - test_data$Total))
rmse <- sqrt(mean((lm_predictions - test_data$Total)^2))

cat("MAE:", mae, "\n")
cat("RMSE:", rmse, "\n")

# Plot actual vs predicted
df_plot <- data.frame(
  Date = test_data$YearMonth,
  Actual = test_data$Total,
  Predicted = lm_predictions
)

ggplot(df_plot, aes(x = Date)) +
  geom_line(aes(y = Actual, colour = "Actual")) +
  geom_line(aes(y = Predicted, colour = "Predicted")) +
  scale_colour_manual(values = c("Actual" = "red", "Predicted" = "blue")) +
  labs(title = "Linear Regression: Actual vs Predicted",
       y = "Total Debit Card Spending",
       x = "Time") +
  theme_minimal()


# Convert test set to ts object
test_ts_fixed <- ts(test_ts$Total, 
                    start = c(2024, as.numeric(format(index(test_ts)[1], "%j"))), 
                    frequency = 365)
h <- length(test_ts_fixed)

# ---------- 1. ARIMA ----------
arima_model <- auto.arima(train_ts$Total)
fc_arima <- forecast(arima_model, h = h)

fc_arimax <- 
  
  # ---------- 2. ETS ----------
ets_model <- ets(train_ts$Total)
fc_ets <- forecast(ets_model, h = h)

# ---------- 3. Linear Trend Model ----------
train_df <- data.frame(
  Date = index(train_ts),
  Total = coredata(train_ts)[, "Total"]
)
train_df$time_index <- 1:nrow(train_df)
lm_model <- lm(Total ~ time_index, data = train_df)
future_index <- (nrow(train_df) + 1):(nrow(train_df) + h)
fc_lm <- predict(lm_model, newdata = data.frame(time_index = future_index))

# ---------- 4. Seasonal Naive ----------
snaive_model <- snaive(train_ts$Total, h = h)

# ---------- 5. Mean Forecast ----------
mean_model <- meanf(train_ts$Total, h = h)

# ---------- Combine predictions ----------
comparison_df <- data.frame(
  Date = time(test_ts_fixed),
  Actual = as.numeric(test_ts_fixed),
  ARIMA = as.numeric(fc_arima$mean),
  ARIMAX = as.numeric(forecast_arimax$mean),
  ETS = as.numeric(fc_ets$mean),
  Linear = as.numeric(fc_lm),
  SNaive = as.numeric(snaive_model$mean),
  Mean = as.numeric(mean_model$mean)
)

# ---------- Error Metrics ----------
models <- c("ARIMA", "ETS", "Linear", "SNaive", "Mean")
metrics <- data.frame(Model = character(), MAE = numeric(), RMSE = numeric(), MAPE = numeric())

for (m in models) {
  mae_val <- mae(comparison_df$Actual, comparison_df[[m]])
  rmse_val <- rmse(comparison_df$Actual, comparison_df[[m]])
  mape_val <- mape(comparison_df$Actual, comparison_df[[m]]) * 100
  metrics <- rbind(metrics, data.frame(Model = m, MAE = mae_val, RMSE = rmse_val, MAPE = mape_val))
}

print(metrics)

# ---------- Plot ----------
library(reshape2)
plot_df <- melt(comparison_df, id.vars = "Date", variable.name = "Series", value.name = "Value")

ggplot(plot_df, aes(x = Date, y = Value, color = Series, linetype = Series)) +
  geom_line(size = 1.1) +
  scale_color_manual(values = c("Actual" = "black", "ARIMA" = "red", "ETS" = "blue", 
                                "Linear" = "green", "SNaive" = "purple", "Mean" = "orange")) +
  labs(title = "Actual vs Forecasts from Five Models",
       y = "Total Spending", x = "Date") +
  theme_minimal()


# Bar chart for MAE
ggplot(metrics, aes(x = Model, y = MAE, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(MAE, 2)), vjust = -0.5, size = 4) +
  labs(title = "Model Comparison: Mean Absolute Error (MAE)",
       x = "Model", y = "MAE") +
  theme_minimal() +
  theme(legend.position = "none")



# 1. Convert test set to ts object
test_ts_fixed <- ts(test_ts$Total, 
                    start = c(2024, as.numeric(format(index(test_ts)[1], "%j"))), 
                    frequency = 365)
h <- length(test_ts_fixed)

# ---------- 1. ARIMA ----------
arima_model <- auto.arima(train_ts$Total)
fc_arima <- forecast(arima_model, h = h)

# ---------- 2. ARIMAX ----------
# Assume arimax_model is already fitted with external regressors: xreg_train and xreg_test
forecast_arimax <- forecast(arimax_model, h = h, xreg = xreg_test)

# ---------- 3. ETS ----------
ets_model <- ets(train_ts$Total)
fc_ets <- forecast(ets_model, h = h)

# ---------- 4. Linear Trend Model ----------
train_df <- data.frame(
  Date = index(train_ts),
  Total = coredata(train_ts)[, "Total"]
)
train_df$time_index <- 1:nrow(train_df)
lm_model <- lm(Total ~ time_index, data = train_df)
future_index <- (nrow(train_df) + 1):(nrow(train_df) + h)
fc_lm <- predict(lm_model, newdata = data.frame(time_index = future_index))

# ---------- 5. Seasonal Naive ----------
snaive_model <- snaive(train_ts$Total, h = h)

# ---------- 6. Mean Forecast ----------
mean_model <- meanf(train_ts$Total, h = h)

# ---------- 7. VAR ----------
# Assuming VAR was trained with Total and another variable (e.g., Age_18_34)
# Also assume test VAR data is available in `test_var_data` with same structure
forecast_var <- predict(var_model, n.ahead = h)
fc_var <- forecast_var$fcst$Total[, "fcst"]  # Only Total forecasts

# ---------- Combine predictions ----------
comparison_df <- data.frame(
  Date = time(test_ts_fixed),
  Actual = as.numeric(test_ts_fixed),
  SARIMA = as.numeric(fc_arima$mean),
  SARIMAX = as.numeric(forecast_arimax$mean),
  ETS = as.numeric(fc_ets$mean),
  Linear = as.numeric(fc_lm),
  VAR = as.numeric(fc_var)
)

# ---------- Error Metrics ----------
models <- c("ARIMA", "ARIMAX", "ETS", "Linear", "SNaive", "Mean", "VAR")
metrics <- data.frame(Model = character(), MAE = numeric(), RMSE = numeric(), MAPE = numeric())

for (m in models) {
  mae_val <- mae(comparison_df$Actual, comparison_df[[m]])
  rmse_val <- rmse(comparison_df$Actual, comparison_df[[m]])
  mape_val <- mape(comparison_df$Actual, comparison_df[[m]]) * 100
  metrics <- rbind(metrics, data.frame(Model = m, MAE = mae_val, RMSE = rmse_val, MAPE = mape_val))
}

print(metrics)

# ---------- Plot: Actual vs Forecasts ----------
plot_df <- melt(comparison_df, id.vars = "Date", variable.name = "Series", value.name = "Value")

ggplot(plot_df, aes(x = Date, y = Value, color = Series, linetype = Series)) +
  geom_line(size = 1.1) +
  scale_color_manual(values = c("Actual" = "black", "ARIMA" = "red", "ARIMAX" = "brown", 
                                "ETS" = "blue", "Linear" = "green", 
                                "SNaive" = "purple", "Mean" = "orange", "VAR" = "pink")) +
  labs(title = "Actual vs Forecasts from Seven Models",
       y = "Total Spending", x = "Date") +
  theme_minimal()

# ---------- Bar Chart: MAE Comparison ----------
ggplot(metrics, aes(x = reorder(Model, MAE), y = MAE, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(MAE, 2)), vjust = -0.5, size = 4) +
  labs(title = "Model Comparison: Mean Absolute Error (MAE)",
       x = "Model", y = "MAE") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))



# Assume 'month_data' is preloaded and cleaned
# Convert YearMonth to Date format
month_data$YearMonth <- as.Date(paste0(month_data$YearMonth, "-01"))

# Convert to time series
ts_zoo <- zoo(month_data[, -1], order.by = month_data$YearMonth)
total_ts <- ts(ts_zoo$Total, frequency = 12, start = c(2020, 1))
age_18_34_ts <- ts(ts_zoo$Age_18_34, frequency = 12, start = c(2020, 1))

# Split data into training and test sets
n <- length(total_ts)
train_size <- floor(0.8 * n)
train_total <- window(total_ts, end = c(2020 + (train_size - 1) %/% 12, (train_size - 1) %% 12 + 1))
test_total <- window(total_ts, start = c(2020 + train_size %/% 12, train_size %% 12 + 1))
train_exog <- window(age_18_34_ts, end = time(train_total)[length(train_total)])
test_exog <- window(age_18_34_ts, start = time(test_total)[1])

### 1. SARIMA
sarima_model <- Arima(train_total, order = c(1, 0, 1), seasonal = list(order = c(1, 0, 0), period = 12))
sarima_forecast <- forecast(sarima_model, h = length(test_total))
mae_sarima <- mae(test_total, sarima_forecast$mean)

### 2. SARIMAX
sarimax_model <- Arima(train_total, order = c(1, 1, 1), seasonal = c(1, 1, 0), xreg = train_exog)
sarimax_forecast <- forecast(sarimax_model, xreg = test_exog, h = length(test_total))
mae_sarimax <- mae(test_total, sarimax_forecast$mean)

### 3. ETS
ets_model <- ets(train_total)
ets_forecast <- forecast(ets_model, h = length(test_total))
mae_ets <- mae(test_total, ets_forecast$mean)

### 4. VAR
# Combine series into multivariate time series
var_df <- data.frame(
  Total = total_ts,
  Age_18_34 = age_18_34_ts
)
var_train <- var_df[1:train_size, ]
var_test <- var_df[(train_size + 1):n, ]
var_model <- VAR(var_train, p = 5, type = "const")
var_forecast <- predict(var_model, n.ahead = n - train_size)
var_pred <- var_forecast$fcst$Total[, "fcst"]
mae_var <- mae(var_test$Total, var_pred)

### 5. Linear Regression
lm_model <- lm(Total ~ Age_18_34, data = month_data[1:train_size, ])
lm_pred <- predict(lm_model, newdata = month_data[(train_size + 1):n, ])
mae_lm <- mae(test_total, lm_pred)

### Create summary and select best model
mae_scores <- data.frame(
  Model = c("SARIMA", "SARIMAX", "ETS", "VAR", "Linear Regression"),
  MAE = c(mae_sarima, mae_sarimax, mae_ets, mae_var, mae_lm)
)

best_model <- mae_scores[which.min(mae_scores$MAE), ]

# Print best model
print("Best model based on MAE:")
print(best_model)

### Plot MAE values
ggplot(mae_scores, aes(x = reorder(Model, MAE), y = MAE, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6, show.legend = FALSE) +
  labs(title = "Model Comparison using MAE",
       x = "Model",
       y = "Mean Absolute Error (MAE)") +
  theme_minimal() +
  geom_text(aes(label = round(MAE, 2)), vjust = -0.5) +
  scale_fill_brewer(palette = "Set2")

# Check residuals of the SARIMAX model
sarimax_residuals <- residuals(sarimax_model)

# 1. Autocorrelation check (ACF + Ljung-Box test)
par(mfrow = c(1, 2))
acf(sarimax_residuals, main = "ACF of Residuals")
pacf(sarimax_residuals, main = "PACF of Residuals")

# Ljung-Box test
Box.test(sarimax_residuals, lag = 20, type = "Ljung-Box")

# 2. Normality check (Histogram + Q-Q plot)
par(mfrow = c(1, 2))
hist(sarimax_residuals, breaks = 20, main = "Histogram of Residuals", col = "skyblue", xlab = "Residuals")
qqnorm(sarimax_residuals, main = "Q-Q Plot of Residuals")
qqline(sarimax_residuals, col = "red")

# 3. Shapiro-Wilk normality test
shapiro.test(sarimax_residuals)

# Reset plotting layout
par(mfrow = c(1, 1))

# Extract residuals
resid_arimax <- residuals(arimax_model)

# 1. Plot ACF of residuals
acf(resid_arimax, main = "ACF of SARIMAX Residuals")

# 2. Ljung-Box test (for autocorrelation)
Box.test(resid_arimax, lag = 20, type = "Ljung-Box")

# 3. Histogram of residuals (for normality)
hist(resid_arimax, breaks = 30, main = "Histogram of Residuals of SARIMAX Model", xlab = "Residuals")

# 4. Q-Q plot (normality check)
qqnorm(resid_arimax)
qqline(resid_arimax, col = "red", lwd = 2)

# 5. Residuals vs Fitted values plot
plot(fitted(arimax_model), resid_arimax,
     main = "Residuals vs Fitted",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lwd = 2)


#Forcasting with the best Model
# Step 1: Define forecast horizon
h_future <- 180

# Step 2: Prepare future external regressors (xreg_future)
# xreg_future must be a matrix or data frame with 180 rows and same structure as xreg used in training
# For example:
# xreg_future <- future_data[, c("Age_18_34", "Other_Column", ...)]

# Step 3: Forecast with ARIMAX
forecast_arimax_180 <- forecast(arimax_model, h = h_future, xreg = xreg_future)

# Step 4: Plot forecast
autoplot(forecast_arimax_180) + autolayer(test_ts, series = "Actual")
labs(title = "180-Day Forecast using ARIMAX Model",
     x = "Date", y = "Total Spending") +
  theme_minimal()

# Step 4: Plot forecast
autoplot(forecast_arimax_180) + autolayer(test_ts, series = "Actual")+
  labs(title = "180-Day Forecast using ARIMAX Model",
       x = "Date", y = "Total Spending") +
  theme_minimal()


#Combining the training and test actuals ===
actual_total <- ts(c(train_total, test_total), frequency = 12, start = start(train_total))

# Combining training and test exogenous variables ===
full_exog <- ts(c(train_exog, test_exog), frequency = 12, start = start(train_exog))

# Creating the future exogenous variable (assumed to be known or projected) ===

future_exog <- ts(rep(tail(test_exog, 1), 24), frequency = 12, start = end(test_exog) + c(0,1))

# Refiting the SARIMAX model on full data
full_model <- Arima(actual_total, order = c(1, 1, 1),
                    seasonal = c(1, 1, 0), xreg = full_exog)

# Forecasting next 24 months
future_forecast <- forecast(full_model, xreg = future_exog, h = 24)

predicted_vals <- fitted(sarimax_model)

# Time index
actual_dates <- time(actual_total)
forecast_dates <- time(future_forecast$mean)

plot_df <- data.frame(
  Date = c(as.Date(as.yearmon(actual_dates)), 
           as.Date(as.yearmon(forecast_dates))),
  Value = c(as.numeric(actual_total), rep(NA, 24)),
  Type = "Actual"
)
predicted_df <- data.frame(
  Date = as.Date(as.yearmon(time(predicted_vals))),
  Value = as.numeric(predicted_vals),
  Type = "Predicted"
)
forecast_df <- data.frame(
  Date = as.Date(as.yearmon(forecast_dates)),
  Value = as.numeric(future_forecast$mean),
  Type = "Forecast"
)

# Combine all for plot
combined_df <- rbind(plot_df, predicted_df, forecast_df)

# Plot Actual vs Predicted vs Forecast
ggplot(combined_df, aes(x = Date, y = Value, colour = Type)) +
  geom_line(size = 1.1) +
  scale_colour_manual(values = c("Actual" = "black", "Predicted" = "blue", "Forecast" = "darkgreen")) +
  ggtitle("Actual vs Predicted vs 24-Month Forecast (SARIMAX)") +
  ylab("Total Debit Card Spending") +
  xlab("Date") +
  theme_minimal() +
  theme(legend.title = element_blank())

print(forecast_df)


Based on this description a new dataset must be imported.
The monthly series is calculated by aggregating the daily data for each day of the month, rather than the 7-day rolling average of each day. Thus monthly data presented here will not match aggregated 7-day rolling average data in table 1.


revolut_month <- read_csv("spendings_dataset3.csv")
print(revolut_month)

# Clean and convert 'Date' column
# e.g., "Jan 20" -> "01 Jan 2020"
revolut_month$Date <- paste("01", revolut_month$Date)  # add day
revolut_month$Date <- parse_date_time(revolut_month$Date, orders = "d b y")  # parse to proper date

row.names(revolut_month) <- revolut_month$Date
# Check result
str(revolut_month)
head(revolut_month)


# Sort by Date
revolut_month <- revolut_month %>% arrange(Date)

# Create time series
min_date <- min(revolut_month$Date)
monthly_ts <- ts(revolut_month$Total,
                 start = c(year(min_date), month(min_date)),
                 frequency = 12)



# Decompose time series
decomp <- decompose(monthly_ts)
plot(decomp)


# Plot seasonal pattern
seasonality_df <- data.frame(
  Month = month.abb,
  Seasonal = decomp$seasonal[1:12]
)

ggplot(seasonality_df, aes(x = Month, y = Seasonal)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Seasonal Pattern in Monthly Debit Card Spending",
       y = "Seasonal Effect", x = "Month") +
  theme_minimal()

# A Plot to show seasonal pattern with months
seasonality_df <- data.frame(
  Month = month.abb,  # Abbreviated month names
  Seasonal = decomp$seasonal[1:12]
)

# Converting Month to a factor with levels
seasonality_df$Month <- factor(seasonality_df$Month, levels = month.abb)

# Plot
ggplot(seasonality_df, aes(x = Month, y = Seasonal)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Seasonal Pattern in Monthly Debit Card Spending",
       y = "Seasonal Effect", x = "Month") +
  theme_minimal()




