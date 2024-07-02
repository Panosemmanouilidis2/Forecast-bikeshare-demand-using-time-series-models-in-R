#Project to conduct a time series data analysis to forecast future rental bike demands
# Project to conduct a time series data analysis to forecast future rental bike demands

# STEP 1: Importing Libraries
library(tidyverse)
library(dplyr)
library(lubridate)
library(rmarkdown)
library(pandoc)
library(tinytex)
library(ggplot2)
library(tidyr)
library(forecast)
library(openxlsx)
library(gridExtra)
library(scales)
library(xts)
library(forecast)
library(reshape2)
library(e1071)
library(caret)
library(ggcorrplot)

# Setting the Working Directory
setwd("C:\\Panos\\Time Series Analysis in R\\Forecast Daily Bike Rental")

# STEP 2: Importing Datasets
daily_rental <- read.csv("day.csv")
hourly_rental <- read.csv("hour.csv")

# STEP 3: DATA CLEANSING
# Viewing Datasets to check if and what data cleansing is needed
glimpse(daily_rental)
summary(daily_rental)
glimpse(hourly_rental)
summary(hourly_rental)

# STEP 4: DATA PREPARATION
# Convert date columns to the appropriate date format
daily_rental <- daily_rental %>%
  mutate(Date = as.Date(dteday, format = "%Y-%m-%d")) %>%
  select(-dteday)

hourly_rental <- hourly_rental %>%
  mutate(Date = as.Date(dteday, format = "%Y-%m-%d")) %>%
  select(-dteday)

glimpse(daily_rental)
glimpse(hourly_rental)

# STEP 5: Split the Data before EDA
# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(daily_rental$instant, p = 0.8, list = FALSE)

# Subset the data using the created indices
train_data <- daily_rental[train_index, ]
test_data <- daily_rental[-train_index, ]

# STEP 6: Handle Outliers in Training Data
# Handling outliers for casual users
train_data_remove_casual <- train_data %>%
  filter(abs((casual - mean(casual)) / sd(casual)) <= 3)  # Keep only non-outliers

# Exploratory Data Analysis (EDA)

# Descriptive Statistics
summary(train_data_remove_casual$casual)

# Histogram of casual users
ggplot(train_data_remove_casual, aes(x = casual)) +
  geom_histogram(binwidth = 100, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Casual Users", x = "Casual Users", y = "Frequency")

# Boxplot of casual users
ggplot(train_data_remove_casual, aes(y = casual)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  labs(title = "Boxplot of Casual Users", y = "Casual Users")

# Time series plot for casual users
ggplot(train_data_remove_casual, aes(x = Date, y = casual)) +
  geom_line(color = "green") +
  labs(title = "Time Series of Casual Users", x = "Date", y = "Casual Users")

####The above chart did not show clear results

# Group by the new YearMonth column and calculate the average number of casual users
glimpse(train_data_remove_casual)
monthly_avg_casual <- train_data_remove_casual %>%
  group_by(Date) %>%
  dplyr::summarise(AverageCasualUsers =mean(casual))
# Print the resulting DataFrame
print(monthly_avg_casual)

# Convert YearMonth back to a Date type for proper ordering
monthly_avg_casual$Date <- as.Date(paste0("01-", monthly_avg_casual$Date), format = "%d-%b-%y")

# Order the DataFrame by the Date column
monthly_avg_casual <- monthly_avg_casual %>%
  arrange(Date)

# Create the ggplot with geom_line
# Create the line graph
library(ggplot2)
library(scales)

# Ensure the Date column is in Date format
train_data_remove_casual <- train_data_remove_casual %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

# Create a new column for the formatted date
train_data_remove_casual$Date_MM_YY <- format(train_data_remove_casual$Date, "%m/%y")

# Create the line graph
library(ggplot2)
library(scales)

# Ensure the Date column is in Date format
train_data_remove_casual <- train_data_remove_casual %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

# Create the line graph
ggplot(train_data_remove_casual, aes(x = Date, y = casual, group = 1)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +  # Add a trend line
  theme_minimal() +
  labs(title = "Average Casual Users by Month-Year",
       x = "Month-Year",
       y = "Average Casual Users") +
  theme(panel.background = element_blank()) +
  theme(plot.background = element_blank()) +
  scale_y_continuous(limits = c(0, max(train_data_remove_casual$casual, na.rm = TRUE))) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(y = "none")








# Scatter plot of casual users vs temperature
ggplot(train_data_remove_casual, aes(x = temp, y = casual)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Casual Users vs Temperature", x = "Temperature", y = "Casual Users")


# Ensure the Date column in train_data_remove_casual is correctly formatted
train_data_remove_casual <- train_data_remove_casual %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

# Extract month and convert to factor
train_data_remove_casual <- train_data_remove_casual %>%
  mutate(month = factor(month(Date, label = TRUE), levels = month.abb))

# Plotting the corrected monthly distribution of casual users
ggplot(train_data_remove_casual, aes(x = month, y = casual)) +
  geom_boxplot(fill = "cyan", alpha = 0.7) +
  labs(title = "Monthly Distribution of Casual Users", x = "Month", y = "Casual Users")

# Extract day of the week
train_data_remove_casual <- train_data_remove_casual %>%
  mutate(day_of_week = wday(Date, label = TRUE))

# Day of Week Analysis
ggplot(train_data_remove_casual, aes(x = day_of_week, y = casual)) +
  geom_boxplot(fill = "magenta", alpha = 0.7) +
  labs(title = "Day of Week Distribution of Casual Users", x = "Day of Week", y = "Casual Users")

# Relationship with Weather Variables
ggplot(train_data_remove_casual, aes(x = temp, y = casual)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Casual Users vs Temperature", x = "Temperature", y = "Casual Users")

ggplot(train_data_remove_casual, aes(x = atemp, y = casual)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Casual Users vs Apparent Temperature", x = "Apparent Temperature", y = "Casual Users")

ggplot(train_data_remove_casual, aes(x = hum, y = casual)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "green") +
  labs(title = "Casual Users vs Humidity", x = "Humidity", y = "Casual Users")

ggplot(train_data_remove_casual, aes(x = windspeed, y = casual)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "purple") +
  labs(title = "Casual Users vs Windspeed", x = "Windspeed", y = "Casual Users")

# Heatmap of correlations
# Selecting numeric columns only
numeric_vars <- train_data_remove_casual %>%
  select_if(is.numeric)

# Calculating the correlation matrix
cor_matrix <- cor(numeric_vars)

# Creating the heatmap
ggcorrplot(cor_matrix, 
           hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method = "circle", 
           colors = c("red", "white", "blue"), 
           title = "Correlation Heatmap", 
           ggtheme = theme_minimal())




# Q-Q plot to check normality of residuals
qqnorm(train_data_remove$casual)
qqline(train_data_remove$casual)








#Step 7: Time Series Decomposition
ts_train <- ts(train_data$value, frequency=365)  # Assuming daily data
decomp <- stl(ts_train, s.window="periodic")
plot(decomp)

#Step 8: Model Building
#Build a forecasting model using the training data.

#Step 9: Model Evaluation
#Evaluate the model's performance using the testing data.




#DATA INTEGRITY

#we have 2011 and 2012 data(0 and 1 respectively)
#Clear or Few Clouds (Category 1),Partly Cloudy (Category 2),Cloudy or Overcast (Category 3) (Documentation is missing)
#weekdays 0,1,2,3,4,5,6(7 week days)
#workingday:0,1(I assume 0 it's saturday and Sunday whilst 1 is a working day)
#4 seasons
#holiday: 0 is non holiday day and 1 is a bank holiday
unique(daily_rental$yr)
unique(hourly_rental$yr)
unique(daily_rental$mnth)
unique(hourly_rental$mnth)
unique(daily_rental$weekday)
unique(hourly_rental$weekday)
unique(daily_rental$workingday)
unique(hourly_rental$workingday)
unique(daily_rental$season)
unique(hourly_rental$season)
unique(daily_rental$holiday)
unique(hourly_rental$holiday)
#Checking if casual and registered columns=cnt
#It is confirmed that cnt=registered+casual
daily_rental <- daily_rental %>%
  mutate(AggCasualandReg=casual+registered) 
hourly_rental <- hourly_rental %>%
  mutate(AggCasualandReg=casual+registered) 
diff<- daily_rental$AggCasualandReg-daily_rental$cnt
diffhourly<- hourly_rental$AggCasualandReg-hourly_rental$cnt
#Data integrity check
unique(diff)
unique(diffhourly)

#Checking of datasets after transformations
glimpse(data_no_outliers_casual)
glimpse(daily_rental)

#EXPLORATORY DATA ANALYSIS(EDA)

#Generate summary statistics to understand the data distribution.
#cnt(casual+reg)
#We use reframe() instead of summarise() to ensure that the result is always an ungrouped data frame.
summary_stats_cnt <- data_no_outliers_casual %>%
  reframe(
    mean_cnt = mean(cnt, na.rm = TRUE),
    median_cnt = median(cnt, na.rm = TRUE),
    sd_cnt = sd(cnt, na.rm = TRUE),
    var_cnt = var(cnt, na.rm = TRUE),
    range_min_cnt = min(cnt, na.rm = TRUE),
    range_max_cnt = max(cnt, na.rm = TRUE),
    iqr_cnt = IQR(cnt, na.rm = TRUE),
    skewness_cnt = skewness(cnt, na.rm = TRUE),
    kurtosis_cnt = kurtosis(cnt, na.rm = TRUE),
    count = n(),
    proportion_above_1000 = mean(cnt > 1000, na.rm = TRUE)
  )
# Since the mean and median are close, this suggests a relatively symmetric distribution of bike rentals
#High variability should be factored into the modeling process to ensure accurate forecasts.
print(summary_stats_cnt)

#casual
#The variability in casual bike rentals should be taken into account in the analysis and forecasting models.
# right skew indicates occasional high rental days, which could impact the average values.
summary_stats_casual <- data_no_outliers_casual %>%
  reframe(
    mean_casual = mean(casual, na.rm = TRUE),
    median_casual = median(casual, na.rm = TRUE),
    sd_casual = sd(casual, na.rm = TRUE),
    var_casual = var(casual, na.rm = TRUE),
    range_min_casual = min(casual, na.rm = TRUE),
    range_max_casual = max(casual, na.rm = TRUE),
    iqr_casual = IQR(casual, na.rm = TRUE),
    skewness_casual = skewness(casual, na.rm = TRUE),
    kurtosis_casual = kurtosis(casual, na.rm = TRUE),
    count = n(),
    proportion_above_1000 = mean(casual > 1000, na.rm = TRUE)
  )
#print
print(summary_stats_casual)




#Seasonal Decomposition of Time Series
#Autocorrelation and Partial Autocorrelation Plots
## Rolling Statistics
# Trend Analysis







#STATIONARITY CHECK 

#The statistical properties of the time series (mean, variance, autocorrelation) do not change over time.
#In other words checking whether the distribution of data over time is consistent
#Also we check if time series have bounded x values(finite variance)
#Methods for checking stationarity include visual inspection and statistical tests like the Augmented Dickey-Fuller (ADF) test.

#ACF plot-Casual users
#All lags crossing the dashed line suggests there is significant autocorrelation at multiple lags, 
#indicating that the series might not be stationary
#This pattern suggests that a moving average (MA) component might be appropriate
#The series might benefit from an ARMA(p, q) or ARIMA(p, d, q) model, 
#where 'p' and 'q' are determined by the significant spikes in the PACF and ACF plots, respectively.
g1<- ggAcf(daily_rental$casual,type="correlation") + xlab("T") + ylab("casual") + ggtitle("NonStationary")
#PACF plot-Casual users
g1_pacf <- ggPacf(daily_rental$casual, type = "correlation") + 
  xlab("Lag") + ylab("PACF") + 
  ggtitle("PACF - Casual Users")
grid.arrange(g1,g1_pacf,ncol = 2)

#ACF plot-Registered users
g2<- ggAcf(daily_rental$registered,type="correlation")+xlab("T")+ylab("registered")+ggtitle("Stationary")
#PACF plot-Registered users
# Suggested an ARMA(p, q) model,
#where 'p' is determined by the significant spikes in the PACF plot and 'q' is determined by the significant crossings of the dashed line in the ACF plot
g2_pacf <- ggPacf(daily_rental$registered, type = "correlation") + 
  xlab("Lag") + ylab("PACF") + 
  ggtitle("PACF - Registered Users")
grid.arrange(g2,g2_pacf,ncol = 2)

#ACF plot-Temperature
g3<- ggAcf(daily_rental$registered,type="correlation")+xlab("T")+ylab("temperature")+ggtitle("Stationary")
grid.arrange(g1,g2,g3)
# PACF plot
g3_pacf <- ggPacf(daily_rental$temp, type = "correlation") + 
  xlab("Lag") + ylab("PACF") + 
  ggtitle("PACF - Temperature")
# Arranging PACF plots
grid.arrange(g3, g3_pacf, ncol = 2)

#Based on the ACF and PACF analysis I have non-stationary series
#and hence I have to   make it stationary before fitting a time series model:
#Differencing,Detrending: Decomposition
# Apply first-order differencing with lag=1
daily_rental$casual_diff <- c(NA, diff(daily_rental$casual))
daily_rental$reg_diff <- c(NA, diff(daily_rental$registered))

# Plot the ACF of the differenced series
ggAcf(daily_rental$casual_diff, type = "correlation") +
  xlab("Lag") + ylab("ACF") +
  ggtitle("ACF - Differenced Casual Users")

ggPacf(daily_rental$casual_diff, type = "correlation") +
  xlab("Lag") + ylab("PACF") +
  ggtitle("PACF - Differenced Casual Users")

#SEASONAL DECOMPOSITION
# Perform seasonal decomposition using STL decomposition
#It seems that the stl() function requires the input series to have at least two complete periods of seasonal data.
#This error suggests that your data may not have enough observations to perform seasonal decomposition effectively
#using the default settings.
#decomp_result <- stl(daily_rental$casual, s.window = "periodic")

# Plot the decomposed components
#plot(decomp_result)

#MODEL SELECTION
# Automatic selection of SARIMA model using auto.arima
sarima_model <- auto.arima(daily_rental$casual, seasonal = TRUE)
sarima_model

# Fit the SARIMA model
sarima_fit <- Arima(daily_rental$casual, order = c(5, 1, 3))

# Print model summary
summary(sarima_fit)

# Check diagnostic plots
checkresiduals(sarima_fit)

#HOLDOUT VALIDATION OF MY MODEL

# Assuming daily_rental is your time series dataset
# Split the data into training and test sets (80-20 split)
train_size <- floor(0.8 * nrow(daily_rental))
train_data <- daily_rental[1:train_size, ]
test_data <- daily_rental[(train_size + 1):nrow(daily_rental), ]

# Fit the SARIMA model on the training set
sarima_model <- auto.arima(train_data$casual, seasonal = TRUE)

# Make predictions on the test set
forecast_values <- forecast(sarima_model, h = nrow(test_data))

# Evaluate model performance
#it appears that while the SARIMA model performs reasonably well on the training set,
#
#it exhibits poorer performance on the test set. 
#This could be due to overfitting or a lack of generalization to unseen data.
#Further investigation and model refinement may be necessary to improve its performance on the test set.
accuracy_metrics <- accuracy(forecast_values, test_data$casual)
print(accuracy_metrics)



# Generate forecasts for the next 12 months
forecast_values_next <- forecast(sarima_fit, h = 12)

# Extract the last date in the series and add one month to it
last_date <- as.Date(paste0("01-", tail(monthly_casual_filtered$Month_Year, 1)), format = "%d-%b-%y")
next_month <- last_date + months(1)

# Create a sequence of dates for the next 12 months
forecast_dates <- seq(next_month, by = "1 month", length.out = 12)

# Create a dataframe with forecasted values
forecast_df_next <- data.frame(date = forecast_dates, 
                               forecast = forecast_values_next$mean, 
                               lower = forecast_values_next$lower[, "95%"], 
                               upper = forecast_values_next$upper[, "95%"])



# Check if 'casual' column exists in the 'daily_rental' dataset
colnames(daily_rental)

# Convert the date format to "Month-Year"
daily_rental$Month_Year <- format(daily_rental$Date, "%b-%y")

# Create a sequence of dates for the next 12 months beyond the last date in the dataset
last_date <- max(daily_rental$Date)
forecast_dates <- seq(last_date + 1, by = "1 month", length.out = 12)

# Create a dataframe with forecasted values
forecast_df_next <- data.frame(date = forecast_dates, 
                               forecast = forecast_values_next$mean, 
                               lower = forecast_values_next$lower[, "95%"], 
                               upper = forecast_values_next$upper[, "95%"])

# Modify date format for forecast dates
forecast_df_next$Month_Year <- format(forecast_df_next$date, "%b-%y")

# Create the plot with explicit dataset specification
# Create the plot with explicit dataset specification and x-axis formatting
# Create the plot with explicit dataset specification and custom x-axis breaks
# Create the plot with explicit dataset specification and custom x-axis breaks
casual_plot <- ggplot() +
  geom_line(data = daily_rental, aes(x = Date, y = casual), color = "blue") +
  geom_ribbon(data = forecast_df_next, aes(x = date, ymin = lower, ymax = upper), fill = "grey", alpha = 0.3) +
  geom_line(data = forecast_df_next, aes(x = date, y = forecast), color = "red") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +  # Format x-axis labels
  labs(title = "Historical Casual Users and Forecast for Next 12 Months",
       x = "Date",
       y = "Casual Users") +
  theme_minimal()

# Display the plot
casual_plot

#What type of data are you analyzing (e.g., bike rental data)?
##Are there any specific patterns or trends you've observed in the historical data?
#What forecasting methods or models have you used so far?
#Have you identified any potential factors that could explain the discrepancy between historical peak values and forecasted values?
#Are there any external factors or additional data sources you're considering incorporating into your analysis?

# Ensure the 'Date' column is of Date type
data_no_outliers_casual$Date <- as.Date(data_no_outliers_casual$Date, format="%Y-%m-%d")

# Determine the unique months present in the dataset
unique_months <- unique(format(data_no_outliers_casual$Date, "%Y-%m"))

# Add January 2013 manually to the unique months if it's not already present
if (!"2013-01" %in% unique_months) {
  unique_months <- c(unique_months, "2013-01")
}

# Remove the label for February 2013
unique_months <- unique_months[unique_months != "2013-02"]

# Sort the unique months
unique_months <- sort(unique_months)

# Convert unique months to Date format for ggplot2 breaks
date_breaks <- as.Date(paste0(unique_months, "-01"))

# Plot the data with modified x-axis
ggplot(data_no_outliers_casual, aes(x = Date, y = casual)) + 
  geom_line() + 
  geom_point() +
  labs(title = "Casual Users Over Time (No Outliers)", x = "Time", y = "Casual Users") +
  scale_x_date(breaks = date_breaks, date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#FROM THE ABOVE DIAGRAM I CANNOT RECOGNISE TREND PATTERNS


# Plotting
plot(ts_data_temp, xlab = "Date", ylab = "Temperature", main = "Temperature Over Time")
plot(ts_data_casual, xlab = "Date", ylab = "Temperature", main = "# of Casual Users Over Time")


