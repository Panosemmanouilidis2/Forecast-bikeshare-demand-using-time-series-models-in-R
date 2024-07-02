#Project to conduct a time series data analysis to forecast future rental bike demands
install.packages("jsonlite")
#importing libraries
library(tidyverse)
library(dplyr)
library(lubridate)
library("rmarkdown")
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

#setting the Working Directory
setwd("C:\\Panos\\Time Series Analysis in R\\Forecast Daily Bike Rental")

#Importing Datasets
daily_rental<- read.csv("day.csv")
hourly_rental<- read.csv("hour.csv")

#DATA CLEANSING
#Viewing Datasets to check if and what data cleansing is needed
#"cnt" provides the total demand for bikes on a particular day, regardless of user type.
#"registered" co is registered users and "casual" is casual users. "atemp" is "apparent temperature" or "feels-like temperature." 
#cnt=registered and casual

#DATA CLEANSING

#1.Ensure all data columns are of the correct type.
#2.Missing values
#3.Outliers

#Overview for checking data type and missing values
#We don't have missing values
glimpse(daily_rental)
summary(daily_rental)
glimpse(hourly_rental)
summary(hourly_rental)

#Convert date columns to the appropriate date format.
#dteday column is string in both datasets and should be converted to date
#Converting dteday from string to date format
daily_rental <- daily_rental %>%
  mutate(Date = as.Date(dteday, format = "%d/%m/%Y")) %>%
  select(-dteday)
glimpse(daily_rental)
hourly_rental <- hourly_rental %>%
  mutate(Date = as.Date(dteday, format = "%Y-%m-%d")) %>%
  select(-dteday)
glimpse(hourly_rental)

#Outliers only for the casual users(limit the project scope due to time limitations)
#Thera are outliers on casual users number
daily_rental%>%
  ggplot()+
  geom_boxplot(mapping=aes(cnt))
daily_rental%>%
  ggplot()+
  geom_boxplot(mapping=aes(registered))
daily_rental%>%
  ggplot()+
  geom_boxplot(mapping=aes(casual))
daily_rental%>%
  ggplot()+
  geom_boxplot(mapping=aes(temp))

#Removing outliers from casual users columns
# Define a threshold to identify outliers
threshold <- quantile(daily_rental$casual, 0.90) # 90th percentile

# Filter out the outliers
data_no_outliers_casual <- daily_rental[daily_rental$casual <= threshold, ]
data_no_outliers_casual

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



#PLOTS
#Plot the time series to visualize trends, patterns, and anomalies.
#Plot time series(1 chart)+3 charts for daily,weekly and monthly datasets to identify pattern and trends
#ts
#Visualising rental data
# Temperature as expected has seasonal characteristics
ts_data_temp <- ts(daily_rental$temp, start = c(year(min(daily_rental$Date)), as.numeric(format(min(daily_rental$Date), "%j"))),
              frequency = 365)  # Assuming daily data
ts_data_casual <- ts(data_no_outliers_casual$casual, start = c(year(min(daily_rental$Date)), as.numeric(format(min(daily_rental$Date), "%j"))),
                   frequency = 365)  # Assuming daily data

#EDA
#PLOT for seasonality trends
# Plotting
plot(ts_data_temp, xlab = "Date", ylab = "Temperature", main = "Temperature Over Time")
plot(ts_data_casual, xlab = "Date", ylab = "Temperature", main = "# of Casual Users Over Time")

#The above casual users graph doesn't show clear patterns so I create a customise graph with Avg prices

# Create a new column for Year-Month in the format 'Jan-11'
data_no_outliers_casual$YearMonth <- format(data_no_outliers_casual$Date, "%b-%y")

# Group by the new YearMonth column and calculate the average number of casual users
monthly_avg_casual <- data_no_outliers_casual %>%
  group_by(YearMonth) %>%
  dplyr::summarise(AverageCasualUsers =mean(casual))
# Print the resulting DataFrame
print(monthly_avg_casual)

# Convert YearMonth back to a Date type for proper ordering
monthly_avg_casual$Date <- as.Date(paste0("01-", monthly_avg_casual$YearMonth), format = "%d-%b-%y")

# Order the DataFrame by the Date column
monthly_avg_casual <- monthly_avg_casual %>%
  arrange(Date)

print(monthly_avg_casual)

# Create the ggplot with geom_line
ggplot(monthly_avg_casual, aes(x = Date, y = AverageCasualUsers, group = 1)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  theme_minimal() +
  labs(title = "Average Number of Casual Users Per Month",
       x = "Month-Year",
       y = "Average Number of Casual Users") +
  theme(panel.background = element_blank()) +
  theme(plot.background = element_blank()) +
  scale_y_continuous(limits = c(0, max(monthly_avg_casual$AverageCasualUsers, na.rm = TRUE))) +
  theme(axis.text.x = element_text(vjust = 5, margin = margin(-6, 0, 0, 0))) +
  guides(y = "none")

#HEATMAP

#TOTAL USERS(cASUAL+REG)

# Check unique values in YearMonth column to identify any malformed entries
unique(data_no_outliers_casual$YearMonth)

# Create YearMonth column with correct Date format
data_no_outliers_casual <- data_no_outliers_casual %>%
  mutate(YearMonth = as.Date(paste0("01-", YearMonth), format = "%d-%b-%y"))

# Check for NA values in YearMonth after conversion
sum(is.na(data_no_outliers_casual$YearMonth))

# Aggregate data to monthly level
monthly_rental <- data_no_outliers_casual %>%
  group_by(YearMonth) %>%
  summarize(monthly_cnt = sum(cnt))

# Extract Year and Month from YearMonth
monthly_rental_totalusers <- monthly_rental %>%
  mutate(Year = year(YearMonth),
         Month = month(YearMonth))

# Ensure all months (1 to 12) are included
complete_months <- expand.grid(Month = 1:12, Year = unique(monthly_rental_totalusers$Year))
monthly_rental_totalusers <- merge(complete_months, monthly_rental_totalusers, by = c("Year", "Month"), all.x = TRUE)
monthly_rental_totalusers$monthly_cnt[is.na(monthly_rental_totalusers$monthly_cnt)] <- 0

# Reshape data for heatmap
heatmap_data <- dcast(monthly_rental_totalusers, Month ~ Year, value.var = "monthly_cnt")

# Plotting heatmap
ggplot(melt(heatmap_data, id.vars = "Month"), aes(x = variable, y = Month, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red", na.value = "white") +
  scale_y_continuous(breaks = 1:12) +  # Ensure y-axis has integer values
  labs(title = "Heatmap of Monthly Total Bike Rentals", x = "Year", y = "Month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#HEASTMAP CASUAL USERS

# Aggregate data to monthly level
monthly_rental_casual <- data_no_outliers_casual %>%
  group_by(YearMonth) %>%
  summarize(monthly_casual = sum(casual))

# Extract Year and Month from YearMonth
monthly_rental_casual <- monthly_rental_casual %>%
  mutate(Year = year(YearMonth),
         Month = month(YearMonth))

# Ensure all months (1 to 12) are included
complete_months <- expand.grid(Month = 1:12, Year = unique(monthly_rental_casual$Year))
monthly_rental_casual <- merge(complete_months, monthly_rental_casual, by = c("Year", "Month"), all.x = TRUE)
monthly_rental_casual$monthly_casual[is.na(monthly_rental_casual$monthly_casual)] <- 0


# Reshape data for heatmap
heatmap_data_casual <- dcast(monthly_rental_casual, Month ~ Year, value.var = "monthly_casual")

# Plotting heatmap
ggplot(melt(heatmap_data_casual, id.vars = "Month"), aes(x = variable, y = Month, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red", na.value = "white") +
  scale_y_continuous(breaks = 1:12) +  # Ensure y-axis has integer values
  labs(title = "Heatmap of Monthly Casual Bike Rentals", x = "Year", y = "Month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#what remains to be done for EDA(13/06/2024)


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





