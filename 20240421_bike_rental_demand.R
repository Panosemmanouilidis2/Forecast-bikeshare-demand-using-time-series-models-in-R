#Project Oto conduct a time series data analysis to forecast future rental bike demands
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
#setting the Working Directory
setwd("~/Time Series Analysis in R/Forecast Daily Bike Rental")
#importing datasets
daily_rental<- read.csv("day.csv")
hourly_rental<- read.csv("day.csv")
#Viewing Datasets to check if and what data cleansing is needed
#"cnt" provides the total demand for bikes on a particular day, regardless of user type.
#"registered" co is registered users and "casual" is casual users. "atemp" is "apparent temperature" or "feels-like temperature." 
#cnt=registered and casual

#DATA CLEANSING

#dteday column is string in both datasets and should be converted to date
glimpse(daily_rental)
glimpse(hourly_rental)
#Converting dteday
daily_rental <- daily_rental %>%
  mutate(Date = as.Date(dteday)) %>%
  select(-dteday)
glimpse(daily_rental)
hourly_rental <- hourly_rental %>%
  mutate(Date = as.Date(dteday)) %>%
  select(-dteday)
glimpse(hourly_rental)

#Checking for outliers-DATA CLEANSING
#No outliers found in the daily_rental dataset and I assume this is the case for hourly_rental
daily_rental%>%
  ggplot()+
  geom_boxplot(mapping=aes(cnt))
daily_rental%>%
  ggplot()+
  geom_boxplot(mapping=aes(AggCasualandReg))
daily_rental%>%
  ggplot()+
  geom_boxplot(mapping=aes(temp))
daily_rental%>%
  ggplot()+
  geom_boxplot(mapping=aes(temp))

#DATA INTEGRITY

#we have 2011 and 2012 data(0 and 1 respectively)
#Clear or Few Clouds (Category 1),Partly Cloudy (Category 2),Cloudy or Overcast (Category 3) (Documentation is missing)
#weekdays 0,1,2,3,4,5,6(7 week days)
#workingday:0,1(I assume it's saturday and Sunday)
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
glimpse(daily_rental)
diff<- daily_rental$AggCasualandReg-daily_rental$cnt
diffhourly<- hourly_rental$AggCasualandReg-hourly_rental$cnt
unique(diff)
unique(diffhourly)

#Checking of datasets
glimpse(hourly_rental)
glimpse(daily_rental)

#EXPLORATORY DATA ANALYSIS

#PLOTS
#Plot time series(1 chart)+3 charts for daily,weekly and monthly dataset to identify pattern and trends
#ts
#Visualising rental data
# Temperature as expected has seasonal characteristics
ts_data_temp <- ts(daily_rental$temp, start = c(year(min(daily_rental$Date)), as.numeric(format(min(daily_rental$Date), "%j"))),
              frequency = 365)  # Assuming daily data
# Plotting
plot(ts_data_temp, xlab = "Date", ylab = "Temperature", main = "Temperature Over Time")
#Casual users
# Create a time series object
ts_data_casualmonthly <- ts(daily_rental$casual, start = c(2011,1), end =c(2012,12),
              frequency = 12)  # Assuming monthly data  
ts_data_casualmonthly
ts_data_casual_weekly <- ts(daily_rental$casual, start = c(2011,1), end =c(2012,12),
                     frequency = 52)  # Assuming weekly data 
ts_data_casual_daily <- ts(daily_rental$casual, start = c(2011,1), end =c(2012,12),
                            frequency = 365)  # Assuming dailyly data 
# Plotting
plot(ts_data_casualmonthly, xlab = "Date", ylab = "Casual Users", main = "Casual Users Monthly")
plot(ts_data_casual_weekly, xlab = "Date", ylab = "Casual Users", main = "Casual Users over Weekly")
#plot(ts_data_casual_daily, xlab = "Date", ylab = "Casual Users", main = "Casual Users over Daily")

#The above plot didn't show a clear trend so i will use ggplot for more customised plots

# Extract month abbreviation and last two digits of the year
daily_rental <- daily_rental %>%
  mutate(Month_Year = paste(substr(month(Date, label = TRUE), 1, 3), format(Date, "%y"), sep = "-"))

# Modify Month_Year variable
monthly_temp <- monthly_temp %>%
  mutate(Month_Year = paste(substr(Month_Year, 1, 3), substr(Month_Year, 5, 6), sep = "-"))

# Aggregate data by month-year and calculate the average casual users
monthly_casual <- daily_rental %>%
  group_by(Month_Year) %>%
  summarize(Avg_Casual_Users = mean(casual))
# Aggregate data by month-year and calculate the average reg users
monthly_registered <- daily_rental %>%
  group_by(Month_Year) %>%
  summarize(Avg_Registered_Users = mean(registered))
# Aggregate data by month-year and calculate the average temperature
monthly_temp <- daily_rental %>%
  group_by(Month_Year) %>%
  summarize(Avg_Temp = mean(temp))

# Modify Month_Year variable
monthly_casual <- monthly_casual %>%
  mutate(Month_Year = paste(substr(Month_Year, 1, 3), substr(Month_Year, 5, 6), sep = "-"))
monthly_registered<- monthly_registered %>%
  mutate(Month_Year = paste(substr(Month_Year, 1, 3), substr(Month_Year, 5, 6), sep = "-"))
monthly_temp <- monthly_temp %>%
  mutate(Month_Year = paste(substr(Month_Year, 1, 3), substr(Month_Year, 5, 6), sep = "-"))

# Filter out December 2010 and January 2013
monthly_casual_filtered <- monthly_casual %>%
  filter(!Month_Year %in% c("Dec-10", "Jan-13"))
monthly_registered_filtered <- monthly_registered %>%
  filter(!Month_Year %in% c("Dec-10", "Jan-13"))
monthly_temp_filtered <- monthly_temp %>%
  filter(!Month_Year %in% c("Dec-10", "Jan-13"))

# Create a time series plot with monthly data
#Casual users
plot_casual<- ggplot(monthly_casual_filtered, aes(x = as.Date(paste("01", Month_Year, sep = "-"), format = "%d-%b-%y"), y = Avg_Casual_Users)) +
  geom_line() +
  scale_x_date(breaks = as.Date(paste0("01-", unique(monthly_casual_filtered$Month_Year)), format = "%d-%b-%y"), 
               labels = unique(monthly_casual_filtered$Month_Year), date_labels = "%b-%y") +
  labs(title = "Average Casual Users by Month-Year",
       x = "Month-Year",
       y = "Average Casual Users") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Adjust angle and alignment
        axis.text = element_text(size = 8))  # Reduce font size
plot_casual
#Registered users
plot_reg<- ggplot(monthly_registered_filtered, aes(x = as.Date(paste("01", Month_Year, sep = "-"), format = "%d-%b-%y"), y = Avg_Registered_Users)) +
  geom_line() +
  scale_x_date(breaks = as.Date(paste0("01-", unique(monthly_registered_filtered$Month_Year)), format = "%d-%b-%y"),
               labels = unique(monthly_registered_filtered$Month_Year), date_labels = "%b-%y") +
  labs(title = "Average Registered Users by Month-Year",
       x = "Month-Year",
       y = "Average Registered Users") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Adjust angle and alignment
        axis.text = element_text(size = 8))  # Reduce font size
plot_reg
#Temperature
plot_temp<- ggplot(monthly_temp_filtered, aes(x = as.Date(paste("01", Month_Year, sep = "-"), format = "%d-%b-%y"), y = Avg_Temp)) +
  geom_line() +
  scale_x_date(breaks = as.Date(paste0("01-", unique(monthly_temp_filtered$Month_Year)), format = "%d-%b-%y"), 
               labels = unique(monthly_temp_filtered$Month_Year), date_labels = "%b-%y") +
  labs(title = "Average Temperature by Month-Year",
       x = "Month-Year",
       y = "Average Temperature") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Adjust angle and alignment
        axis.text = element_text(size = 8))  # Reduce font size

# Arrange plots side by side
grid.arrange(plot_casual, plot_reg, plot_temp, ncol = 3)
plot_temp

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






