# Load necessary libraries
library(readr)
library(dplyr)
library(lubridate)  # For working with date formats

# Load and clean the Michigan sentiment data
michigan_data <- read_csv2("dane/trend_CES_USA.csv", col_names = c("Date", "Michigan_Sentiment"))[-1,1:2]
michigan_data$Michigan_Sentiment <- as.numeric(gsub(",", ".", michigan_data$Michigan_Sentiment))
# Convert Date to proper format (dd.mm.yyyy)
michigan_data$Date <- dmy(michigan_data$Date)

# Load and clean the czynniki_EG_CS.csv file
czynniki_data <- read_csv2("dane/czynniki_EG_CS.csv", skip = 1, col_names = c("Date", "ZEW_Economic_Growth", "Empty_Col", "ZEW_Current_Situation"))
czynniki_data <- czynniki_data %>% select(-Empty_Col)
czynniki_data$ZEW_Economic_Growth <- as.numeric(gsub(",", ".", czynniki_data$ZEW_Economic_Growth))
czynniki_data$ZEW_Current_Situation <- as.numeric(gsub(",", ".", czynniki_data$ZEW_Current_Situation))
# Convert Date to proper format (dd.mm.yyyy)
czynniki_data$Date <- dmy(czynniki_data$Date)

# Load and clean the ZEW Germany data
zew_germany_data <- read_csv2("dane/ZEW_GERMANY_OK.csv", col_names = c("Date", "Zamkniecie"))[-1, ]
zew_germany_data$Zamkniecie <- as.numeric(gsub(",", ".", zew_germany_data$Zamkniecie))
# Convert Date to proper format (dd.mm.yyyy)
zew_germany_data$Date <- dmy(zew_germany_data$Date)

# Load and clean the rates.csv file
rates_data <- read_csv("dane/rates.csv")
# Remove the unnecessary "1" column
rates_data <- rates_data %>% select(-"...1")
# Convert Date column to proper format (yyyy-mm-dd)
rates_data$Date <- ymd(rates_data$Date)

# Display the first few rows to verify loading and cleaning was successful
head(michigan_data)
head(czynniki_data)
head(zew_germany_data)
head(rates_data)

# Rename columns for clarity
zew_germany_data <- zew_germany_data %>% rename(ZEW_Germany_Closure = Zamkniecie)

# Now let's join all the sentiment data to the rates_data
# We'll perform left joins on the "Date" column
rates_with_sentiment <- rates_data %>%
  left_join(michigan_data, by = "Date") %>%
  left_join(czynniki_data, by = "Date") %>%
  left_join(zew_germany_data, by = "Date")

# Rename columns in the final dataset for better clarity
rates_with_sentiment <- rates_with_sentiment %>%
  rename(
    Michigan_Sentiment_Index = Michigan_Sentiment,
    ZEW_Economic_Growth_Index = ZEW_Economic_Growth,
    ZEW_Current_Situation_Index = ZEW_Current_Situation,
    ZEW_Germany_Sentiment = ZEW_Germany_Closure
  )

# # Convert Date to numeric format for regression purposes
# rates_with_sentiment$Date_numeric <- as.numeric(as.Date(rates_with_sentiment$Date))
# 
# # Subset data to exclude NA values for Michigan_Sentiment_Index
# non_na_michigan_data <- rates_with_sentiment[!is.na(rates_with_sentiment$Michigan_Sentiment_Index), ]
# 
# # Fit a linear model with Michigan_Sentiment_Index as a function of Date_numeric
# michigan_lm <- lm(Michigan_Sentiment_Index ~ Date_numeric, data = non_na_michigan_data)
# 
# # Calculate residuals and add them as a new column for non-NA rows
# non_na_michigan_data$Michigan_Sentiment_Index_detrended <- residuals(michigan_lm)
# 
# # Merge residuals back to the original data based on Date
# rates_with_sentiment <- rates_with_sentiment %>%
#   left_join(non_na_michigan_data %>% select(Date, Michigan_Sentiment_Index_detrended), by = "Date")
# 
# # Display the first few rows to verify the new column
# head(rates_with_sentiment)

# Save the updated dataset with the detrended column
write_csv(rates_with_sentiment, "dane/rates_with_sentiment.csv")


# Display the first few rows of the combined dataset
head(rates_with_sentiment)

write_csv(rates_with_sentiment, "dane/rates_with_sentiment.csv")

