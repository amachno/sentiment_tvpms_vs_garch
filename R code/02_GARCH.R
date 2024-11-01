# Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)
library(rugarch)
library(openxlsx)

# 1. Load the rates_with_sentiment.csv file
rates_with_sentiment <- read_csv("dane/rates_with_sentiment.csv")

# 2. Keep only the specified columns: Developed, Emerging, Frontier, Date, and sentiment indices
rates_with_sentiment <- rates_with_sentiment %>%
  select(Developed, Emerging, Frontier, Date, 
         Michigan_Sentiment_Index, ZEW_Economic_Growth_Index, ZEW_Current_Situation_Index, ZEW_Germany_Sentiment)

# 3. Remove trend from Michigan_Sentiment_Index using linear regression
# Convert Date to numeric for regression
rates_with_sentiment$Date_numeric <- as.numeric(as.Date(rates_with_sentiment$Date))
# Fit a linear model and extract residuals to de-trend Michigan sentiment
michigan_lm <- lm(Michigan_Sentiment_Index ~ Date_numeric, data = rates_with_sentiment)
rates_with_sentiment$Michigan_Sentiment_Index <- residuals(michigan_lm)

# 4. Impute missing values by carrying forward the last available value
rates_with_sentiment <- rates_with_sentiment %>%
  arrange(Date) %>%
  fill(Michigan_Sentiment_Index, ZEW_Economic_Growth_Index, ZEW_Current_Situation_Index, ZEW_Germany_Sentiment, .direction = "down")

# 5. Perform Gaussianization of sentiment indices
rank_normalize <- function(x) {
  # Rank the data and scale to (0, 1) range
  ranks <- rank(x, ties.method = "average")
  ranks_scaled <- (ranks - 0.5) / length(x)
  # Apply inverse normal CDF (qnorm) to obtain Gaussianized values
  qnorm(ranks_scaled)
}

# Apply Gaussianization to each sentiment index
rates_with_sentiment <- rates_with_sentiment %>%
  mutate(
    Michigan_Sentiment_Index = rank_normalize(Michigan_Sentiment_Index),
    ZEW_Economic_Growth_Index = rank_normalize(ZEW_Economic_Growth_Index),
    ZEW_Current_Situation_Index = rank_normalize(ZEW_Current_Situation_Index),
    ZEW_Germany_Sentiment = rank_normalize(ZEW_Germany_Sentiment)
  )

# 6. Loop through each market-sentiment pair to train the GJR-GARCH-M model and save results
# Define the markets and sentiment indices for the loop
markets <- c("Developed", "Emerging", "Frontier")
sentiments <- c("Michigan_Sentiment_Index", "ZEW_Economic_Growth_Index", "ZEW_Current_Situation_Index", "ZEW_Germany_Sentiment")

# Initialize list to store coefficient matrices
results <- list()

for (market in markets) {
  for (sentiment in sentiments) {
    
    # Prepare data by removing any rows with missing values for the current sentiment and market
    model_data <- rates_with_sentiment %>%
      select(all_of(market), all_of(sentiment)) %>%
      drop_na()  # Remove rows with NA values
    
    # Define GJR-GARCH-M model specification
    spec <- ugarchspec(
      variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
      mean.model = list(armaOrder = c(0, 0), include.mean = TRUE, 
                        external.regressors = as.matrix(model_data[[sentiment]])),
      distribution.model = "norm"
    )
    
    # Fit the model to the current market's returns with the specified sentiment index
    fit <- tryCatch({
      ugarchfit(spec = spec, data = model_data[[market]], solver = "hybrid")
    }, error = function(e) NULL)
    
    # If model fitting is successful, store the coefficient matrix in results list
    if (!is.null(fit)) {
      coef_matrix <- fit@fit$matcoef
      results[[paste(market, sentiment, sep = "_")]] <- coef_matrix
    } else {
      cat("Model fitting failed for", market, "-", sentiment, "\n")
    }
  }
}

# 7. Save the coefficient matrices to an Excel file with different sheets for each market-sentiment pair
# Shorten sentiment names for Excel sheet compatibility
abbreviate_name <- function(name) {
  name <- gsub("Michigan_Sentiment_Index", "Mich_Sent", name)
  name <- gsub("ZEW_Economic_Growth_Index", "ZEW_Econ", name)
  name <- gsub("ZEW_Current_Situation_Index", "ZEW_CS", name)
  name <- gsub("ZEW_Germany_Sentiment", "ZEW_Germ", name)
  return(name)
}

# Apply abbreviation function to sheet names
output_file <- "dane/GARCH_Coefficients.xlsx"
wb <- createWorkbook()

for (name in names(results)) {
  # Abbreviate the sheet name to fit the 31-character limit
  sheet_name <- abbreviate_name(name)
  
  # Add the worksheet with the abbreviated name and write data
  addWorksheet(wb, sheetName = sheet_name)
  writeData(wb, sheet = sheet_name, results[[name]])
}

# Save the workbook
saveWorkbook(wb, output_file, overwrite = TRUE)

cat("Results saved to", output_file)

