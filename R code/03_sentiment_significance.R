# Load necessary libraries
library(readr)
library(dplyr)
library(rugarch)

# Load one of the scaled datasets
rates_with_sentiment <- read_csv("dane/rates_with_sentiment_clean_gauss.csv")

# List of countries (columns representing countries in the dataset)
countries <- colnames(rates_with_sentiment)[1:(ncol(rates_with_sentiment) - 4)]  # Exclude sentiment indices and date

# List of sentiment indices to test
sentiment_indices <- c("Michigan_Sentiment_Index", "ZEW_Economic_Growth_Index", "ZEW_Current_Situation_Index")

# Initialize a matrix to store p-values (rows: countries, columns: sentiment indices)
p_value_matrix <- matrix(NA, nrow = length(countries), ncol = length(sentiment_indices), 
                         dimnames = list(countries, sentiment_indices))

# Initialize a matrix to store the coefficient signs (positive/negative)
sign_matrix <- matrix(NA, nrow = length(countries), ncol = length(sentiment_indices), 
                      dimnames = list(countries, sentiment_indices))

# Loop over each country and each sentiment index to build GJR-GARCH-M models
for (country in countries) {
  for (sentiment in sentiment_indices) {
    
    # Print progress for current country and sentiment index
    cat("Fitting model for country:", country, "and sentiment index:", sentiment, "\n")
    
    # Build the model for the current country and sentiment index
    spec <- ugarchspec(
      variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
      mean.model = list(armaOrder = c(0, 0), include.mean = TRUE, 
                        external.regressors = as.matrix(rates_with_sentiment[[sentiment]])),
      distribution.model = "norm"
    )
    
    # Fit the model for the current country's returns
    fit <- ugarchfit(spec = spec, data = rates_with_sentiment[[country]], solver = "hybrid")
    
    # Extract the coefficient matrix
    coef_matrix <- fit@fit$matcoef
    
    # Dynamically find the row name for the external regressor (sentiment index)
    regressor_name <- grep("mxreg", rownames(coef_matrix), value = TRUE)
    
    if (length(regressor_name) > 0) {
      # Fix the column name references by trimming leading spaces
      p_value <- coef_matrix[regressor_name, "Pr(>|t|)"]  # No leading space here
      coefficient <- coef_matrix[regressor_name, " Estimate"]  # Adjust for leading space
      
      # Store the p-value in the matrix
      p_value_matrix[country, sentiment] <- p_value
      
      # Store the sign of the coefficient if it is significant
      if (p_value < 0.05) {
        sign_matrix[country, sentiment] <- ifelse(coefficient > 0, "Positive", "Negative")
      } else {
        sign_matrix[country, sentiment] <- "Not significant"
      }
    } else {
      # If no external regressor was found, mark it as not significant
      p_value_matrix[country, sentiment] <- NA
      sign_matrix[country, sentiment] <- "No regressor"
    }
    
    # Print progress after model fit
    cat("Model for country:", country, "and sentiment index:", sentiment, "completed.\n")
  }
}

# Display the p-value matrix
p_value_matrix

# Display the sign matrix
sign_matrix

