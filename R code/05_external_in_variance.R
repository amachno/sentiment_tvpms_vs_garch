# Load necessary libraries
library(readr)
library(dplyr)
library(rugarch)
library(openxlsx)
library(tidyr)
library(stringr)


# 1. Load the dataset and select specified columns
rates_with_sentiment <- read_csv("dane/rates_with_sentiment.csv")

# Keep only the relevant columns: "Developed", "Emerging", "Frontier", "Date", and the sentiment indices
rates_with_sentiment <- rates_with_sentiment %>%
  select(Developed, Emerging, Frontier, Date, 
         Michigan_Sentiment_Index, ZEW_Economic_Growth_Index, ZEW_Current_Situation_Index, ZEW_Germany_Sentiment)


# 3. Impute missing values by carrying the last observed value forward
rates_with_sentiment <- rates_with_sentiment %>%
  arrange(Date) %>%
  fill(Michigan_Sentiment_Index, ZEW_Economic_Growth_Index, ZEW_Current_Situation_Index, ZEW_Germany_Sentiment, .direction = "down")

# 4. Perform Gaussianization of sentiment indices
rank_normalize <- function(x) {
  ranks <- rank(x, ties.method = "average")
  ranks_scaled <- (ranks - 0.5) / length(x)
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

# 5. Loop through each market-sentiment pair to train the GJR-GARCH-M model and save results
markets <- c("Developed", "Emerging", "Frontier")
sentiments <- c("Michigan_Sentiment_Index", "ZEW_Economic_Growth_Index", "ZEW_Current_Situation_Index", "ZEW_Germany_Sentiment")
results <- list()

for (market in markets) {
  for (sentiment in sentiments) {
    
    # Prepare data by removing any rows with missing values for the specific sentiment and market columns
    model_data <- rates_with_sentiment %>%
      select(market, sentiment) %>%
      drop_na()  # Remove rows with any NA values in the columns for the market-sentiment pair
    
    # Define GJR-GARCH-M model specification
    spec <- ugarchspec(
      variance.model = list(
        model = "gjrGARCH", 
        garchOrder = c(1, 1),
        external.regressors = as.matrix(model_data[, sentiment])
      ),
      mean.model = list(
        armaOrder = c(1, 0), 
        include.mean = TRUE,
        external.regressors = as.matrix(model_data[, sentiment])
      ),
      distribution.model = "norm"
    )
    
    # Fit the model
    fit <- tryCatch({
      ugarchfit(spec = spec, data = model_data[[market]], solver = "hybrid")
    }, error = function(e) NULL)
    
    if (!is.null(fit)) {
      # Extract the coefficient matrix for the fitted model
      coef_matrix <- fit@fit$matcoef
      
      # Check for the significance of the external regressor in the variance model
      # The parameter name for the external regressor in the variance model is typically "vxreg1"
      if ("vxreg1" %in% rownames(coef_matrix)) {
        p_value <- coef_matrix["vxreg1", "Pr(>|t|)"]
        
        if (p_value < 0.05) {
          cat("Significant impact of", sentiment, "on variance for", market, "\n")
        } else {
          cat("No significant impact of", sentiment, "on variance for", market, "\n")
        }
      } else {
        cat("External regressor not found in variance model for", market, "-", sentiment, "\n")
      }
      
      # Store the coefficient matrix in the results list with a unique name for each market-sentiment pair
      results[[paste(market, sentiment, sep = "_")]] <- coef_matrix
    } else {
      cat("Model fitting failed for", market, "-", sentiment, "\n")
    }
  }
}

# 6. Save the coefficient matrices to an Excel file with different sheets for each market-sentiment pair
output_file <- "dane/GARCH_Coefficients_ar1g.xlsx"
wb <- createWorkbook()

for (name in names(results)) {
  addWorksheet(wb, sheetName = name %>% str_trunc(31))  # Truncate sheet name if longer than 31 characters
  writeData(wb, sheet = name %>% str_trunc(31), x = results[[name]], rowNames = TRUE)  # Enable row names
}

# Save the workbook
saveWorkbook(wb, output_file, overwrite = TRUE)


cat("Results saved to", output_file)


# Initialize matrices to store p-values and coefficient signs for each market-sentiment pair
p_value_matrix <- matrix(NA, nrow = length(markets), ncol = length(sentiments), 
                         dimnames = list(markets, sentiments))
sign_matrix <- matrix(NA, nrow = length(markets), ncol = length(sentiments), 
                      dimnames = list(markets, sentiments))

# Populate the matrices by extracting p-values and signs from each fitted model
for (market in markets) {
  for (sentiment in sentiments) {
    # Get the result from the results list
    result_name <- paste(market, sentiment, sep = "_")
    
    # Check if the result is available
    if (!is.null(results[[result_name]])) {
      coef_matrix <- results[[result_name]]
      
      # Find the row for the external regressor in the variance model
      regressor_name <- "vxreg1"  # Expected label for the external regressor in the variance model
      
      # Check if the external regressor row exists in the coefficient matrix
      if (regressor_name %in% rownames(coef_matrix)) {
        p_value <- coef_matrix[regressor_name, "Pr(>|t|)"]
        coefficient <- coef_matrix[regressor_name, " Estimate"]
        
        # Store the p-value in the p_value_matrix
        p_value_matrix[market, sentiment] <- p_value
        
        # Store the sign in the sign_matrix based on significance
        if (p_value < 0.05) {
          sign_matrix[market, sentiment] <- ifelse(coefficient > 0, "Positive", "Negative")
        } else {
          sign_matrix[market, sentiment] <- "Not significant"
        }
      } else {
        # If the external regressor is not found, mark as "Not applicable"
        sign_matrix[market, sentiment] <- "Not applicable"
        p_value_matrix[market, sentiment] <- NA
      }
    }
  }
}

# Display the p-value matrix
print("P-Value Matrix:")
print(p_value_matrix)

# Display the sign matrix
print("Sign Matrix:")
print(sign_matrix)

##########################################################

