#R Lotto predict
library(dplyr)
library(tidyr)
library(forecast)
library(ggplot2)

df <- read_csv("Lotto.csv",col_names = TRUE, show_col_types = FALSE)

# Assuming df is your original tibble
df_new <- df %>%
  separate(data2, into = c("d1", "d2", "d3", "d4", "d5", "d6"), sep = "\n")

# convert to numeric
df_new <- df_new %>% 
  mutate_at(vars(d1:d6), as.numeric)
  
# Convert 'date' column to date format
# Wed, Feb 28, 2024
df_new$date2 <- as.Date(df_new$data, format = "%a, %b %d, %Y")

# keep only the new columns
df_new <- df_new %>% select(date2, d1, d2, d3, d4, d5, d6)
  
# Display the new tibble
view(df_new)

#########################################################
# Function to fit ARIMA model and make predictions
fit_and_predict <- function(column_name) {
  # Fit ARIMA model
  model <- auto.arima(df_new[[column_name]])
  
  # Make one-step-ahead forecast
  forecast_result <- forecast(model, h = 1)
  
  # Extract forecasted value
  forecast_value <- forecast_result$mean[1]
  
  return(list(forecast_value = forecast_value, model = model))
}

# Predict each column one step ahead and store the results
predictions <- lapply(paste0("d", 1:6), fit_and_predict)

# Print the predictions
for (i in 1:6) {
  cat("Prediction for", paste0("d", i), ":", predictions[[i]]$forecast_value, "\n")
}

# Function to make predictions and plot actual vs. predicted values
plot_predictions <- function(data, col_name) {
  # Extract the target column
  target <- data[[col_name]]
  
  # Make predictions (here, I'm just using a simple lag-1 prediction as an example)
  predictions <- c(NA, target[-length(target)])  # Lag-1 prediction
  print(paste("predictions=",length(predictions)))
  print(paste("target=",length(target)))
  
  # Create a data frame with actual and predicted values
  df3 <- data.frame(actual = target, predicted = predictions)
  view(df3)
  
  # Plot actual vs. predicted values
  ggplot(df3, aes(x = 1:length(actual))) +
    geom_line(aes(y = actual, color = "Actual")) +
    geom_line(aes(y = predicted, color = "Predicted")) +
    labs(title = col_name) +
    theme_minimal()
}

# Plot each column d1 to d6
for (col in c("d1", "d2", "d3", "d4", "d5", "d6")) {
  plot_predictions(df_new, col)
}


# Plot errors
errors <- sapply(1:6, function(i) {
  observed <- df_new[[paste0("d", i)]][-1]
  forecasted <- residuals(predictions[[i]]$model)
  data.frame(error = observed - forecasted)
})

# Convert errors to a tibble
errors_df <- as.data.frame(do.call(cbind, errors))

# Plotting errors
error_plots <- lapply(errors_df, function(error_column) {
  ggplot(data.frame(error = error_column), aes(x = 1:length(error_column), y = error)) +
    geom_line() +
    labs(title = "Error Plot", y = "Error", x = "Observation") +
    theme_minimal()
})

# Show the plots
for (i in 1:6) {
  print(error_plots[[i]])
}

############### ML predicts ###################
This code will train and test different machine learning models (linear regression, random forest, and GBM) 
for each column d1 to d6, selecting the model with the lowest root mean squared error (RMSE) 
for each column. Note that you may need to adapt the code to your specific dataset and model requirements
library(caret)
library(tidyverse)
library(randomForest)
library(xgboost)

# Function to train and test a model, returning the RMSE
train_and_test_model <- function(train_data, test_data, model_name) {
  model_formula <- as.formula(paste0("d1 ~ ."))
  
  # Train the model
  model <- train(
    model_formula,
    data = train_data,
    method = model_name,
    trControl = trainControl(method = "cv", number = 5)
  )
  
  # Make predictions
  predictions <- predict(model, test_data)
  
  # Calculate RMSE
  rmse <- sqrt(mean((test_data$d1 - predictions)^2))
  
  return(list(model = model, rmse = rmse))
}

# List to store the results
results <- list()

# Predict each column d1 to d6
for (col in c("d1", "d2", "d3", "d4", "d5", "d6")) {
  # Split data into train and test sets
  split_index <- floor(0.8 * nrow(df_new))
  train_data <- df_new[1:split_index, ]
  test_data <- df_new[(split_index + 1):nrow(df_new), ]
  
  # Train and test linear regression model
  lm_result <- train_and_test_model(train_data, test_data, "lm")
  
  # Train and test random forest model
  rf_result <- train_and_test_model(train_data, test_data, "rf")
  
  # Train and test GBM model
  gbm_result <- train_and_test_model(train_data, test_data, "gbm")
  
  # Select the model with the lowest RMSE
  best_model_name <- which.min(c(lm_result$rmse, rf_result$rmse, gbm_result$rmse))
  best_model <- list(
    "lm" = lm_result$model,
    "rf" = rf_result$model,
    "gbm" = gbm_result$model
  )[[names(which.min(c(lm_result$rmse, rf_result$rmse, gbm_result$rmse)))]]
  
  # Store the results
  results[[col]] <- list(
    "model" = best_model,
    "rmse" = min(c(lm_result$rmse, rf_result$rmse, gbm_result$rmse))
  )
}

# Print the results
print(results)

#################
In this code:

We first create a function predict_next_value() that takes the data and the column name as input, trains different 
machine learning models (in this case, linear regression, random forest, gradient boosting, and XGBoost), 
and selects the best model based on cross-validation.
We then use lapply() to apply this function to each column d1 to d6, storing the predictions and error for each 
column in a list called prediction_results.
You can adjust the list of machine learning algorithms (methodList) in the caretList() function to include
other algorithms you want to try. Additionally, you can fine-tune the parameters of each algorithm for better performance.
library(caret)

# Function to train and predict using different algorithms
predict_next_value <- function(data, col_name) {
  # Extract the target column
  target <- data[[col_name]]
  
  # Create lagged dataset
  lag_data <- cbind(data[-1, ], lag(data[[col_name]], 1))
  colnames(lag_data)[ncol(lag_data)] <- paste0(col_name, "_lag1")
  
  # Split data into training and testing sets
  train_index <- 1:(nrow(lag_data) * 0.8)
  train <- lag_data[train_index, ]
  test <- lag_data[-train_index, ]
  
  # Train predictive models
  models <- caretList(as.formula(paste(col_name, "_lag1", "~ .")), data = train, trControl = trainControl(method = "cv", number = 5), 
  methodList = c("lm", "rf", "gbm", "xgbTree"))
  
  # Make predictions
  predictions <- predict(models$bestTune$finalModel, newdata = test)
  
  # Calculate error
  error <- sqrt(mean((predictions - test[[paste0(col_name, "_lag1")]])^2))
  
  # Return predictions and error
  return(list(predictions = predictions, error = error))
}

# Predict next values for each column d1 to d6
prediction_results <- lapply(c("d1", "d2", "d3", "d4", "d5", "d6"), function(col) {
  predict_next_value(df_new, col)
})
