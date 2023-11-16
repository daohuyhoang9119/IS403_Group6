# Load libraries
library(keras)
library(tidyverse)
library(zoo)


# Install and load 'keras' package
install.packages("keras")
library(keras)

# Install and load 'tensorflow' package
install.packages("tensorflow")
library(tensorflow)

# Install and load 'magrittr' package
install.packages("magrittr")
library(magrittr)

# Install TensorFlow
install_tensorflow()


#Setup file path
print(getwd())
setwd("D:\\Đại Học CNTT\\Học kỳ 1 năm 5(2023-2024)\\Phân tích kinh doanh\\IS403_Group6")
print(getwd())

# Load BTC data from CSV file
btc_data <- read.csv("BTC-USD.csv", row.names='Date')

# Check the structure of the data
str(btc_data)
print(is.data.frame(btc_data))
print(ncol(btc_data))
print(nrow(btc_data))
# count total missing values  
print("Count of total missing values - ") 
sum(is.na(btc_data))


#Pre-processing data
btc_data$Date <- as.Date(btc_data$Date)
#btc_data <- btc_data %>% arrange(Date)
#df <- subset(btc_data, select = c(Date,Close))
#df <- btc_data[, c("Close")]
df <- data.frame(Date = rownames(btc_data), Close = btc_data$Close)
print(is.data.frame(df))
print(str(df))




# Assuming df1 is your data
total_size <- nrow(df)

# Define the sizes for train, test, and validation sets 7-2-1
train_size <- round(0.7 * total_size)
test_size <- round(0.2 * total_size)
val_size <- total_size - train_size - test_size

# Create the train, test, and validation sets
train_data <- df[1:train_size, ]
test_data <- df[(train_size + 1):(train_size + test_size), ]
val_data <- df[(train_size + test_size + 1):total_size, ]


#Create a matrix from dataframe
create_dataset <- function(dataset, time_step = 1) {
  dataX <- dataY <- list()
  
  for (i in 1:(nrow(dataset) - time_step - 1)) {
    a <- dataset[i:(i + time_step - 1), 1]
    dataX[[i]] <- as.matrix(a) 
    dataY[[i]] <- dataset[i + time_step,]
  }

  return(list(dataX = dataX, dataY = dataY))
}


time_step <- 40


X_train <- create_dataset(train_data, time_step)
y_train <- create_dataset(train_data, time_step)
X_val <- yval <- create_dataset(val_data, time_step)
X_test <- ytest <- create_dataset(test_data, time_step)



# Reshape input data for GRU model
X_train <- array_reshape(X_train, c(dim(X_train)[1], dim(X_train)[2], 1))
X_test <- array_reshape(X_test, c(dim(X_test)[1], dim(X_test)[2], 1))
X_val <- array_reshape(X_val, c(dim(X_val)[1], dim(X_val)[2], 1))



# Define the GRU model
gru_model <- keras_model_sequential()
  layer_gru(units = 60, input_shape = c(time_steps, 1)) %>%
  layer_dense(units = 1)  # Output layer with 1 neuron for regression

# Compile the model
gru_model %>% compile(
  optimizer = optimizer_adam(),  # You can choose a different optimizer if needed
  loss = 'mean_squared_error',   # Use mean squared error for regression
  metrics = c('mean_absolute_error')  # You can add more metrics as needed
)


#Fit the model
history <- model %>% fit(
  x = X_train,
  y = y_train,
  validation_data = list(X_test, y_test),
  epochs = 100,
  batch_size = 64,
  verbose = 1
)

# Print the summary of the model
summary(gru_model)