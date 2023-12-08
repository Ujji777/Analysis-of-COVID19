library(tidyverse)
library(caret)
library(e1071)


ccovid_data<-
# Create a dataframe
#covid_data <- data.frame(Age = age, Temperature = temperature, Cough_Severity = cough_severity, Has_COVID = has_covid)

# Explore the dataset
#head(covid_data)

  
# Split the data into features (train_x, test_x) and target variable (train_y, test_y)
set.seed(456)
train_indices <- createDataPartition(covid_data$Has_COVID, p = 0.8, list = FALSE)
train_data <- covid_data[train_indices, ]
test_data <- covid_data[-train_indices, ]

train_x <- subset(train_data, select = -Has_COVID)
test_x <- subset(test_data, select = -Has_COVID)

train_y <- train_data$Has_COVID
test_y <- test_data$Has_COVID

# Build a simple SVM model
model <- svm(train_y ~ ., data = cbind(train_x, train_y))

# Make predictions on the test set
predictions <- predict(model, newdata = test_x)
predictions