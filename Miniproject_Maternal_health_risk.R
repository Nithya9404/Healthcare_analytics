library(readr)
df <- read.csv('E:/Desktop/datasets/Maternal Health Risk Data Set.csv')
head(df)

missing_values <- rowSums(is.na(df))
print(missing_values)

summary(df)

print(unique(df$RiskLevel))

risk_level_mapping <- c("low risk"=0,"mid risk"=1,"high risk"=2)
df$RiskLevel <- risk_level_mapping[df$RiskLevel]
print(df$RiskLevel)

#Feature Selection
#Correlation Analysis
correlations <- sapply(df, function(x) cor(x, df$RiskLevel))
correlations <- correlations[-length(correlations)]
print(correlations)

library(corrplot)
correlation_matrix <- cor(df)
corrplot(correlation_matrix, method = "circle", type = "full", order = "hclust", tl.col = "black", tl.srt = 45)
title("Correlation Plot")

#Creating Simple neural network
# Import required libraries
# Import required libraries
library(keras)
library(tensorflow)

# Define the deep learning model for classification
model <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = 'relu', input_shape = c(6)) %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dense(units = 3, activation = 'softmax')  # Change to 3 units for 3 classes

# Compile the model for classification
model %>% compile(
  optimizer = 'adam',
  loss = 'categorical_crossentropy',  # Change the loss function
  metrics = c('accuracy')  # Use accuracy as a metric for classification
)
y_train = df$RiskLevel
print(x_train)
print(y_train)

dim(x_train)
dim(y_train)
# Fit the model with one-hot encoded y_train
history <- model %>% fit(
  x = as.matrix(x_train), 
  y = to_categorical(y_train,num_classes = 3),  # Assuming y_train is one-hot encoded
  epochs = 35,
  batch_size = 32
)
print(history)

#Predicting for new value
new_data <- data.frame(
  Age = c(30,35,25),
  SystolicBP = c(120,140,130),
  DiastolicBP = c(80,90,70),
  BS = c(6.9,7.2,6.5),
  BodyTemp = c(98,98.2,98.1),
  RiskLevel = 0
)
new_example <- as.matrix(new_data[1, ])  # Choose a specific row from new_data
predictions <- model %>% predict(new_example)
predicted_class <- which.max(predictions)
print(predicted_class)






