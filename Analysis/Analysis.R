library(caret)
library(ggplot2)
library(dplyr)
data <- read.csv("PathToFile/wisconsin_dataset.csv")
data <- na.omit(data) # Remove rows with missing values
preprocess_data <- function(data) {
  # Check if data has at least 2 rows
  if (nrow(data) < 2) {
    return(data) # Return data without modification
  }
  for (i in 1:ncol(data)){
    if(is.character(data[,i])){
      data[,i]=factor(data[,i])
    }
  }
  numerical_features <- c("Cl.thickness", "Cell.size", "Cell.shape", "Marg.adhesion", "Epith.c.size",
                          "Bare.nuclei", "Bl.cromatin", "Normal.nucleoli", "Mitoses")
  data[numerical_features] <- scale(data[numerical_features])
  return(data)
}
data <- preprocess_data(data)
set.seed(123) # Set seed for reproducibility
train_index <- createDataPartition(data$Class, p = 0.8, list = FALSE)
train <- data[train_index, ]
test <- data[-train_index, ]
model <- train(Class.... ~ ., data = train, method ="glm", trControl = trainControl(method = "cv"))
predictions <- predict(model, test)
confusion_matrix <- confusionMatrix(predictions, test$Class)
ggplot(train, aes(x = Cl.thickness, y = Cell.size, color = Class....)) + geom_point()

