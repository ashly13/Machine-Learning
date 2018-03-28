# For the Wisconsin data set
library(rpart)

data = data.frame(read.csv("Wisconsin.csv", header = FALSE))[, -1]

classes = sort(unique(data[, ncol(data)]))

# Cleaning the data - Feature 6 has "?"
data[, 6] = as.numeric(as.character(data[, 6]))
data[ which(is.na(data[, 6])), 6 ] = round(mean(data[, 6], na.rm = TRUE))

# 10-fold Cross-Validation
folds = cut(seq(1, nrow(data)), breaks = 10, labels = FALSE)

# Results for accuracy calculation
result = data.frame(Observed = character(), Predicted = character())

for ( i in 1:10){
  
  testIndices = which( folds == i, arr.ind = TRUE )
  testData = data[testIndices, ]
  trainData = data[-testIndices, ]
  
  # Grow the tree
  tree = rpart(V11 ~ ., data = trainData, method = "class")
  
  # Predict
  for ( j in 1:nrow(testData)){
    test = testData[j, ]
    prediction = predict(tree, newdata = test[, 1:(ncol(test)-1)])
    
    if ( prediction[1] > prediction[2] ){
      p = classes[1]
    }
    else{
      p = classes[2]
    }
    
    result = rbind(result, data.frame(Observed  = test[, ncol(test)], Predicted = p))
    
  }
  
}

# Confusion Matrix
confusion = table(result$Observed, result$Predicted)

cat(c("\n\nConfusion Matrix : \n"))
print(confusion)

accuracy = sum(diag(confusion))/sum(confusion)

cat(c("\n\nAccuracy = \n"), accuracy)

cat("\n\n")