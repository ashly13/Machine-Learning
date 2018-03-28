# Read the data
data = read.csv("Dataset.csv", header = TRUE)

k = 5

# 10-fold Cross Validation
folds = cut(seq(1, nrow(data)), breaks=10, labels=FALSE)

# Observed vs Predicted
results = data.frame(Observed = character(), Predicted = character())

for ( fold in 1:10 ){
  cat(c("\n\n-------------------Fold ", fold, "\n"))
  testIndices = which(folds==fold, arr.ind=TRUE)
  testData = data[testIndices, ]
  trainData = data[-testIndices, ]
  
  for ( i in 1:nrow(testData) ){
    test = testData[i, ]
    distances = list()
    for ( j in 1:nrow(trainData) ){
      train = trainData[j, ]
      # Find the distances
      dist = 0
      for ( feature in 1:(ncol(trainData)-1) ){
        dist = dist + (test[, feature] - train[, feature]) ^ (2)
      }
      dist = sqrt(dist)
      distances[[j]] = dist
      
    }
    
    # Find Prediction
    yvals = character()
    for ( l in 1:k ){
      p = which.min(as.numeric(distances))
      yvals = cbind(yvals, toString(trainData[p, ncol(train)]))
      distances = distances[-p]
    }
    p1 = which.max(table(yvals))
    results = rbind(results, data.frame(Observed = toString(test[, ncol(test)]), Predicted = names(p1)))
  }
  
}

# Confusion Matrix
confusion = table(results)
cat(c("\n\nConfusion Matrix : \n"))
print(confusion)

# Accuracy
accuracy = (confusion[1, 3] + confusion[2, 1] + confusion[3, 2])/sum(confusion)
cat(c("\n\nAccuracy = ", accuracy))
cat(c("\n\n"))
