#### Bayesian Classifier Model for Iris Data Set
#### Multivariate Normal Distribution

options(warn=-1)

data = data.frame(read.csv("Iris.csv", header = FALSE))

# Class labels
classes = unique(data[, ncol(data)])

# 10-fold cross validation
folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)

# 10 models
models = list()
length(models) = 10
names(models) = as.character(1:10)

# Observed vs Predicted
result = data.frame(Observed = character(), Predicted = character())
curr_res = 1

for ( i in 1:10 ){
  testIndices = which( folds == i, arr.ind = TRUE )
  trainData = data[-testIndices, ]
  testData = data[testIndices, ]
  
  # Prior Probabilities
  prior = list()
  length(prior) = length(classes)
  names(prior) = classes
  
  # Calculate the Prior Probabilities - Done
  for ( class in classes ){
    prior[[toString(class)]] = length( which( trainData[, ncol(data)] == class ) )/nrow(trainData)
  }
  
  cat(c("\n\n---------------Fold ", i, "---------------\n\n"))
  print(prior)
  
  # Mean vectors
  means = list()
  length(means) = length(classes)
  names(means) = classes
  
  # calculate the Mean vectors - Done
  for ( class in classes ){
    tempMean = list()
    length(tempMean) = ncol(trainData)-1
    
    for ( feature in 1:(ncol(data)-1) ){
      tempMean[feature] = mean(as.numeric(trainData[which( trainData[, ncol(trainData)] == class ), feature]))
    }
    
    means[[toString(class)]] = t(as.matrix(tempMean))
  }
  
  cat(c("\n\nThe Mean Vectors : \n\n"))
  print(means)
  
  # Covariance Matrices
  covarianceMatrices = list()
  length(covarianceMatrices) = length(classes)
  names(covarianceMatrices) = classes
 
  for ( class in classes ){
    covarianceMatrices[[toString(class)]] = cov(trainData[which( trainData[, ncol(data)] == class ), 1:(ncol(trainData)-1) ])
  }
  
  cat(c("\n\nThe Covariance Matrices : \n\n"))
  print(covarianceMatrices)
  
  for ( j in 1:nrow(testData) ){
    
    test = testData[j, ]
    
    # Conditional Probabilities
    conditionalProbabilities = list()
    length(conditionalProbabilities) = length(classes)
    names(conditionalProbabilities) = classes
    
    for ( class in classes ){
      x_minus_mu = as.matrix(test[, 1:4] - means[[toString(class)]])
      mahalanobis = x_minus_mu %*% solve(covarianceMatrices[[toString(class)]]) %*% t(x_minus_mu)
      
      #cat(c("\n\n The Mahalanobis Distance for Class ", class, " is : \n"))
      #print(mahalanobis)
      
      pdf = ( det( 2*3.14*covarianceMatrices[[toString(class)]] ) ^ (-1/2) ) * exp(-1/2 * mahalanobis)
      #cat(c("\n\n The PDF for Class ", class, " is : \n"))
      #print(pdf)
      
      conditionalProbabilities[[toString(class)]] = pdf
      
    }
    
    # Calculate Posterior Probabilities
    posterior = list()
    for (class in classes){
      posterior[[toString(class)]] = conditionalProbabilities[[toString(class)]]*prior[[toString(class)]]/sum(as.numeric(conditionalProbabilities) * as.numeric(prior))
    }
    
    cat(c("\n\n The Posterior Probabilities is : \n"))
    print(posterior)
    
    # Find the prediction
    for ( cl in classes ){
      if ( posterior[[toString(cl)]] == max(as.numeric(posterior)) ){
        result = rbind(result, data.frame(Observed = toString(test$V5), Predicted = toString(cl)))
        #result[[curr_res]] = c(toString(test$V5), cl)
        #curr_res = curr_res + 1
      }
    }
    
  } 
}

# Print the Observed vs. Predicted
cat(c("\n\nResults : ", "\n  Observed \t\t Predicted\n"))
print(table(result$Observed, result$Predicted))
cat("\n\n")