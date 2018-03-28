#### Bayesian Classifier Model for Gender Data Set
#### Multivariate Normal Distribution

options(warn=-1)

data = data.frame(read.csv("Gender.csv", header = TRUE))

# Class labels
classes = unique(data[, ncol(data)])

# Observed vs Predicted
result = data.frame(Observed = character(), Predicted = character())
curr_res = 1

# Prior Probabilities
prior = list()
length(prior) = length(classes)
names(prior) = classes

# Calculate the Prior Probabilities - Done
for ( class in classes ){
  prior[[toString(class)]] = length( which( data[, ncol(data)] == class ) )/nrow(data)
}
cat(c("\n\nThe Prior Probabilities : \n\n"))
print(prior)

# Mean vectors
means = list()
length(means) = length(classes)
names(means) = classes

# calculate the Mean vectors - Done
for ( class in classes ){
  tempMean = list()
  length(tempMean) = ncol(data)-1
  
  for ( feature in 1:(ncol(data)-1) ){
    tempMean[feature] = mean(as.numeric(data[which( data[, ncol(data)] == class ), feature]))
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
  covarianceMatrices[[toString(class)]] = cov(data[which( data[, ncol(data)] == class ), 1:(ncol(data)-1) ])
}

cat(c("\n\nThe Covariance Matrices : \n\n"))
print(covarianceMatrices)

# Testing
test = c(6, 130, 8)

# Conditional Probabilities
conditionalProbabilities = list()
length(conditionalProbabilities) = length(classes)
names(conditionalProbabilities) = classes

for ( class in classes ){
  x_minus_mu = t(as.matrix(test - as.numeric(means[[toString(class)]])))
  mahalanobis = x_minus_mu %*% solve(covarianceMatrices[[toString(class)]]) %*% t(x_minus_mu)
  
  cat(c("\n\n The Mahalanobis Distance for Class ", class, " is : \n"))
  print(mahalanobis)
  
  pdf = ( (2 * 3.14) ^ (-1/2) * ( det( covarianceMatrices[[toString(class)]] ) ^ (-1/2) ) ) * exp(-1/2 * mahalanobis)
  cat(c("\n\n The PDF for Class ", class, " is : \n"))
  print(pdf)
  
  conditionalProbabilities[[toString(class)]] = pdf
  
}


cat(c("\n\n The Conditional Probabilities : \n"))
print(conditionalProbabilities)

# Calculate Posterior Probabilities
posterior = list()
for (class in classes){
  posterior[[toString(class)]] = conditionalProbabilities[[toString(class)]]*prior[[toString(class)]]/sum(as.numeric(conditionalProbabilities) * as.numeric(prior))
}

cat(c("\n\n The Posterior Probabilities is : \n"))
print(posterior)


