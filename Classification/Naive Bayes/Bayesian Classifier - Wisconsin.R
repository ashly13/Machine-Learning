#### Bayesian Classifier Model for Wisconsin Breast Cancer Data Set

options(warn=-1)

# Read the data
data = read.csv("Wisconsin Breast Cancer.csv", header=FALSE)
data = data[ , 2:ncol(data)]

# Cleaning the data - Feature 6 has "?"
data[, 6] = as.numeric(as.character(data[, 6]))
data[ which(is.na(data[, 6])), 6 ] = round(mean(data[, 6], na.rm = TRUE))

# Split the dataset into training and test data

# Set Seed so that same sample can be reproduced in future also
set.seed(101) 

# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(data), size = floor(.75*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]

rows = dim(train)[1]
cols = dim(train)[2]

# Unique class labels
classes = sort(unique(train[, cols]))

# Find the prior probability of each class - Done!
prior = vector(length=length(classes))
names(prior) = as.vector(classes)

for ( class in classes ){
  prior[toString(class)] = length(which(train[, cols] == class))/nrow(train)
}
cat(c("\n\nPrior Probabilities : \n\n"))
print(prior)

'
# Collection of Conditional Probability Tables
cpt = list()

for ( feature_count in 1:(cols-1) ){
  f = sort(unique(train[, feature_count]))
  cpt1 = list()
  
  for (feature_value in f){
    temp = vector(length=length(classes))
    names(temp) = as.vector(classes)
    for ( class in classes ){
      temp[[toString(class)]] = length(which(train[, cols] == class & train[, feature_count] == feature_value )) / length(which(train[, cols] == class))
    }
    cpt1[[toString(feature_value)]] = temp
  }
  
  cpt[[toString(feature_count)]] = cpt1
}
'

'
# Collection of Conditional Probability Tables with Laplace Smoothing
cpt = list()

for ( feature_count in 1:(cols-1) ){
  f = sort(unique(train[, feature_count]))
  cpt1 = list()
  
  for (feature_value in f){
    temp = vector(length=length(classes))
    names(temp) = as.vector(classes)
    for ( class in classes ){
      temp[[toString(class)]] = ( length(which(train[, cols] == class & train[, feature_count] == feature_value )) + 1/length(f) ) / ( length(which(train[, cols] == class)) + 1 )
    }
    cpt1[[toString(feature_value)]] = temp
  }
  
  cpt[[toString(feature_count)]] = cpt1
}
'

# Collection of Conditional Probability Tables with Add One Smoothing
cpt = list()

for ( feature_count in 1:(cols-1) ){
  f = sort(unique(train[, feature_count]))
  cpt1 = list()
  
  for (feature_value in f){
    temp = vector(length=length(classes))
    names(temp) = as.vector(classes)
    for ( class in classes ){
      temp[[toString(class)]] = ( length(which(train[, cols] == class & train[, feature_count] == feature_value )) + 1 ) / ( length(which(train[, cols] == class)) + length(f) )
    }
    cpt1[[toString(feature_value)]] = temp
  }
  
  cpt[[toString(feature_count)]] = cpt1
}


# Predicted values
prediction = array(dim=nrow(test))

for ( i in 1:nrow(test) ){
  
  # Select the current row
  test_data = test[i, ]
  
  # Posterior probabilities for the test_data
  posterior = list()
  
  each_class = vector(length=length(classes))
  names(each_class) = as.vector(classes)
  
  for (class in classes){
    each_class[[toString(class)]] = 1
    for ( feature in 1:(cols-1) ){
      feature_value = test_data[feature]
      each_class[[toString(class)]] = each_class[[toString(class)]] * cpt[[toString(feature)]][[toString(as.numeric(feature_value))]][[toString(class)]]
    }
    each_class[[toString(class)]] = each_class[[toString(class)]] * prior[[toString(class)]]
  }
  
  for (class in classes){
    posterior[[toString(class)]] = each_class[[toString(class)]]/sum(each_class)
  }
  
  # Select the predicted class based on the posterior probabilities
  for ( class in classes ){
    if ( posterior[[toString(class)]] >= 0.5 ){
      prediction[i] = class
    }
  }
  
}

prediction = as.numeric(prediction)

# Confusion Matrix
confusion = table( test[, cols], prediction )

cat(c("\n\nConfusion Matrix : \n\n"))
print(confusion)


# Accuracy
accuracy = sum(diag(confusion))/sum(confusion)

cat(c("\n\nAccuracy = ", accuracy))

cat(c("\n\n"))
