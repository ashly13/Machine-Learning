#### Bayesian Classifier Model for Play Tennis Data Set

options(warn=-1)

# Read the data
data = read.csv("Play Tennis.csv", header=TRUE)
data = as.matrix(data)

rows = dim(data)[1]
cols = dim(data)[2]

# Take the last point as test data
test_data = as.vector(data[14, ])

# Unique class labels
classes = sort(unique(data[, cols]))

# Find the prior probability of each class - Done!
prior = vector(length=length(classes))
names(prior) = as.vector(classes)

for ( class in classes ){
  prior[toString(class)] = length(which(data[, cols] == class))/nrow(data)
}
cat(c("\n\nPrior Probabilities : \n\n"))
print(prior)

# Collection of Conditional Probability Tables - Done
cpt = list()

for ( feature_count in 1:(cols-1) ){
  f = sort(unique(data[, feature_count]))
  cpt1 = list()
  
  for (feature_value in f){
    temp = vector(length=length(classes))
    names(temp) = as.vector(classes)
    for ( class in classes ){
      temp[[toString(class)]] = length(which(data[, cols] == class & data[, feature_count] == feature_value ))/length(which(data[, cols] == class))
    }
    cpt1[[toString(feature_value)]] = temp
  }
  
  cpt[[toString(feature_count)]] = cpt1
}


cat(c("\n\n"))


# Posterior probabilities for the test_data
posterior = list()

each_class = vector(length=length(classes))
names(each_class) = as.vector(classes)

for (class in classes){
  each_class[[toString(class)]] = 1
  for ( feature in 1:(cols-1) ){
    feature_value = test_data[feature]
    each_class[[toString(class)]] = each_class[[toString(class)]] * cpt[[toString(feature)]][[feature_value]][[toString(class)]]
  }
  each_class[[toString(class)]] = each_class[[toString(class)]] * prior[[toString(class)]]
}

for (class in classes){
  posterior[[toString(class)]] = each_class[[toString(class)]]/sum(each_class)
}

cat(c("\n\nTest Data : \n\n"))
print(test_data)
cat(c("\n\nPosterior Probabilities for the test data : \n\n"))
print(posterior)
