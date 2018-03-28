# Principal Component Analysis

x1 = c(4,2,2,3,4,9,6,9,8,10)
x2 = c(1,4,3,6,4,10,8,5,7,8)

x = matrix(ncol=2, nrow=10)
x[, 1] = cbind(x1)
x[, 2] = cbind(x2)

mx1 = mean(x1)
mx2 = mean(x2)

x1_mx1 = x1 - mx1
x2_mx2 = x2 - mx2

covariance = matrix(nrow=2, ncol=2)
covariance[1, 2] = cov(x1_mx1, x2_mx2)
covariance[2, 1] = cov(x1_mx1, x2_mx2)
covariance[1, 1] = var(x1_mx1)
covariance[2, 2] = var(x2_mx2)

cat(c("\n\nEigen Values : "))
eigen_values = eigen(covariance)$values
print(eigen_values)

cat(c("\n\nEigen Vectors : "))
eigen_vectors = eigen(covariance)$vectors[, which.max(eigen_values)]
print(eigen_vectors)

# Find the new data points
new_data = matrix(nrow=10, ncol=1)
for ( i in 1:10){
  new_data[i, 1] = x[i, 1] * eigen_vectors[1] + x[i, 2] * eigen_vectors[2]
}

cat(c("\n\nOriginal Data  : \n\n"))
print(x)

cat(c("\n\nData after reducing the features using Principal Component Analysis : \n\n"))
print(new_data)