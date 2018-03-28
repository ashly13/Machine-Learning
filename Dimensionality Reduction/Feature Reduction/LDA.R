# Linear Discriminant Analysis

data = matrix(nrow = 10, ncol = 3)
data[, 1] = c(4, 2, 2, 3, 4, 9, 6, 9, 8, 10)
data[, 2] = c(1, 4, 3, 6, 4, 10, 8, 5, 7, 8)
data[, 3] = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0)

# Split based on class labels
data0 = data[which(data[, 3] == 0), ]
data1 = data[which(data[, 3] == 1), ]

# Means 
mean0 = list()
mean0[[1]] = mean(data0[, 1])
mean0[[2]] = mean(data0[, 2])

mean1 = list()
mean1[[1]] = mean(data1[, 1])
mean1[[2]] = mean(data1[, 2])

# Scatter Matrices
s0 = cov(data0[, 1:2])*( nrow(data0) - 1 )
s1 = cov(data1[, 1:2])*( nrow(data1) - 1 )

# Sw and Sb
sw = s0 + s1
sb = ( as.numeric(mean1) - as.numeric(mean0) ) %*% t( as.numeric(mean1) - as.numeric(mean0) )

a = solve(sw) %*% sb
eigen_value = max(eigen(a)$values)
eigen_vector = eigen(a)$vectors[, which.max(eigen(a)$values)]

# Dimensionality Reduced Dataset
new_data = t(t(as.matrix(eigen_vector)) %*% t(data[, 1:2]))

cat(c("\n\nThe Original Dataset : \n\n"))
print(data[, 1:2])

cat(c("\n\nThe Dimensionally Reduced Dataset : \n\n"))
print(new_data)