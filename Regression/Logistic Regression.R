# Data Set
# y
y = c(0, 0, 0, 0, 0, 1, 1, 1, 1)

d = 2

x = matrix( nrow=length(y), ncol=d )
# x1
x[, 2] = c(2.5, 2.3, 4.4, 1.9, 3.0, 2.8, 2.1, 1.8, -0.2)
# x2
x[, 1] = c(2.7, 1.5, 3.4, 1.3, 3.1, 7.6, 5.3, 6.9, 8.7)


# Model Building - Multiple Regression

# (XT)X
xtx = matrix( nrow=d+1, ncol=d+1 )

xtx[1, 1] = length(y)

# Column 1
for ( i in 2:(d+1) ){
  xtx[i, 1] = sum( x[, i-1] )
  xtx[1, i] = sum( x[, i-1] )
}

factor = x[, 1]

for ( i in 2:(d+1) ){
  for ( j in 2:(d+1) ){
    xtx[i, j] = sum( x[, j-1] * factor )
  }
  factor = x[, j-1]
}

print("XTX")
print(xtx)

# (XT)Y
xty = matrix( nrow=d+1, ncol=1 )

xty[1, 1] = sum(y)

for ( i in 2:(d+1) ){
  xty[i, 1] = sum( y * x[, i-1])
}

print("XTY")
print(xty)

# Solve
coeff = solve(xtx, xty)
print(coeff)

# Logistic regression

test = c(1, 7.7, 3.5)

ebtx = exp( t(coeff) %*% test ) 

# Probability that y = 1
Py1 = ebtx/(1 + ebtx)
Py0 = 1/(1 + ebtx)
cat("\n\n")
print("P(y=1|x) = ")
print(Py1)
cat("\n\n")
print("P(y=0|x) = ")
print(Py0)
