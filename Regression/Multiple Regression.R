# Data Set
# y
y = c(64, 73, 61, 76, 72, 80, 71, 83, 83, 89, 86, 93, 88, 95, 94, 100)

d = 2

x = matrix( nrow=length(y), ncol=d )
# x1
x[,1] = c(4, 4, 4, 4, 6, 6, 6, 6, 8, 8, 8, 8, 10, 10, 10, 10)
# x2
x[,2] = c(2, 4, 2, 4, 2, 4, 2, 4, 2, 4, 2, 4, 2, 4, 2, 4)


# Model Building

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

# (XT)Y
xty = matrix( nrow=d+1, ncol=1 )

xty[1, 1] = sum(y)

for ( i in 2:(d+1) ){
  xty[i, 1] = sum( y * x[, i-1])
}


# Solve
coeff = solve(xtx, xty)
print(coeff)