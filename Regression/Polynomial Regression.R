# Training Data
x = c(0, 2, 4, 6, 8)
y = c(0, 4, 16, 36, 64)

degree = 2

# Model building
# Ax = b
# x = A-1b

mat = matrix(nrow=degree+1, ncol=degree+1)

power = 0
for ( i in 1:(degree+1) ){
  power = i-1
  for ( j in 1:(degree+1) ){
    mat[i,j] = sum(x^power)
    power = power + 1
  }
}
mat[1,1] = length(x)

mat2 = matrix(nrow=degree+1, ncol=1)

power = 0;
for ( i in 1:(degree+1) ){
  mat2[i,1] = sum( (x^power)*y)
  power = power + 1
}

coeff = round(solve(mat, mat2))

print(coeff)
