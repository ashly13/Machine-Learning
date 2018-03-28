# Training Data
x = c(92, 94, 97, 98, 100, 102, 104, 105, 105, 107, 107, 110)
y = c(930, 900, 1020, 990, 1100, 1050, 1150, 1120, 1130, 1200, 1250, 1220)

# Model building
xbar = mean(x)
ybar = mean(y)

xy = x*y
x2 = x*x

b = ( 12*sum(xy) - sum(x)*sum(y)) / ( 12*sum(x2) - (sum(x)*sum(x)) )
a = ybar - b*xbar

cat(c("\nThe y-intercept is ", a, ".\nThe slope is ", b, "\n"))

# Measures calculation

# Total Sum of Squares
sst = sum((y - ybar)**2) ;
cat(c("\nThe Total Sum of Squares is ", sst))

# Regression Sum of Squares
ssr = sum(( (a+b*x) - ybar)**2)
cat(c("\nThe Regression Sum of Squares is ", ssr))

# Sum of Squares
sse = sum( ( y - (a+b*x) )**2)
cat(c("\nThe Sum of Squares is ", sse))
cat(c("\nChecking SST = SSE + SSR ; SSE + SSR = ", sse+ssr))

# Co-efficient of Determination
r2 = ssr/sst
cat(c("\nThe Co-efficient of Determination is ", r2, "\n\n\n"))

png(file="LinearRegression.png")
plot(x, y, col="red", xlab="Advertisement", ylab="Sales")
lines(x, a+b*x, col="blue")
dev.off()
