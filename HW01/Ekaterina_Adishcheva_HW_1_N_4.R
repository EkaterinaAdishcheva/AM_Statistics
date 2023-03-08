# Task 4

suppressPackageStartupMessages({
  library(waveslim)
})

#?ibm
data(ibm)
x = ibm[1:369]
y = rep(NA,368)

for (i in 1:368) {
  y[i] = (log(x[i+1]/x[i]))^2
}

plot(y)

# 1
theta = mean(y)

# 2
lambda = 1/mean(y)

#
y_order = sort(y)
x = rep(NA,368)
for (i in 1:368) {
  x[i] = i/368
}
plot(y_order, x,type="l")


F1 = rep(NA, N)
F1 = pnorm(sqrt(y_order), 0, theta) - pnorm(-sqrt(y_order), 0, theta)
lines(y_order, F1, type="l",col="red")

F2 = rep(NA, N)
F2 = 1 - exp(-lambda*y_order)
lines(y_order, F2, type="l",col="blue")
        
legend(x = "topright", legend = c("actual", "F1", "F2"), col = c("black", "red", "blue"), lwd = 2) 
      
      