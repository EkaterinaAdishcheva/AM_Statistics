library("pracma")

# sample with gamma distribution
n = 1000
s = rgamma(n, 3, 4)


# x_k
M = 200
x = seq(from=0.01, to=2,length=M)
p <- function(x){
  dgamma(x, 3, 4)
}


legendre_kernel <- function(x){
  (1/2 - 5/8 * (3*x^2 - 1)) * (x < 1) * (x > -1)
}


bandwidth = seq(from=0.1, to=5, by=0.1)
mise_e = vector(len=50)
mise_l = vector(len=50)

for (i in 1:50) {
  D <- density(s, bw=bandwidth[i], kernel=c("epanechnikov"), from=0, to=2)
  p_hat <- approx(D$x, D$y, xout=x)
  mise_e[i] <- mean((p(unlist(p_hat[1])) - unlist(p_hat[2]))^2)
  
  p_l = vector(len=200)
  for (j in 1:M) {
    p_l[j] <- mean(legendre_kernel((x[j] - s)/bandwidth[i]))/bandwidth[i]  
  }
  mise_l[i] <- mean((p(x) - p_l)^2)
}
plot(mise_e, type="l",col="green")
lines(mise_l, type="l",col="blue")

legend(x = "topleft", legend = c("epanechnikov", "legendre"), col = c("green", "blue"), lwd = 2) 

