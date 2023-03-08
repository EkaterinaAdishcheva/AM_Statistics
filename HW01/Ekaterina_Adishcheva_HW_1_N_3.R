# Task 3

theta = 5
x_m = log(2)/5
sigma_m = (10*exp(-5*x_m))^(-2)
M = 100

sigma1 = rep(NA,M)
sigma2 = rep(NA,M)
sigma3 = rep(NA,M)

n = 1000
diff = rep(NA,M)
for (i in 1:M) {
  x = rexp(n, theta)
  diff[i] = sqrt(n)*(median(x)-x_m)
  sigma1[i] = sum((x - mean(x))^2)/(length(x) - 1)
}
qqnorm(diff, main="QQ-plot: n = 1000")
qqline(diff)

n = 10000
diff = rep(NA,M)

for (i in 1:M) {
  x = rexp(n, theta)
  diff[i] = sqrt(n)*(median(x)-x_m)
  sigma2[i] = sum((x - mean(x))^2)/(length(x) - 1)
}
qqnorm(diff, main="QQ-plot: n = 10000")
qqline(diff)

n = 100000
diff = rep(NA,M)
for (i in 1:M) {
  x = rexp(n, theta)
  diff[i] = sqrt(n)*(median(x)-x_m)
  sigma3[i] = sum((x - mean(x))^2)/(length(x) - 1)
}
qqnorm(diff, main="QQ-plot: n = 100000")
qqline(diff)


boxplot(sigma1, sigma2, sigma3, xaxt="n")
axis(side=1,at=1:3,label=c("1000","10000", "100000"))

