# Task 2

theta = 2

n = 1000
x = rexp(n, theta)

N = 100
LEFT = 2
RIGHT = 2.5
t = seq(LEFT, RIGHT, (RIGHT-LEFT)/(N-1))

f_hat = rep(NA, N)

for (i in 1:N) {
  if (sum(x) > t[i]) {
    f_hat[i] = (1 - t[i] / sum(x))^(n - 1)
  } else {
    f_hat[i] = 0
  }
}

plot(t, f_hat, type="l", main = "Graphs f", xlim = c(LEFT, RIGHT),col="black")

f = rep(NA, N)
f = exp(-theta*t)
lines(t, f, type="l",col="red")

f_hat_3 = rep(NA, N)
x_1 = rexp(n, theta)
x_2 = rexp(n, theta)
x_3 = rexp(n, theta)
for (i in 1:N) {
  if(sum(x)>t[i] && sum(x_2)>t[i] && sum(x_3)>t[i]) {
    f_hat_3[i] = ((1-t[i]/sum(x_1))^(n-1) + (1-t[i]/sum(x_2))^(n-1) + (1-t[i]/sum(x_3))^(n-1))/3
  } else {
    f_hat_3[i] = 0
  }
}

lines(t, f_hat_3, type="l",col="green")

legend(x = "topright", legend = c("f_hat", "f", "f_hat_3"), col = c("black", "red", "green"), lwd = 2) 

