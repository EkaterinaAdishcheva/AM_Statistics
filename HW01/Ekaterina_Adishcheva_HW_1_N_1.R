# Task 1

M = 100
n = 999
mu = 5
sigma = 1

mu_hat = matrix(data=NA,nrow=M,ncol=2)
sigma_hat = matrix(data=NA,nrow=M,ncol=2)
for (i in 1:M) {
  x = rlaplace(n, mu, sigma)
  mu_hat[i,1] = median(x)
  mu_hat[i,2] = mean(x)
  

  sigma_hat[i, 1] = sum(abs(x - mu_hat[i,1])) * 1/n
  sigma_hat[i, 2] = sqrt((sum(x^2) * 1/n - mu_hat[i,2]^2)/2)
}

mu_ml = median(mu_hat[,1])
mu_mm = median(mu_hat[,2])

boxplot(mu_hat,col="green", xaxt="n")
axis(side=1,at=1:2,label=c("ML","MM"))
title('mu_hat')




sigma_ml = median(sigma_hat[,1])
sigma_mm = median(sigma_hat[,2])


boxplot(sigma_hat,col="blue", xaxt="n")
axis(side=1,at=1:2,label=c("ML","MM"))
title('sigma_hat')

