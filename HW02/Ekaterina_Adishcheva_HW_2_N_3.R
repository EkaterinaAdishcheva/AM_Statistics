library(mixtools)

N = 1000
eta = sample(1:6, size=N, prob=c(0.5,rep(0.1,5)), replace=TRUE)
m = c(0,seq(from=-1, to=1, by=0.5))
s = c(1,rep(0.1,5))

X = rep(NA, N)
for (k in 1:N){
  X[k]=rnorm(1, mean=m[eta[k]], sd=s[eta[k]])
}

p = function(x){
  y = 0.5 * dnorm(x,0,1)
  for (j in 0:4){
    y = y + 0.1 * dnorm(x, mean=m[j+2], sd=s[j+2])
  }
  return(y)
}

# i
ll = rep(NA, 9)
for (k in 2:10) {
  gm <- normalmixEM(X, k=k, maxit=2500)
  ll[k-1] <- gm$loglik
  print(gm$loglik)
}
which(ll==max(ll))[1]

gm <- normalmixEM(X, k=(which(ll==max(ll))[1]+1), maxit=2500)

p_em <- function(x){
  y = 0
  for (j in 1:(which(ll==max(ll))[1] + 1)){
    y = y + gm$lambda[j] * dnorm(x, mean=gm$mu[j], sd=gm$sigma[j])
  }
  return(y)
}

u=seq(from=-3.5,to=3.5,length=1000)
plot(sapply(u,p_em)~u,type="l",col="red", ylim=c(0,max(sapply(u,p_em))))
lines(sapply(u,p)~u,type="l",col="green")

legend(x = "topright", legend = c("Bart Simpson (EM)", "Bart Simpson (actual))"), col = c("red", "green"), lwd = 2) 

# ii
D_ucv <- density(X, bw = "ucv", from=-3, to=3 )
D_bcv <- density(X, bw = "bcv", from=-3, to=3 )
plot(unlist(D_ucv[1]), unlist(D_ucv[2]),type="l",col="blue")
lines(unlist(D_bcv[1]), unlist(D_bcv[2]),type="l",col="red")
lines(sapply(u,p)~u,type="l",col="green")

legend(x = "topleft", legend = 
         c("Bart Simpson (kernel ucv)",
           "Bart Simpson (kernel bcv)",
           "Bart Simpson (actual)"), col = c("blue", "red", "green"),
       lwd = 2) 

mean((p_em(unlist(D_ucv$x)) - D_ucv$y)^2)
mean((p_em(unlist(D_bcv$x)) - D_bcv$y)^2)
