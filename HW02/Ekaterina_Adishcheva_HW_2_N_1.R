library("pracma")
X <- LakeHuron

diff = matrix(nrow=9, ncol=6)

breaks = seq(from=5, to=45, by=5)
kernels = eval(formals(density.default)$kernel)
for (i in 1:9) {
  H = hist(X, breaks=breaks[i], plot=FALSE)
  p_h <- function(x){
    H$density[max(which(H$breaks<x))]
  }
  for (j in 2:length(kernels)) {
    Dk <- density(X, kernel=kernels[j], from=min(X), to=max(X))
    diff[i, j-1] <- mean((sapply(unlist(Dk[1]), p_h)-unlist(Dk[2]))^2)
  }
}

break_min <- which(diff==min(diff), arr.ind=T)[1]
kernel_min <- which(diff==min(diff), arr.ind=T)[2]

H = hist(X,
         breaks=breaks[break_min],
         freq=FALSE, , main="LakeHuron")
Dk <- density(X, kernel=kernels[kernel_min+1], from=min(X), to=max(X))
lines(unlist(Dk[1]), unlist(Dk[2]), type="l",col="red")
legend(x = "topleft",
       legend = c(paste("Histogram with b =", breaks[break_min]),
                  paste("Density with Kernel =", kernels[kernel_min+1])),
       col = c("black", "red"), lwd = 2)



