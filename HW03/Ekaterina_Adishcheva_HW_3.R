library('plyr')

# TASK 2
calls <- c(1, 1, 7, 16, 8, 8, 11, 7, 5, 45, 13, 0, 36, 15, 4, 15, 7, 39, 6, 91, 28, 7, 0, 2,
           9, 2, 6, 1, 4, 83, 2, 3, 5, 34, 1, 1, 2, 0, 11, 79, 2, 2, 4, 1, 3, 0, 2, 2, 17, 55, 8,
           9, 20, 23, 16, 3, 5, 5, 4, 84, 1, 20, 1, 1, 20, 0, 19, 17, 5, 66, 0, 2, 5, 1, 26,
           14, 1, 0, 9, 88, 4, 11, 4, 2, 1, 32, 21, 2, 15, 76, 44, 8, 16, 12, 1, 9)

lambda_ml <- mean(calls)

b <- c(0, 2, 5, 9, 20)

expected_prob <- array(data=0.0, dim=5)
observed <- array(data=0, dim=5)

for (i in 1:4) {
  for (j in b[i]:(b[i+1] - 1)) {
    ind <- which(count(calls)[1] == j)
    if (length(ind) > 0) { 
      observed[i] = observed[i] + count(calls)[ind[1], 2] # observed
    }
    expected_prob[i] = expected_prob[i] + dpois(x=j, lambda=lambda_ml) # probs
  }
}

expected_prob[5] = 1.0 - sum(expected_prob[1:4]) # probs
observed[5] = length(calls) - sum(observed[1:4]) # observed


chisq.test(observed, p=expected_prob)

#As p-value < 0.05 we conclude that there is real evidence to suggest 
# the data DO NOT follow a Poisson distribution
# (In case of Poisson distribution the probability to get our data is p-value)

#-------
  
# TASK 3

qnt <- c(1, 86, 111)
observed <- c(6, 70, 14)


# i
prob_1 <- function(lambda) {
  prob <- 0
  for (i in qnt[1]:(qnt[2] - 1)) {
    prob = prob + dpois(i, lambda)   
  }
  return(prob)
}

prob_2 <- function(lambda) {
  prob <- 0
  for (i in qnt[2]:(qnt[3] - 1)) {
    prob = prob + dpois(i, lambda)   
  }
  return(prob)
}

prob_3 <- function(lambda) {
  prob <- 1 - prob_1(lambda) - prob_2(lambda) 
  return(prob)
}

stat <- function(lambda) {
  res = (observed[1] - sum(observed)*prob_1(lambda))^2/(sum(observed)*prob_1(lambda))
  res = res + (observed[2] - sum(observed)*prob_2(lambda))^2/(sum(observed)*prob_2(lambda))
  res = res + (observed[3] - sum(observed)*prob_3(lambda))^2/(sum(observed)*prob_3(lambda))
  return(res)
}

hat_lambda = optimize(stat, interval = c(0, 500))$minimum

# ii

expected_prob <- c(prob_1(hat_lambda), prob_2(hat_lambda), prob_3(hat_lambda))

chisq.test(observed, p=expected_prob)

K <- 3
dim <- 1

pchisq(chisq.test(observed, p=expected_prob)$statistic, df=(K-dim-1), lower.tail=FALSE)

# As p-value > 0.05 we conclude that there is NO real evidence to suggest 
# the data DO NOT follow a Poisson distribution
# (In case of Poisson distribution the probability to get our data is p-value)


#-------

# TASK 4

random_sample <- c(0, 1, 1, 4, 5, 8, 4, 9, 5, 1, 5, 5, 9, 6, 7, 2, 6, 2, 5, 4)

# i contingency table (sub_array and sub_set is independent)
sub_array <- c(1, 6, 11, 16, 21) 
sub_set <- c(0, 5, 10)

observed <- matrix(NA, ncol=4, nrow=2)

sub_set[2]:(sub_set[2+1]-1)

for (i in 1:4) {
  for (j in 1:2) {
    observed[j,i] = length(
      which(random_sample[sub_array[i]:(sub_array[i+1]-1)] %in% sub_set[j]:(sub_set[j+1]-1))
      )
  }
}

chisq.test(observed)
chisq.test(observed)$expected # is not what is really expected

xi_square <- sum((observed - 2.5)^2/2.5)

pchisq(xi_square, df=3, lower.tail=FALSE)

# As p-value > 0.05 we conclude that there is NO real evidence to suggest 
# the subarray and subset IS NOT independent
# (In case of independent subarray and subset the probability to get our data is p-value)


# ii intervals
b <- c(0, 3, 7)

expected_prob <- array(data=0.0, dim=3)
observed <- array(data=0, dim=3)

for (i in 1:2) {
  for (j in b[i]:(b[i+1] - 1)) {
    ind <- which(count(random_sample)[1] == j)
    if (length(ind) > 0) { 
      observed[i] = observed[i] + count(random_sample)[ind[1], 2] # observed
    }
    expected_prob[i] = expected_prob[i] + 0.1 # probs
  }
}

expected_prob[3] = 1.0 - sum(expected_prob[1:2]) # probs
observed[3] = length(random_sample) - sum(observed[1:2]) # observed


chisq.test(observed, p=expected_prob)

# As p-value > 0.05 we conclude that there is NO real evidence to suggest 
# the data IS NOT random
# (In case of random generator the probability to get our data is p-value)

#-------

# TASK 6

f_monotone <- function(x) {
  res <- sin(pi * x / 2)
  return(res)
}

f_monotone_error <- function(x) {
  res <- sin(pi * x / 2) + rnorm(1, 0, 0.2)
  return(res)
}

f_unmonotone_error <- function(x) {
  res <- sin(pi * x) + rnorm(1, 0, 0.2)
  return(res)
}

cor_val <- array(NA, dim = c(3,3,20))
cor_test_pval <- array(NA, dim = c(3,3,20))

methods <- c("pearson", "kendall", "spearman")

for (i in 1:3) {
  for (k in 1:20) {
    x <- runif(1000, min = 0, max = 1)
    y <- sapply(x, f_monotone)
    cor_val[1, i, k] = cor(x, y, method=methods[i])[1]
    cor_test_pval[1, i, k] = cor.test(x, y, method=methods[i])$p.value
  }
  for (k in 1:20) {
    x <- runif(1000, min = 0, max = 1)
    y <- sapply(x, f_monotone_error)
    cor_val[2, i, k] = cor(x, y, method=methods[i])
    cor_test_pval[1, 2, k] = cor.test(x, y, method=methods[i])$p.value
  }
  for (k in 1:20) {
    x <- runif(1000, min = 0, max = 1)
    y <- sapply(x, f_unmonotone_error)
    cor_val[3, i, k] = cor(x, y, method=methods[i])
    cor_test_pval[3, i, k] = cor.test(x, y, method=methods[i])$p.value
  }
}

boxplot(cor_test_pval[1, 1, ], cor_test_pval[1, 2, ], cor_test_pval[1, 3, ],
        main = "Monotone")
axis(side=1,at=1:3,label=c("pearson", "kendall", "spearman"))
# Since this value is less than .05,
# we do not have sufficient evidence to say that
# there is no correlation between two variables

boxplot(cor_test_pval[2, 1, ], cor_test_pval[2, 2, ], cor_test_pval[2, 3, ],
        main = "Monotone with error")
axis(side=1,at=1:3,label=c("pearson", "kendall", "spearman"))
# For pearson and kendall cor tests
# we do not have sufficient evidence to say that
# there is no correlation between two variables
# For spearman cor test
# we have sufficient evidence to say that
# there is no correlation between two variables

boxplot(cor_test_pval[3, 1, ], cor_test_pval[3, 2, ], cor_test_pval[3, 3, ],
        main = "Unmonotone with error")
axis(side=1,at=1:3,label=c("pearson", "kendall", "spearman"))
# For all cor tests
# we have sufficient evidence to say that
# there is no correlation between two variables

