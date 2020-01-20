library(mvtnorm)
means=c(0,0)
sigma <- matrix(c(5,2,2,3), ncol = 2)
sigma
x <- rmvnorm(n = 1000, mean = means, sigma = sigma)
d <- data.frame(x)
p2 <- ggplot(d, aes(x = inv.logit(X1), y = inv.logit(X2))) +
  geom_point(alpha = .5) +
  geom_density_2d()
p2

cormatrix<-cov2cor(sigma)

#Conditional Distribution
#correlation between x1 and x2 = cormatrix[2,2]
rho = 0.8
#means for both are 0
mu_x1 = 0
mu_x2 = 0
#variances are the off diagonals
sigma_x2 = cormatrix[1,2]
sigma_x1 = cormatrix[2,1]
conditional_mu = (mu_x2 + rho * (sqrt(sigma_x2)/sqrt(sigma_x1)) * (1-mu_x2))
conditional_variance = sigma_x2 * (1-rho^2)
conditional_distribution = rnorm(1e5,conditional_mu,conditional_variance)
qplot(conditional_distribution)
mean(conditional_distribution)


##Pckage
library(condMVNorm)
# 10-dimensional multivariate normal distribution
n <- 14
A <- matrix(rnorm(n^2), n, n)
A <- A %*% t(A)
# density of Z[c(2,5)] given Z[c(1,4,7,9)]=c(1,1,0,-1)
dcmvnorm(x=c(1.2,-1), mean=rep(1,n), sigma=A,
         dependent.ind=c(2,5), given.ind=c(1,4,7,9),
         X.given=c(1,1,0,-1))
