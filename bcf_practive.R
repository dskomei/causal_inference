library(bcf)


p <- 3
n <- 250

set.seed(1)

x <- matrix(rnorm(n*p), nrow=n)

q <- -1 * (x[,1]>x[,2]) + 1*(x[,1]<x[,2])


pi <- pnorm(q)
z <- rbinom(n, 1, pi)

tau <- (0.5*(x[, 3] > -3/4) + 0.25*(x[,3] > 0) + 0.25*(x[,3]>3/4))

mu <- (q + tau*z)

sigma <- diff(range(q + tau*pi)) / 8


y <- mu + sigma * rnorm(n)

pihat <- pnorm(q)

bcf_fit <- bcf(y, z, x, x, pihat, nburn=2000, nsim=2000)


tau_post <- bcf_fit$tau
tauhat <- colMeans(tau_post)
plot(tau, tauhat)
abline(0, 1)

