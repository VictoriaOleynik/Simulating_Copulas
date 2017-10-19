#  Chapitre 1

# Ex. 1. Simulate Weibull distribution
# generate by R
w <- rweibull(1000,shape=5,scale=1)
hist(w)

# simulation
n <- 1000
a <- 1
b <- 5
U <- runif(n)
w_s <- rep(0,n)
w_s <- a*(-log(1-U))^(1/b)
hist(w_s)
plot(density(w_s))


# Ex. 2 Simulate Cauchy distribution
# by R
c <- rcauchy(1000,location = 0, scale=1)
hist(c)

#simulation
n <- 1000
U <- runif(n)
c_s <- tan(pi*(U-1/2))
hist(c_s)
plot(density(c_s))


# Ex. 4 Simulate Pareto distribution
# by inversion
theta <- 1
alpha <- 100
n <- 1000
U <- runif(n)
p_s <- theta/(U)^(1/alpha)
hist(p_s)

#

