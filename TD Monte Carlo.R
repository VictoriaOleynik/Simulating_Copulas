# TD4. Monte Carlo
# Ex. 1 

n <- 10^3
U <- runif(n, 0, 2)
V <- runif(n, 0, 4)
#plot(U,V)
f = function(x) x^2
X1 <- V < f(U)
I1 <- 8*mean(X)
evol1 <- cumsum(X)/(1:n)
plot(8*evol1,type = "line")#, ylim=c(pi-0,5,pi+0.5))
abline(h=8/3, col="red")

N <- 250
Is <- seq(1:N)
for (i in seq(1:N)){
  U <- runif(n, 0, 2)
  V <- runif(n, 0, 4)
  X <- V < f(U)
  Is[i] <- 8*mean(X)
}
hist(Is)

# apply-approach
U <- runif(n*N, 0, 2)
V <- runif(n*N, 0, 4)
X = matrix(V<f(U), nrow=n, ncol = N, byrow = F)
Is <- 8*apply(X, 2, mean)
hist(Is)

# 2me methode
ns <-  seq(1,10^3,length.out = 100)
I1s <- seq(1,length(ns))
I2s <- seq(1,length(ns))
for (j in 1:length(ns)){
  U <- runif(ns[j], 0, 2)
  V <- runif(ns[j], 0, 4)
  I1s[j] <- 8*mean(V < f(U))
  I2s[j] <- 2*mean(f(U))
}
plot(ns,I1s,type="line")
lines(ns,I2s,col="green")

# 3me methode. Importance sampling
# simuler X par methode d'inversion. Densite 1/2x ind(0,2)
n <- 10^3
U <- runif(n)
p = function(x) 0.5*x*(x<2)
x <- p(2*sqrt(U))
hist(x)
