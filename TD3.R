# TD 3
# Ex. 1
# Copule elliptique
# on desine c(u,v)
u <- seq(0.05,1,length=100)
v <- seq(0.05,1,length=100)
c <-function(u,v){return (2*u*v/(u+v-u*v)^3)}
z <- outer(u,v,c)
persp(u,v,z)
# on simule n couples (U,V) et plot
n <- 10^3
U <- runif(n) 
Z <- runif(n)
V <- sqrt(Z)*U/(1+sqrt(Z)*U-sqrt(Z))
plot(U,V)
# on calcule tau kendall empirique

#tau <- 4/(n*(n-1))*
# on simule couple (X,Y) comme inv(F(U)),inv(G(V))
X <- log(U/(1-U))
Y <- log(V/(1-V))
plot(X,Y)
# on desine densite h(s,t)
s <- seq(-5,5,length=100)
t <- seq(-5,5,length=100)
h <-function(s,t){return (2*exp(-s-t)/(1+exp(-s)+exp(-t))^3)}
z <- outer(s,t,h)
persp(s,t,z)

# Simuler copule de Franck
theta <- -1
n <- 10^4
U <- runif(n) 
Z <- runif(n)
V <- -1/theta*log(1+exp(theta*U)*Z*(exp(-theta)-1)/(1-(1-exp(theta*U))*Z))
plot(U,V)
# on desine c(u,v)
u <- seq(0.05,1,length=100)
v <- seq(0.05,1,length=100)
c_fr <- function(u,v){ 
  return (exp(-theta*u)*(-theta*exp(-theta*v))*(exp(-theta)-1)/(exp(-theta)-1+(exp(-theta*u)-1)*(exp(-theta*v)-1))^2)
  }
z <- outer(u,v,c_fr)
persp(u,v,z)
# on fait des marges exponentielles F(x)=1-exp(-x), G(y)=1-exp(-y)
theta <- 15
n <- 10^4
U <- runif(n) 
Z <- runif(n)
V <- -1/theta*log(1+exp(theta*U)*Z*(exp(-theta)-1)/(1-(1-exp(theta*U))*Z))
X <- qnorm(U)
Y <- qexp(V)
plot(X,Y)
# on desine densite h(x,y)=f(x)g(y)*c(F(x)*G(y))
x <- seq(0.05,10,length=100)
y <- seq(0.05,10,length=100)
h_fr <- function(x,y){ 
  return (dexp(x)*dexp(y)*c_fr(pexp(x),pexp(y)))
}
z <- outer(x,y,h_fr)
persp(x,y,z)

# Copule FGM. Methode de rejet
n <- 10^3
theta <- 1 # in [-1;1]
K <- 1+abs(theta)
fgm <- function(u,v) { return(1+theta*(1-2*u)*(1-2*v)) }
U <- rep(0,n)
V <- rep(0,n)
for (i in 1:n){
  u <- 10^5
  v <- 0
  z <- 0
  while (z>fgm(u,v)){
    u <- runif(1)
    v <- runif(1)
    z <- runif(1, min = 0, max = K)
  }
  U[i] <- u
  V[i] <- v
}
plot(U,V)
# on desine c(u,v)
u <- seq(0.05,1,length=100)
v <- seq(0.05,1,length=100)
z <- outer(u,v,fgm)
persp(u,v,z)

