# We use phi(u,v) = 1+sin((cos(u)*sqrt(1+u^2+v^2)))
a <- -1 #0
b <- 0 # 3*pi/8
c <- -3 #-3
d <- -2 #-1
u <- seq(a,b,length=100)
v <- seq(c,d,length=100)
f <-function(u,v){return (1.8*(0.05+sin(cos(u)*sqrt(1+u^2+v^2))))}
z <- outer(u,v,f)
p <- function(u,v){return ((-u+0.5)*(v+3.5))}
w <- outer(u,v,p)
persp3d(u,v,z,col = "blue",alpha=0.5)
persp3d(u,v,w,col = "green", alpha=0.5,add = TRUE)
t <- 0.5
f_u <- function(u){return(-u+1/2)}
g_v <- function(v){return(v+3.5)}
F_u <- function(u){return(0.5*(-u^2+u+2))}
G_v <- function(v){return(0.5*(v^2+7*v+12))}
h_uv <- function(u,v){return(
  f_u(u)*g_v(v)*c_frank(F_u(u),G_v(v))
)}
z_cop <- outer(u,v,h_uv)
persp3d(u,v,z_cop,col="red",alpha=0.5, add=TRUE)


# phi(u,v) > 0
min(z)
max(z)
# Methode 1
K <- max(z)
n <- 10^3
U <- runif(n,min = a,max = b)
V <- runif(n,min = c,max = d)
W <- runif(n,0,K)
cst <- (b-a)*(d-c)*K # volume de cube
I1 <- cst*sum(f(U,V) > W)/n
print(I1)
evol1 <- cst*cumsum(f(U,V) > W)/(1:n)

# Methode 2
fres <- f(U,V)
I2 <- (b-a)*(d-c)*mean(f(U,V))
print(I2)
evol2 <- (b-a)*(d-c)*cumsum(f(U,V))/(1:n)

# Methode 3
# Importance Sampling with independent margins
Uis <- runif(n,0,1)
Vis <- runif(n,0,1)
Xis <- 1/2*(1-sqrt(9-8*Uis))
Yis <- -3.5+0.5*sqrt(8*Vis+1)
p <- function(u,v){return ((-u+0.5)*(v+3.5))}
I3 <- 1/n*sum(f(Xis,Yis)/p(Xis,Yis))
print(I4)
evol3 <- cumsum(f(Xis,Yis)/p(Xis,Yis))/(1:n)

# Methode 4
# Importance Sampling. Frank copula
U <- runif(n) 
Z <- runif(n)
V <- -1/t*log(1+exp(t*U)*Z*(exp(-t)-1)/(1-(1-exp(t*U))*Z))
X <- 1/2*(1-sqrt(9-8*U))
Y <- -3.5+0.5*sqrt(8*V+1)
I4 <- 1/n*sum(f(X,Y)/p(X,Y))
print(I4)
evol4 <- cumsum(f(X,Y)/p(X,Y))/(1:n)

# Double-check. Numeric integration
library(pracma)
quad2d(f,a,b,c,d)

plot(1:n,evol1,type="line")
lines(1:n,evol2,col="green")
lines(1:n,evol3,col="blue")
lines(1:n,evol4,col="darkmagenta")
abline(h=quad2d(f,a,b,c,d), col="red")


