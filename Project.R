# Simulations et copules
# PROJET

# S = [-5, 3]x[1/3, 4]
# phi(x,y) =  sin((1+sqrt(x^2+y^2)))

# Methode 1
# Chose beautiful function
u <- seq(-pi/2,pi/4,length=100)
v <- seq(-pi,2,length=100)
c1 <-function(u,v){return (sin(abs(u)+abs(v)))}
c2 <-function(u,v){return (sin(u)+sin(v))}
c3 <-function(u,v){return ((u^2+3*v^2)*exp(-u^2-v^2))}
c4 <-function(u,v){return (-3*v/(u^2+v^2+1))} # very good
c5 <-function(u,v){return (v/(u^2+v^2))}
c6 <-function(u,v){return (asin(u^2+v^2-2))}
c7 <-function(u,v){return (1/(1+u^2+v^2))}
c8 <-function(u,v){return (exp(v/u))}
c9 <-function(u,v){return (u^3-v)}
c10 <-function(u,v){return (u*v^2-u^3)}
c11 <-function(u,v){return (u*v^3-v*u^3)}
c12 <-function(u,v){return (sin(u*v))} # very interesting
c13 <-function(u,v){return (exp(u)*cos(v))}
c14 <-function(u,v){return (sin(u-v))} 
c15 <-function(u,v){return (-(1-u^2)*(1-v^2))} # not bad
c16 <-function(u,v){return (sin(u)-sin(v))} # cool! check it's not multiplication
c17 <-function(u,v){return (u*v/(u^2+v^2))}
c18 <-function(u,v){return ((u+v)/(1+u^2+v^2))}
c19 <-function(u,v){return (-log(sqrt(u^2+v^2+1)))}
c20 <-function(u,v){return (exp(sqrt(u^2+v^2)))}
c21 <-function(u,v){return (sin((1+sqrt((u^2)+(v^2)))))} # super!
c22 <-function(u,v){return (u*sin(u+2*v))}
z <- outer(u,v,c21)
persp(u,v,z,theta = 60, phi = 30)

# We use c21
a <- -1 #0
b <- 0 # 3*pi/8
c <- -3 #-3
d <- -2 #-1
u <- seq(a,b,length=100)
v <- seq(c,d,length=100)
f <-function(u,v){return (1.8*(0.1+sin(cos(u)*sqrt(1+u^2+v^2))))}
z <- outer(u,v,f)
p <- function(u,v){return ((-u+0.5)*(v+3.5))}
w <- outer(u,v,p)
persp3d(u,v,z,col = "blue",alpha=0.2)
persp3d(u,v,w,col = "green", alpha=0.3,add = TRUE)

# Methode 1
K <- max(z)
n <- 10^3
U <- runif(n,min = a,max = b)
V <- runif(n,min = c,max = d)
W <- runif(n,0,K)
cst <- (b-a)*(d-c)*K # volume de cube
I1 <- cst*sum(f(U,V) > W)/n
print(I1)

# Methode 2
fres <- f(U,V)
I2 <- (b-a)*(d-c)*mean(f(U,V))
print(I2)

# Methode 3
# Importance Sampling
Uis <- runif(n,0,1)
Vis <- runif(n,0,1)
Xis <- 1/2*(1-sqrt(9-8*Uis))
hist(Xis)
Yis <- -3.5+0.5*sqrt(8*Vis+1)
hist(Yis)
p <- function(u,v){return ((-u+0.5)*(v+3.5))}
I4 <- 1/n*sum(f(Xis,Yis)/p(Xis,Yis))
print(I4)
evol4 <- cumsum(f(Xis,Yis)/p(Xis,Yis))/(1:n)
plot(Xis,Yis)

# Double-check. Numeric integration
library(pracma)
quad2d(f,a,b,c,d)

