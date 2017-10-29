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
a <- -pi/2
b <- pi/4
c <- -pi
d <- 2
u <- seq(a,b,length=100)
v <- seq(c,d,length=100)
f <-function(u,v){return (sin((cos(u)*sqrt(1+u^2+v^2))))}
z <- outer(u,v,f)
persp(u,v,z,theta = 100, phi = 20)
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

