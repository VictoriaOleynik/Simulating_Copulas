# Ex. 5
# inversion
n <- 10^5
a <- 1
b <- 4
c <- 6.5
d <- 7
l <- b-a+d-c
U <- runif(n) 

x <- l*U+a +(c-b)*(U>((b-a)/l))
hist(x)
plot(density(x))

# rejet
x <- rep(0,n)
for (i in 1:n){
  u <- runif(1, min = a, max = d)
  while ((u>b)&(u<c)){
    u <- runif(1, min = a, max = d)
  }
  x[i] <- u
}
hist(x)

# Ex. 6
a <- 0
b <- 3
c <- 1/18
d <- 1/4
n <- 10^5

# inversion
U <- runif(n)
x <- (-d+sqrt(d^2+2*c*U))/c
hist(x, xlim = c(-1,5))
plot(density(x), xlim = c(-1,5))

# rejet
a <- 0
b <- 3
c <- 1/18
d <- 1/4
n <- 10^5
x <- rep(0,n)
K <- max(a*c+d,b*c+d)
tic <- 0
for (i in 1:n){
  u <- 0
  v <- 10^6
  while (c*u+d <= v){
    u <- runif(1, min = a, max = b)
    v <- runif(1, min = 0, max = K)
    tic <- tic+1
  }
  x[i] <- u
}
hist(x, xlim = c(-1,5))


# Ex. 7. Simulation de couple
n <- 10^3
y <- rexp(n,rate=1)
U <- runif(n) 
x <- U^(1/y) # once I know Y (which follows its marginal dsbtn), I can generate X knowing Y
plot(x,y)

# to plot density
x <- seq(0.05,1,length=30)
y <- seq(0.05, 10,length=30)
f <- function(x,y){return(y*x^(y-1)*exp(-y))}
z <- outer(x,y,f)
persp(x, y, z, phi=20, theta = 120)

# Ex. 8
N <- 10^3
n <- 10
p1 <- 0.6
p2 <- 0.3
p3 <- 1-(p1+p2)
X1 <- rbinom(N,n,p1)
X2 <- rbinom(N,n-X1,p2/(1-p1))
X3 <- n-X1-X2
plot(X1,X2)
res <- sum(X3)/N-n*p3 # residual check LLN
abs(res)

toto <- cumsum(X3)/(1:N)
plot(toto,type='line')
abline(h=n*p3)
