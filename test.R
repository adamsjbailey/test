
a <- 6
b <- -1
n<- 500
p<-5/38
mu <- n*(a*p + b*(1-p))
mu
sigma <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
sigma

pnorm(0,mu,sigma)

p <- seq(0.25, 0.95, 0.05)

mu<- 44*p
sigma<- sqrt(p*(1-p))*sqrt(44)
t<-1-pnorm(35,mu,sigma)


p <- seq(0.25, 0.95, 0.05)
exp_val <- sapply(p, function(x){
  mu <- n * a*x + b*(1-x)
  sigma <- sqrt(n) * abs(b-a) * sqrt(x*(1-x))
  1-pnorm(35, mu, sigma)
})

exp_val
min(p[which(t > 0.8)])

