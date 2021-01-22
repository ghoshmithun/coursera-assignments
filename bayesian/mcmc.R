

m=1000
a=2.0
b=1.0/3.0

theta = rgamma(n=m, shape=a,rate = b)

hist(theta,freq=FALSE)
curve(dgamma(x,shape = a, rate = b), col='blue', add=TRUE)

mean(theta)

a/b

m=10000
val <-rbeta(n=m,shape1=5.0,shape2 = 3.0)

v<-mean(val)

v_vec <- val/(1-val)

mean(v_vec)
mean(v_vec > 1.0)

norm_vec <- rnorm(10000)
quantile(norm_vec,probs = 0.3)

qnorm(p=0.3)
