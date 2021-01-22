
library("car")  # load the 'car' package
data("Anscombe")  # load the data set
?Anscombe  # read a description of the data
head(Anscombe)  # look at the first few lines of the data
pairs(Anscombe)  # scatter plots for each pair of variables

library("coda")
library("rjags")

mod_string = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    	## Initial guess of variance based on overall
    	## variance of education variable. Uses low prior
    	## effective sample size. Technically, this is not
    	## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data_jags = list(education=Anscombe$education, income= Anscombe$income, young = Anscombe$income, urban = Anscombe$urban)
param1 = c( "b","sig")
inits1 = function(){
  inits=list("b"=rnorm(3,0.0,100.0),"prec"=rgamma(1,1.0,1.0))
}

mod1 =jags.model(textConnection(mod_string),data=data_jags,init=inits1,n.chains=3)


update(mod1,1000)

mod1_sim = coda.samples(model=mod1, variable.names=param1,n.iter=5000)

mod1_csim=do.call(rbind,mod1_sim)

summary(mod1_sim)

gelman.diag(mod1_sim)
autocorr.diag(mod1_sim)


#reference model
mod_lm = lm(education ~ income + young + urban, data = Anscombe)

plot(mod_lm)




mod_string = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*income[i]*young[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    	## Initial guess of variance based on overall
    	## variance of education variable. Uses low prior
    	## effective sample size. Technically, this is not
    	## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data_jags = list(education=Anscombe$education, income= Anscombe$income, young = Anscombe$young)
param1 = c( "b","sig")
inits1 = function(){
  inits=list("b"=rnorm(3,0.0,100.0),"prec"=rgamma(1,1.0,1.0))
}

mod1 =jags.model(textConnection(mod_string),data=data_jags,init=inits1,n.chains=3)


update(mod1,1000)

mod1_sim = coda.samples(model=mod1, variable.names=param1,n.iter=5000)

mod1_csim=do.call(rbind,mod1_sim)

summary(mod1_sim)

gelman.diag(mod1_sim)
autocorr.diag(mod1_sim)

dic.samples(mod1,n.iter=1e3)

