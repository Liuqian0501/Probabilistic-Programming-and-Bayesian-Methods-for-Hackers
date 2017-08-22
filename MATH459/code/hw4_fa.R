#load data package
library(ncvreg)
data(heart)
#use MCMCpack to do the regression
library(MCMCpack)

#Bayesian Probit Regression Model with multivariate normal prior
fit1 <-MCMCprobit(chd~.,data=heart,thin=2, burnin=1000,mcmc =50000, b0=0, B0=.001)
summary(fit1)
plot(fit1)

# Bayesian Logistic Regression Model with multivariate normal prior
fit2 <-MCMClogit(chd~.,data=heart,thin=2, burnin=1000,mcmc =50000, b0=0, B0=.001)
summary(fit2)
plot(fit2)

# Model comparison using Bayes factors
# sepcify the Laplaces method of approximation
fit3 <-MCMCprobit(chd~.,data=heart,thin=2, burnin=1000,mcmc =50000,
                  b0=0, B0=.001, marginal.likelihood="Laplace")
fit4 <-MCMClogit(chd~.,data=heart,thin=2, burnin=1000,mcmc =50000,
                 b0=0, B0=.001, marginal.likelihood="Laplace")
BayesFactor(fit3,fit4)

#Remove one data point from the original dataset and fit the probit and logistic models
heart_new = heart[1,]
x = cbind(1,heart_new[1,1:9])
y = heart_new[10]
heart = heart[-1,]

# refit the models
fit5 <-MCMCprobit(chd~.,data=heart,thin=2, burnin=1000,mcmc =50000, b0=0, B0=.001)
fit6 <-MCMClogit(chd~.,data=heart,thin=2, burnin=1000,mcmc =50000, b0=0, B0=.001)
# predict the value at testpoint for the two models
pred <- fit6 %*% t(as.matrix(x))
p1 <- 1/(1+exp(-pred))
HPDinterval(mcmc(p1))

pred <- fit5 %*% t(as.matrix(x))
p2 <- pnorm(pred)
HPDinterval(mcmc(p2))