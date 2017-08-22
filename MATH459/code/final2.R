# initiate date
data("Boston")


#use MCMCpack to do the regression
library(MCMCpack)

#Bayesian regression Regression Model with multivariate normal prior, with variable crim+indus+nox+rm
fit1 <-MCMCregress(medv~crim+indus+nox+rm,data=Boston,thin=2, burnin=1000,
                   mcmc =50000, b0=0, B0=.001,marginal.likelihood="Laplace")
# Bayesian regression Regression Model with multivariate normal prior, with variable crim+indus+nox+rm+age
fit2 <-MCMCregress(medv~crim+indus+nox+rm+age,data=Boston,thin=2, burnin=1000,
                   mcmc =50000, b0=0, B0=.001,marginal.likelihood="Laplace")
summary(fit1)
# compare two model using bayesFactor
BayesFactor(fit1,fit2)

#plot the posterior distribution wrt pred value
x = cbind(1,Boston[,c(1,3,5,6)])
pred <- fit1[,1:5] %*% t(as.matrix(x))
basicPlot <- function(...){
  plot(pred[1:10000] ~ fit1[1:10000,2], bty="n", lwd=2,
       main="MCMCpack marginal effect", col="#00526D",
       xlab="crim's weight",
       ylab="pred", ...)
  axis(side = 1, col="grey")
  axis(side = 2, col="grey")
}
basicPlot()

library(ggplot2)
ggplot(wells, aes(x = adiposity, y = ..density..)) +
  geom_histogram(data = subset(heart, chd == 0)) +
  geom_histogram(data = subset(heart, chd == 1),
                 fill = "skyblue", alpha = 0.3)

#plot the marginal effect of baysian linear model
library(brms)
lin.brm.mod <- brm(medv~crim+indus+nox+rm+age,data=Boston,
                   family="gaussian")
log.brm.mod <- brm(log(medv)~crim+indus+nox+rm+age,data=Boston,
                   family="gaussian")
lin.samples <- posterior_samples(lin.brm.mod)

plot(marginal_effects(lin.brm.mod))
hist(predict(lin.brm.mod))
plot(marginal_effects(log.brm.mod))
hist(exp(predict(log.brm.mod)))

#plot the posterior distribution wrt pred value
x = cbind(1,Boston[,c(1,3,5,6,7)])
pred <- as.matrix(lin.samples[,1:6]) %*% t(as.matrix(x))
basicPlotBrm <- function(...){
  plot(pred[1:10000] ~ lin.samples[1:10000,5], bty="n", lwd=2,
       main="brm marginal effect", col="#00526D",
       xlab="rm's weight",
       ylab="pred", ...)
  axis(side = 1, col="grey")
  axis(side = 2, col="grey")
}
basicPlotBrm()
#guess distribution
df <- data.frame(pred = pred[1:50000],
                 rm = lin.samples[1:50000,5])
library(ggplot2)
ggplot(df, aes(x = rm, y = ..density..)) +
  geom_histogram(data = subset(df, pred >26)) +
  geom_histogram(data = subset(df, pred<26),
                 fill = "skyblue", alpha = 0.3)

