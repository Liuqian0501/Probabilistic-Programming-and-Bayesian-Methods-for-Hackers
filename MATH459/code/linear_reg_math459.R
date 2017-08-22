library(LearnBayes);
library(MASS)
fit1 <- lm(Nest ~ Grass + Soil + Angle + Distance, data = puffin)
summary(fit)



source("http://bioconductor.org/biocLite.R") 
biocLite(c("graph", "RBGL", "Rgraphviz")) 
install.packages("gRain", dependencies=TRUE)
library(MCMCpack);
Bfit <- MCMCregress(Nest ~ Grass + Soil + Angle + Distance, data = puffin,
                    burnin = 1000, mcmc = 25000, thin = 25)
summary(Bfit)
intervals = HPDinterval(Bfit, prob = 0.95)
puffin_new = puffin[1,]
x = cbind(1,puffin_new[2:5])
pred <- Bfit[,1:5] %*% t(as.matrix(x))
densityplot(pred)

hist(pred)
plot(Bfit[,1:5])



source("http://bioconductor.org/biocLite.R") 
biocLite(c("graph", "RBGL", "Rgraphviz")) 
install.packages("gRain", dependencies=TRUE)
library(MCMCpack);
Nfit <- MCMCregress(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc. , data = stackloss,
                    burnin = 1000, mcmc = 25000, thin = 25)
intervals = HPDinterval(Nfit, prob = 0.95)
intervals
summary(Nfit)
plot(Nfit)