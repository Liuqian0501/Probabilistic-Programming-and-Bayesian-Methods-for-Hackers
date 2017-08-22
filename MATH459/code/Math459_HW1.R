## Math 459 Homework 1

## Question 1
n <- 40
a <- 2
b <- 3
data <- rpois(n, lambda=6)
# note we ignore terms which do not depend on lambda in the likelihood
# work with log for numerical stability
loglike <- function(lamb,y){
	logl <- sum(y)*log(lamb)-n*lamb
	return(-logl)
}
# optim minimizes, so we use the negative log likelihood
likeopt <- optim(5,loglike, y=data, method="Brent", lower=0.1,upper=15)
mlelamb <- likeopt$par		# the MLE
mlelamb
mlelike <- likeopt$value	# the value of the negative log likelihood function at the MLE
# also use the negative log prior
prior <- function(lamb,a,b){
		p <- dgamma(lamb,shape=a,rate=b)
		return(p)
}

# negative log posterior
logpost <- function(lamb,y){
	lpost <- -log(prior(lamb,a,b))+loglike(lamb,y)
	return(lpost)
}

postopt <- optim(5,logpost,y=data, method="Brent",lower=0.1,upper=15)
pmodelamb <- postopt$par 	# the posterior mode
pmodelamb
pmodepost <- postopt$value  # the value of the negative posterior at the posterior mode
pmodepost

