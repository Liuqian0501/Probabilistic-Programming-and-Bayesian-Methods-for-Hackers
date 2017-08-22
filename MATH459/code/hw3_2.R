TwoDMH <- function(sims, burnin){
  library(coda);
  # generate some data
  data_X = rgamma(30, shape = 1.7, rate = 4.4);

  #posterior probability
  post_prob = function(x){                    
    prior = sqrt(x[1]*trigamma(x[1])-1)/x[2];
    m=1;
    for(j in data_X){
      m = m * dgamma(j, shape=x[1],rate=x[2])
    }
    prob_pos=m*prior;
    return (prob_pos)
  }
  
  
  #initial point
  theta.current <- c(1, 1)
  
  #define RWMH function
  theta.mh <- matrix(NA, nrow = sims, ncol = 2)
  theta.update <- function(index,theta.current) {
 
    if (index == 1){
      theta.can <- rgamma(1, theta.current[1], 1);
      theta.can =c(theta.can,theta.current[2]);
    }
    else{
      theta.can <- rgamma(1, theta.current[2], 1);
      theta.can =c(theta.current[1],theta.can);
    }
    
    accept.prob <- min(1,post_prob(theta.can)/post_prob(theta.current))
    if(runif(1) <= accept.prob)
        theta.can
    else
      theta.current
  }
  
  
  for(i in 1:sims){
       theta.current <- theta.update(1, theta.current);
       theta.mh[i,1] = theta.current[1];
       theta.current<- theta.update(2, theta.current);
       theta.mh[i,2] = theta.current[2]
  }
  
  res <- mcmc(theta.mh[(1+burnin):sims,])
  cat("Acceptance Rate:", 1-rejectionRate(res), "\n")

  return(res)
  
}


mh.draws <- TwoDMH(sims  = 10000, burnin = 1000)
# Summarizing the Posterior Density
summary(mh.draws)
autocorr.plot(mh.draws)
plot(mh.draws)

mh.draws1 <- TwoDMH(sims  = 10000, burnin = 1000)
mh.draws2 <- TwoDMH(sims  = 10000, burnin = 1000)
mh.draws3 <- TwoDMH(sims  = 10000, burnin = 1000)
mh.draws4 <- TwoDMH(sims  = 10000, burnin = 1000)
mh.draws5 <- TwoDMH(sims  = 10000, burnin = 1000)
rwmh.list <- list(mh.draws1, mh.draws2, mh.draws3, mh.draws4, mh.draws5)

# Plotting how PSRF Changes through Iteration
gelman.diag(rwmh.list)
gelman.plot(rwmh.list)

geweke.diag(mh.draws)
heidel.diag(mh.draws)

# calculate 95% HPD intervals
intervals = HPDinterval(mh.draws, prob = 0.95)

# plot the MCMC estimate of the marginal posterior density for each parameter
densplot(mh.draws[,1])
densplot(mh.draws[,2])


