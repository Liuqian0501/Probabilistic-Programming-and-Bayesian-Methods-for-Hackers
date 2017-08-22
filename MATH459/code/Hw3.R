library(coda);

data_X = rgamma(200, shape = 1.7, rate = 4.4);
data_alpha = seq(0, 30, by = 0.1);
data_beta = seq(0, 30, by = 0.1);
prob_pos  = 1:200;

post_prob = function(x,y){
      prior = sqrt(x*trigamma(x)-1)/y;
      m=1;
      for(j in data_X){
        m = m * dgamma(j, shape=x,rate=y)
      }
      prob_pos=m*prior;
  return (prob_pos)
}
mh.gamma = function(n.sims, start, burnin, cand.sd, shape, rate){
  theta.cur <- start;
  draws <- c();
  theta.update <- function(theta.cur, shape, rate){
    theta.can <- rnorm(1, mean=theta.cur, sd=cand.sd)
    accept.prob <- dgamma(theta.can, shape=shape,rate=rate)/
      dgamma(theta.cur,shape=shape,rate=rate)
    if(runif(1) <= accept.prob)
      theta.can
    else
      theta.cur
  }
  for(i in 1:n.sims){
    draws[i] <- theta.cur <- theta.update(theta.cur, shape = shape,rate = rate)
  }
  res <- mcmc(draws[(burnin + 1):n.sims])
  cat("Acceptance Rate:", 1-rejectionRate(res), "\n")
  return(res)
}

mh.draws <- mh.gamma(10000, start = 1, burnin = 1000, cand.sd = 1, shape = 1.7, rate = 4.4)

gamma.samp <- rgamma(10000, shape = 1.7, rate = 4.4)
gamma.draws <- as.mcmc(gamma.samp) 
plot(gamma.draws)# calls both traceplot(), densplot()
plot(mh.draws) # calls both traceplot(), densplot()




Xdata <- rgamma(1000, shape = 1.7, scale = 1/4.4)
mh.draws <- MCMCmetrop1R(gammafun, theta.init=c(1.7, 4.4), x=Xdata, N=30, burnin = 1000, mcmc =10000, verbose = 1,logfun = FALSE, V=matrix(c(0.4,0,0,0.8),nrow=2, ncol=2))