Gibbs <- function(sims, burnin){
  library(coda);
  # generate some data
  n = 25;
  data_X = rnorm(n, mean = 2, sd = 2);
  #posterior probability
  post_prob = function(x){                    
    prior = x[2]^(-2);
    m=1;
    for(j in data_X){
      m = m * dnorm(j, mean=x[1],sd=x[2])
    }
    prob_pos=m*prior;
    return (prob_pos)
  }
  #initial point
  theta.current <- c(2, 2)
  #define RWMH function
  theta.mh <- matrix(NA, nrow = sims, ncol = 2)
  theta.update <- function(index,theta.current) {
    if (index == 1){
      theta.can <- rnorm(1, theta.current[1], 1);
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

# With respect to gibbs sampling
gibbs.draws <- Gibbs(sims  = 11000, burnin = 1000)

# Plot the trace and the marginal posterior density
plot(gibbs.draws)
hist(gibbs.draws[,1])
hist(gibbs.draws[,2])

gibbs.draws1 <- Gibbs(sims  = 11000, burnin = 1000)
gibbs.draws2 <- Gibbs(sims  = 11000, burnin = 1000)
gibbs.draws3 <- Gibbs(sims  = 11000, burnin = 1000)
gibbs.draws4 <- Gibbs(sims  = 11000, burnin = 1000)
gibbs.draws5 <- Gibbs(sims  = 11000, burnin = 1000)
gibbs.list <- list(gibbs.draws1, gibbs.draws2, gibbs.draws3, gibbs.draws4, gibbs.draws5)

# Plotting how PSRF Changes through Iteration
gelman.diag(gibbs.list)
gelman.plot(gibbs.list)


# RW
TwoDRW <- function(sims, burnin){
  library(coda);
  require(MASS)
  # generate some data
  n = 25;
  data_X = rnorm(n, mean = 2, sd = 2);
  #posterior probability
  post_prob = function(x){                    
    prior = x[2]^(-2);
    m=1;
    for(j in data_X){
      m = m * dnorm(j, mean=x[1],sd=x[2])
    }
    prob_pos=m*prior;
    return (prob_pos)
  }
  
  #initial point
  start =c(2,2);
  
  # use 2-D normal as candidate distribution and sepcify variance-convariance matrix
  mle_fit <- fitdistr(data_X, "normal")
  cand.sd <- unname(diag(mle_fit$sd))
  
  # define the random walk Metropolis-Hastings sampling function
  theta.cur <- start
  draws <- matrix( NA, nrow = sims, ncol = 2)
  for(i in 1:sims){
    theta.can <- mvrnorm(1, theta.cur, cand.sd)
    if (theta.can[1]>0 && theta.can[2]>0)
      if(runif(1) <= min(1, post_prob(theta.can)
                         /post_prob(theta.cur)))
        theta.cur <- theta.can
      draws[i, ] <- theta.cur
  }
  return(mcmc(unname(draws[(burnin + 1):sims, ])))
}

# With respect to Two D random walk Metropolis Hastings
rwmh.draws <- TwoDRW(sims  = 13000, burnin = 3000)
cat("Acceptance Rate:", 1-rejectionRate(rwmh.draws), "\n")

# Plot the trace and the marginal posterior density
plot(rwmh.draws)
hist(rwmh.draws[,1])
hist(rwmh.draws[,2])

rwmh.draws1 <- TwoDRW(sims  = 13000, burnin = 3000)
rwmh.draws2 <- TwoDRW(sims  = 13000, burnin = 3000)
rwmh.draws3 <- TwoDRW(sims  = 13000, burnin = 3000)
rwmh.draws4 <- TwoDRW(sims  = 13000, burnin = 3000)
rwmh.draws5 <- TwoDRW(sims  = 13000, burnin = 3000)
rwmh.list <- list(rwmh.draws1, rwmh.draws2, rwmh.draws3, rwmh.draws4, rwmh.draws5)

# Plotting how PSRF Changes through Iteration
gelman.diag(rwmh.list)
gelman.plot(rwmh.list)
