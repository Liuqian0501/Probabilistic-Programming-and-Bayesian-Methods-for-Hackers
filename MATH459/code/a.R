prosample = rpois(40, 6)

mle = function(y)
{
  a=0;
  for(i in prosample){
    a = a + log(dpois(x = i, lambda = y ),0.5)
  }
  return (a)
}

optim(par=6, fn = mle)$par



map = function(y)
{
  a=log(dgamma(x=y, shape = 2, scale = 1/3),0.5);
  for(i in prosample){
    a = a + log(dpois(x = i, lambda = y ),0.5)
  }
  return (a)
}

optim(par=6, fn = map)$par
