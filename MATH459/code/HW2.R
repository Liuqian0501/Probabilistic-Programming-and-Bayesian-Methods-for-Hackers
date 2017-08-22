sample = rpareto(n=100, scale=1, shape=1);
logsum = sum(log(sample));
a = qgamma(0.025, shape = 102,  scale = 1/(0.5+ logsum));
b = qgamma(0.975, shape = 102,  scale = 1/(0.5+ logsum));
c = c(a,b);c

err1 = function(x)
{
  a = qgamma(x[3], shape = x[1]+100,  scale = 1/(x[2]+ logsum));
  b = qgamma(0.95+x[3], shape = x[1]+100,  scale = 1/(x[2]+ logsum));
  m = abs((b-a)-1);
  return (m);
}
optim(par= c(2,1,0.01), fn = err1)
