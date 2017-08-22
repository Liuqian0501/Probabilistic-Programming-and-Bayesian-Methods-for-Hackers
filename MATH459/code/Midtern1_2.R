prosample = rexp(50, 3)
alpha = 3;
beta = 1
mle = function(y)
{
  a=0;
  for(i in prosample){
    a = a + log(dexp(x = i, rate = y ),0.5)
  }
  return (a)
}

mle_mean = optim(par=6, fn = mle)$par; mle_mean
post_mean_a = (alpha+length(prosample))/(beta+sum(prosample));post_mean_a
post_mean_b = (1+length(prosample))/sum(prosample);post_mean_b

a_1 = qgamma(0.975, shape = alpha+length(prosample),scale = 1/(beta+sum(prosample)))
a_2 = qgamma(0.025, shape = alpha+length(prosample),scale = 1/(beta+sum(prosample)))
c1 = c(a_2,a_1);c1


b_1 = qgamma(0.975, shape = 1+length(prosample),scale = 1/sum(prosample));
b_2 = qgamma(0.025, shape = 1+length(prosample),scale = 1/sum(prosample));
c2 = c(b_2,b_1);c2
d1 = a_1-a_2;d1
d2 = b_1-b_2;d2
d = d1 - d2;d