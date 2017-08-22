prosample = sort(Michelson$velocity)
model_var = 50;
n = length(prosample);

mle = function(y)
{
  a = 0;
  for(i in prosample){
    a = a + log(dnorm(x = (i-y)/sqrt(model_var)),0.5)
  }
  return (a)
}

mle_mean = optim(par=800, fn = mle)$par ; mle_mean

sample_mean = mean(prosample);
a = sample_mean - qnorm(0.975)*sqrt(model_var/n);
b = sample_mean + qnorm(0.975)*sqrt(model_var/n);
c = c(a,b); c

pri_var = 50;
pri_mean = 800;
post_var = 1/((1/pri_var)+(n/ model_var));
post_mean = ((pri_mean/pri_var)+sum(prosample/model_var))*post_var;

a = qnorm(0.975,mean = post_mean, sd = sqrt(post_var))
b = qnorm(0.025,mean = post_mean, sd = sqrt(post_var))
c = c(b,a); c

sequ = seq(848,857,0.02);
d = dnorm(sequ,mean = post_mean, sd = sqrt(post_var));
plot(x= sequ , y = d, xlab = "velocity", ylab = "Probability")

a = qnorm(0.975,mean = post_mean, sd = sqrt(post_var))
b = qnorm(0.025,mean = post_mean, sd = sqrt(post_var))
c = c(b,a); c

Min_inter = function(x)
{
    a = qnorm(x, mean = post_mean, sd = sqrt(post_var))
    b = qnorm(0.95+x, mean = post_mean, sd = sqrt(post_var))
    m = abs(b-a);
    return (m);
}

a = optim(par= 0.01, fn = Min_inter)$par;
b = 0.95+a;
a = qnorm(a,mean = post_mean, sd = sqrt(post_var))
b = qnorm(b,mean = post_mean, sd = sqrt(post_var))
c = c(a,b); c