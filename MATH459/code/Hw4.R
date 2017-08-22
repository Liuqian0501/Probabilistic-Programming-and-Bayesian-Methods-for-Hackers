library(ncvreg)

library(rstanarm)
library(gridExtra)
data(heart)


# Estimate original model
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
heart$sbp = heart$sbp/130;
heart$ldl =heart$ldl/4;
heart$adiposity = heart$adiposity/28;
heart$obesity = heart$obesity/26;

#guess distribution
library(ggplot2)
ggplot(wells, aes(x = adiposity, y = ..density..)) +
  geom_histogram(data = subset(heart, chd == 0)) +
  geom_histogram(data = subset(heart, chd == 1),
                 fill = "skyblue", alpha = 0.3)

fit1 <- stan_glm(chd~sbp + tobacco+ ldl+adiposity+obesity , 
                 data = heart, family = binomial(link = "logit"),
                 prior= normal(1,0.05), prior_intercept = normal(1,1));

(coef_fit <- round(coef(fit1), 3))

round(posterior_interval(fit1, prob = 0.95), 2)

yrep <- posterior_predict(fit1)

prop_zero <- function(y) mean(y == 0)

(prop_zero_test1 <- pp_check(fit1, check = "test",
                             test = "prop_zero"))

library(brms)
bin.brm.mod <- brm(chd ~ sbp + tobacco+ ldl + adiposity + obesity + typea + age,
                   data=heart, family=binomial(link = "logit"))
  
summary(bin.brm.mod)
summary(marginal_effects(bin.brm.mod))
plot(bin.brm.mod)
plot(marginal_effects(bin.brm.mod))
                                        
