#one way anova
drunk = c(7,4,9,3,2,4,2,3,2);
rank =c(rep("D",3), rep("V",3), rep("P",3));
statedrunk = data.frame(drunk,rank);
anova = aov(drunk~rank, data = statedrunk);
model = lm(drunk~rank);
summary(model);
summary(anova)
TukeyHSD(anova)

##one way anova
ex4 = c(6,2,4,3,10,9,11,12,9,12,13,9);
group =c(rep("Group1",4), rep("Group2",4), rep("Group3",4));
state_group = data.frame(ex4,group);
boxplot(ex4~group) 
anova_4 = aov(ex4~group, data = state_group);
summary(anova_4)
TukeyHSD(anova_4)

##one way anova
drunk = c(49.8,48.5,48.7,47.2,49.3,51.5,50.9,50.1,51.2,52.8,52.3,53.2);
rank =c(rep("D",4), rep("V",4), rep("P",4));
statedrunk = data.frame(drunk,rank);
anova = aov(drunk~rank, data = statedrunk);
summary(anova)
TukeyHSD(anova)

## two way anova
drunk = c(3.1, 4.2, 2.7, 4.9, 2.9, 4.9, 3.2, 4.5, 2.7, 2.9, 1.8, 3.0);
rank =c(rep("R1",4), rep("R2",4), rep("R3",4));
rank1 =c(rep(c("C1","C2","C3","C4"),3));
statedrunk = data.frame(drunk,rank,rank1);
result = aov(drunk~rank+rank1+rank*rank1, data = statedrunk);
summary(result);

## two way anova
ex3 = c(52, 47, 44, 51, 42, 60, 55, 49, 52, 43, 56, 48, 45, 44, 38);
A =c(rep("R1",5), rep("R2",5), rep("R3",5));
B =c(rep(c("C1","C2","C3","C4", "C5"),3));
boxplot(ex3~A+B) 
state_ex3 = data.frame(ex3,A,B);
result_3 = lm(ex3~A+B, data = state_ex3);
anova(result_3)
summary(result_3)

##two way anova 
drunk = c(3.1, 4.2, 2.7, 4.9, 2.9, 4.9, 3.2, 4.5, 2.7, 2.9, 1.8, 3.0,
          2.9, 2.3, 2.4, 3.7, 4.0, 4.6, 3.0, 3.9, 4.4, 5.0, 2.5, 4.2);
rank =c(rep("R1",8), rep("R2",8), rep("R3",8));
rank1 =c(rep(c("C1","C2","C3","C4"),6));
boxplot(drunk~rank+rank1) 
statedrunk = data.frame(drunk,rank,rank1);
result = aov(drunk~rank+rank1, data = statedrunk);
summary(result);
#TukeyHSD confidence interval
TukeyHSD(result);

#Test for interaction terms
##compare ( interaction term )T = R+C+I+W without.. T = R +C +W
result.add = lm(drunk~rank+rank1, data = statedrunk);
result.full = lm(drunk~rank*rank1, data = statedrunk);
summary(result.full);
anova(result.full);
anova(result.add,result.full)

## two way anova test for interaction terms,test for row and column effects.
IQ = c(105,102, 93, 86, 97, 106, 110, 114,103, 96, 89, 110, 108, 112);
drug = c(rep("placebo",2),rep("muricatoxicicaljuice",3),rep(" australawesomesauce",3),
         rep("placebo",1),rep("muricatoxicicaljuice",2),rep(" australawesomesauce",3));
gender = c(rep("male",8),rep("femal",6));
model.drug = lm(IQ~drug);
model.gender = lm(IQ~gender);
model.add = lm(IQ~drug+gender);
model.full = lm(IQ~drug*gender);
summary(model.add);
summary(model.full);
anova(model.full);
anova(model.add);
anova(model.full,model.add);# Test for interaction terms.
anova(model.add,model.drug);#Test for row effects.
anova(model.add,model.gender);#Test for column effects.



##linear factor fit
group = c(1,2,3,4,1,2,3,4);
rank =c(rep(c("GA","GB"), 4));
rank1 =c(rep(c("G1","G1","G2","G2"),2));
stategroup = data.frame(group,rank,rank1);
model.full = lm(group~rank*rank1)
model.add = lm(group~rank+rank1)#without inter
summary(model.add)
anova(model.full,model.add)


##linear regression
x = c(54,47, 69, 87, 65, 73, 83, 81, 72, 74);
y = c(61, 22, 55, 78, 45, 75, 56, 66, 59, 70);
linear_r1 = lm(y~x)
linear_r2 = lm(x~y)
plot(x,y)
summary(linear_r1)
anova(linear_r1)
cor(y,x)^2*8/(1-cor(y,x)*cor(y,x))#calculate F value using corelation
summary(linear_r2)

##compare linear regression with one way anova
x = c (3, 3, 3, 5, 5, 5, 8, 8, 8, 10, 10, 10);
y = c(4, 6, 2, 9, 12, 11, 13, 14, 14, 18, 16,15);
model.linear = lm(y~x);
model.anova = lm(y~factor(x));
summary(model.linear);
summary(model.anova);
anova(model.linear,model.anova)

##compare linear regression with quadratic regression
x = c(2,6,7,7,14,15,17,17,18,18);
y = c(52, 127, -61, -100, 24, 63, 110, 464, 196, 82);
model.linear = lm(y~x);
model.quadratic = lm(y~x+I(x^2));
summary(model.linear);
summary(model.quadratic)
anova(model.linear,model.quadratic)


