
install.packages('car');
install.packages('MPV');

library(car);
library(MPV);


# Ex 7.1
x = c(1,1.7,1.25,1.2,1.45,1.85,1.6,1.5,1.95,2)
x2 = x^2;
x2
cor(x,x2);

summary(lm(x2 ~ x));
VIF = 1/(1-0.9908);
print(VIF);


y = 5 + 2*x + 3*x2
y
gaussian_noise = rnorm(10, mean = 0, sd = 0.2)
y = y + gaussian_noise
y
my.quadratic.model = lm(y ~ x + x2);
my.quadratic.model
vif(my.quadratic.model)

# Ex 7.2
X1 = c(0.25,0.5,0.75,1.0,1.25,1.50,1.75,2.0,2.25,2.50);
X2 = X1^2;
y = c(1.42, 1.39, 1.55, 1.89, 2.43, 3.15, 4.05, 5.15, 6.43, 7.89);
rocket.data = data.frame(X1,X2,y);

weightloss.quad.lm = lm(y ~ X1 + X2, data= rocket.data);
summary(weightloss.quad.lm);

weightloss.linear.lm = lm(y ~ X1 , data= rocket.data);
summary(weightloss.linear.lm);

anova(weightloss.linear.lm, weightloss.quad.lm);

# F test to compare Full model and Reduce model 
f.test.lm = function(R.lm, F.lm) {
   SSE.Reduce.Model = sum(resid(R.lm)^2);
   SSE.Full.Model = sum(resid(F.lm)^2);   
   Extra.SumSquare = SSE.Reduce.Model - SSE.Full.Model;
   df.num = R.lm$df - F.lm$df
   df.den = F.lm$df;
   F = ( Extra.SumSquare / df.num) / (SSE.Full.Model / df.den);
   p.value = 1 - pf(F, df.num, df.den);
   
   SSE.data =  data.frame(SSE.Full.Model, SSE.Reduce.Model, Extra.SumSquare);
   F.data = data.frame(F, df.num, df.den, p.value);   
   test_result = list(Method="extra-sum-of-squares",SS.Residuals=SSE.data,F.statistic=F.data);   
   return(test_result);
}

f.test.lm(weightloss.linear.lm , weightloss.quad.lm);

plot(rocket.data$X1, rocket.data$y, pch=23,bg='orange', cex=2, main ='Overlay the fitted model on the scatter plot');
lines(sort(rocket.data$X1), weightloss.quad.lm$fit[order(rocket.data$X1)], col = 2, lwd = 3)

# Ex 7.3
residuals = resid(weightloss.quad.lm)
standardized.residuals = rstandard(weightloss.quad.lm)
studentized.residuals = rstudent(weightloss.quad.lm);
residuals = data.frame(residuals, standardized.residuals, studentized.residuals);

par(mfrow=c(1,2));
plot(weightloss.quad.lm)

# Ex 7.14
X1 = c(0.25,0.5,0.75,1.0,1.25,1.50,1.75,2.0,2.25,2.50);
X2 = X1^2;
y = c(1.42, 1.39, 1.55, 1.89, 2.43, 3.15, 4.05, 5.15, 6.43, 7.89);
rocket.data = data.frame(X1,X2,y);
print(rocket.data);

second.order.model = lm(y ~ X1 + X2);
summary(second.order.model);
vif(second.order.model);

rocket.data$centered_X = rocket.data$X1 - mean(rocket.data$X1);
rocket.data$centered_X_square = rocket.data$centered_X^2;
center.x.quadratic.model = lm(y ~ centered_X + centered_X_square, data=rocket.data);
summary(center.x.quadratic.model)
vif(center.x.quadratic.model);

# Ex 7.15
ex715 <- read.csv("C:/Users/th/git/mva/regression/ex715.csv");
first.order.model = lm(y ~ x, data=ex715);
summary(first.order.model);

par(mfrow=c(1,1));
plot(ex715$x, ex715$y, pch=23,bg='orange', cex=2, main ='Overlay the fitted model on the scatter plot');
lines(sort(ex715$x), first.order.model$fit[order(ex715$x)], col = 2, lwd = 3);

plot(predict(first.order.model), ex715$y, pch=23,bg='orange', cex=2, main ='predicted vs observed y');
plot(first.order.model)

ex715$x2 = ex715$x^2;
second.order.model = lm(y ~ x + x2, data=ex715);
summary(second.order.model);

anova(first.order.model, second.order.model);

par(mfrow=c(1,2));
plot(ex715$x, ex715$y, pch=23,bg='orange', cex=2, main ='Overlay w/ fitted second-order model');
lines(sort(ex715$x), second.order.model$fit[order(ex715$x)], col = 2, lwd = 3);

plot(predict(second.order.model), ex715$y, pch=23,bg='orange', cex=2, main ='predicted vs observed y');
plot(second.order.model, pch=23,bg='orange', cex=2);

# Ex 8.3
ex83 <- read.csv("C:/Users/th/git/mva/regression/ex83.csv");
ex83$city <- factor(ex83$city);
ex83

delivery.site.model = lm(y ~ x1 + x2 + city, data=ex83);
summary(delivery.site.model)

model.matrix(delivery.site.model);

#anova(without.delivery.site.model, delivery.site.model);

without.delivery.site.model = lm(y ~ x1 + x2, data=ex83);
f.test.lm(without.delivery.site.model, delivery.site.model);

par(mfrow=c(1,2));
plot(delivery.site.model, pch=23,bg='orange', cex=2);

attach(ex83);
colors <- c('red', 'green', 'blue', 'yellow')
symbols <- c(23,24)

par(mfrow=c(1,1));
r = resid(delivery.site.model)
k = 1
plot(city, r, type='n', xlab='City', ylab='Residuals', main='Residuals by delivery site');
for (i in 1:4) {
     subset <- as.logical((city == i))
     points(rep(k, length(r[subset])), r[subset], pch=symbols[1], bg=colors[i], cex=2);
     k = k+1;
}
 
# Ex 8.4
B3.table <- read.csv("C:/Users/th/git/mva/regression/B3.table.csv");
B3.table$x11 <- factor(B3.table$x11);
transmission.type.model = lm(y ~ x1 + x11, data=B3.table);
summary(transmission.type.model);
model.matrix(transmission.type.model);

without.transmission.type.model = lm(y ~ x1 , data=B3.table);
anova(without.transmission.type.model, transmission.type.model);

interaction.model = lm(y ~ x1 * x11, data=B3.table);
no.interaction.model = lm(y ~ x1 + x11, data=B3.table);
summary(interaction.model);

anova(no.interaction.model, interaction.model);

# Ex 8.5
x10.and.transmission.type.model = lm(y ~ x10 + x11, data=B3.table);
summary(x10.and.transmission.type.model);

interaction.model = lm(y ~ x10 * x11, data=B3.table);
no.interaction.model = lm(y ~ x10 + x11, data=B3.table);
summary(interaction.model);

anova(no.interaction.model, interaction.model);

# Ex 8.11
ex811$percent <- factor(ex811$percent);
anova.model = lm(y ~ percent, data=ex811);
X = model.matrix(anova.model);

x1 = c(rep(1,5),rep(0,20));
x2 = X[,2];
x3 = X[,3];
x4 = X[,4];

ex811.table = data.frame(y,x1,x2,x3,x4);
anova.model = lm(y ~ x1+x2+x3+x4, data=ex811.table);
summary(anova.model);
model.frame(anova.model);

# y vector
y = ex811.table$y;
print(y);

X = model.matrix(anova.model);
print(X);

# Ex 10.9
# find a appropriate model thr. all-possible-regression approach 
B3.table <- read.csv("C:/Users/th/git/mva/regression/B3.table.csv");
B3.table$x11 <- factor(B3.table$x11);

all.rows = 1:32;
test.rows = sample(all.rows, 8, replace=F);
print(all.rows);
print(test.rows);

train.rows = all.rows[!(all.rows %in% test.rows)];
print(train.rows);

test.set = B3.table[test.rows,];
print(test.set);

train.set = B3.table[train.rows,];
print(train.set);

library(leaps);

# remove missing values from  (x3=NA);
x3.subset = !is.na(B3.table$x3);
full.model = lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, data=B3.table, subset=x3.subset);
X = model.matrix(full.model)[,-1];
y = B3.table$y[x3.subset];

# leaps try to find best model out of all possible models for a given criterion (adjr2 or Cp)
adjr2_models = leaps(X, y, nbest=3, method='adjr2');
#plot(adjr2_models$size, adjr2_models$adjr2, pch=23, bg='orange', cex=2);
best.model.adjr2 = adjr2_models$which[which((adjr2_models$adjr2 == max(adjr2_models$adjr2))),];
print(best.model.adjr2);

Cp_models = leaps(X, y, nbest=3, method='Cp');
#plot(Cp_models$size, Cp_models$Cp, pch=23, bg='orange', cex=2);
best.model.Cp = Cp_models$which[which((Cp_models$Cp == min(Cp_models$Cp))),]
print(best.model.Cp);

# Both adjr2 and Cp creteria give the best subset as y ~ x5 + x8 + x10

# stepwise model building procedure that tries to include or delete a variable at a time
stepwise.both = step(full.model, direction='both');

# stepwise also suggest model y ~ x5 + x8 + x10 (AIC=68.29)

# this model is suggested by both stepwise and "all-possible-regression" method
choosen.model = lm(y ~ x5 + x8 + x10, data=train.set);
summary(choosen.model);

#compute the PRESS statistic
PRESS(choosen.model);

#compute predicted y for the test.set
y.predicted = predict(choosen.model, test.set);
y.observed = test.set$y;
predicted.error = y.observed - y.predicted
predicted.set = data.frame(y.observed, y.predicted, predicted.error);

# print root mean square error (RMSE)
RMSE = sqrt(sum(predicted.error^2)/nrow(test.set));

predicted.performance = list(prediction.data=predicted.set, RMSE=RMSE);
print(predicted.performance);

# Extra work for fun
# fit a reduced model - developed in problem 3.6
# use only data in the training set (24 observations)
problem3.6.model = lm(y ~ x8 + x10, data=train.set);
summary(problem3.6.model);

#compute the PRESS statistic
PRESS(problem3.6.model);

#compute predicted y for the test.set
y.predicted = predict(problem3.6.model, test.set);
y.observed = test.set$y;
predicted.error = y.observed - y.predicted
predicted.set = data.frame(y.observed, y.predicted, predicted.error);

# print root mean square error (RMSE)
RMSE = sqrt(sum(predicted.error^2)/nrow(test.set));

predicted.performance = list(prediction.data=predicted.set, RMSE=RMSE);
print(predicted.performance);

#Ex 11.19
B15.table <- read.csv("C:/Users/th/git/mva/regression/B15.csv");
ols.model = lm(MORT ~ PRECIP +  EDUC	+ NONWHITE +	NOX	+ SO2, data=B15.table);
summary(ols.model);
vif(ols.model);

X = model.matrix(ols.model);
Y = B15.table$MORT;

eigen(t(X)%*%X);
svd(X);

#
library(MASS);
ridge.model = lm.ridge(MORT ~ PRECIP +  EDUC  + NONWHITE +	NOX	+ SO2, data=B15.table, lambda=seq(0,10,0.02));
par(mfrow=c(1,1));
plot(ridge.model);
plot(ridge.model$lambda, ridge.model$GCV, xlab='Lambda', ylab='GCV',  type='l', lwd=3, col='orange');
select(ridge.model);
summary(ridge.model);

library(pls);
pcr.model = pcr(MORT ~ PRECIP +  EDUC  + NONWHITE +  NOX	+ SO2, ncomp = 5, scale = TRUE,  data=B15.table);
summary(pcr.model);
print(pcr.model$coefficients);
print(pcr.model$loadings);
summary(svdpc.fit(X, Y, ncomp=3));

#Ex 14.3
see matlab code

#Ex 14.6
ex146 <- read.csv("C:/Users/th/git/mva/GLM/ex146.csv");
#summary(glm(y ~ x1+x2+x3, family=poisson(), data=Aircraft));
#summary(glm(y ~ x2, family=poisson(), data=Aircraft));

poisson.model = glm(num_fail ~ months, family=poisson(), data=ex146);
summary(poisson.model);

deviance = poisson.model$deviance;
p.value = 1-pchisq(deviance, poisson.model$df.residual);

Goodness.Of.Fit = data.frame(Method='Deviance', ChiSquare=deviance, DF=poisson.model$df.residual, P=p.value);
Goodness.Of.Fit.Test = list(Test='Goodness Of Fit', Result=Goodness.Of.Fit);
print(Goodness.Of.Fit.Test);

plot(ex146$months, ex146$num_fail, pch=23,bg='orange', cex=2, main ='Fitted model vs months');
lines(sort(ex146$months), poisson.model$fit[order(ex146$months)], col = 2, lwd = 3);

ex146$x2 = ex146$months^2;
expand.poisson.model = glm(num_fail ~ months + x2, family=poisson(), data=ex146);
summary(expand.poisson.model);

# partial deviance test of full vs. reduced
anova(poisson.model, expand.poisson.model);

deviance = anova(poisson.model, expand.poisson.model)$Deviance[2];
print(deviance);

df = anova(poisson.model, expand.poisson.model)$Df[2];
p.value = 1-pchisq(deviance, df);
print(p.value);

# Compute Wald statistic for beta0
beta0 = coef(poisson.model)[1];
SE = sqrt(vcov(poisson.model)[1,1]);
Z = beta1/SE;
#print Wald statistic for beta0
print(Z);


# Compute Wald statistic for beta1
beta1 = coef(poisson.model)['months'];
SE = sqrt(vcov(poisson.model)['months', 'months'])
Z = beta1/SE;

#print Wald statistic for beta 1
print(Z);

# Compute CI for beta1
beta1.hat = coef(poisson.model)['months'];
SE = sqrt(vcov(poisson.model)['months', 'months']);
ci.upr = beta1.hat + SE * qnorm(0.975);
ci.lwr = beta1.hat + SE * qnorm(0.025);
data.frame(center=beta1.hat, lower=ci.lwr, upper=ci.upr);


