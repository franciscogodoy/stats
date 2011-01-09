# In R, we use glm function to fit GLM models. 
# 	glm(formula, family=familytype(link=linkfunction), data=)
#	See help(glm) for more options.

# The list below list common link functions 
#	glm(formula, family=binomial(link="logit"), data=)
#	glm(formula, family=gaussian(link="identity"), data=)
#	glm(formula, family=Gamma(link="inverse"), data=)
#	glm(formula, family=inverse.gaussian(link="1/mu^2"), data=)
#	glm(formula, family=poisson(link="log"), data=)
#	glm(formula, family=quasi(link = "identity", variance = "constant"), data=)
#	glm(formula, family=quasibinomial(link = "logit"), data=)
#	glm(formula, family=quasipoisson(link = "log"), data=)
#	See help(family) for other link functions. 

# Example: Logistic regression.
# 		y is a binary factor and 
# 		x1-x3 are continuous predictors 
#

	mydata = read.table('salmon.data');
	names(mydata) = c('y','x1', 'x2', 'x3');
	mydata$y = mydata$y-1;
	fit <- glm(y~x1+x2+x3,data=mydata,family=binomial())

# display fitting coefficients (est thetas) and oher results
	summary(fit);

# 95% CI for the coefficients
	confint(fit);
 
# exponentiated coefficients
	exp(coef(fit));

# 95% CI for exponentiated coefficients
	exp(confint(fit));

# predicted values
	y.hat = predict(fit, type="response");
	y.hat = y.hat > 0.5;
	y.train = mydata$y;
	y.train = y.train  > 0.5;

# confusion matrix to see the training error
	table(y.hat, y.train);

	# residuals
	residuals(fit, type="deviance");

# 	Comparing to discriminant function analysis, logistic regression is a more natural 
#	choice when we predict a binary response from a set of continuous predictors. 
#	because of its less restrictive assumptions.

	fit2 <- glm(y~x2+x3,data=mydata,family=binomial());

	# to compare nested models and suggest drop x1 from the model
	anova(fit,fit2, test="Chisq");
	

	fit3 <- glm(y~x3,data=mydata,family=binomial());
	anova(fit,fit3, test="Chisq");


 
# Ref: 
# http://www.statmethods.net/advstats/glm.html
