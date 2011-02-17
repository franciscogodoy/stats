	install.packages('car');
	library(car);

	e141.table = read.table('ex141.csv', sep=',', header=T);
	ratio = e141.table$cases/e141.table$miners;

	e141.glm = glm(y ~ x, data=e141.table, family=binomial());
	summary(e141.glm);

	# estimated beta.hat 
	beta.hat = e141.glm$coef;

	# covariance matrix of model paramaters beta	
	vcov(e141.glm); # not matching book result. check with matlab!!!!!!!!!!

	plot(e141.table$x, e141.table$y, pch=23, cex=2, bg='orange');
	lines(e141.table$x, e141.table$y, lwd=2, col='orange');
	lines(e141.table$x, fitted(e141.glm), lwd=2, col='red');
	
	## Odd Ratio can be intepreted as the estimated increase in the probability 
	## of success associated with a one-unit change in the value of predictor variable
	odd.Ratio = exp(beta.hat[2]);
	print(odd.Ratio);

	# odd.Ratio = 0.88
	# Imply that every additional year of exposure increase the odds
	# of contracting a severe case of the pneumoconiosis by 8.8%
	# if the exposure time increases by 10 years then the 
	# odds ratio become exp(d*beta1) = exp[10*0.08467792] = 2.5
	# i.e. the odds more than double with in a 10-year exposure.
   

	# The logistic curve is a sigmoidal curve starting at 0 at , and rising to 1. 
	# Hence, it is a distribution function, also called the logistic distribution
	g <- function(x) {
		return(log(x / (1 - x)))
	}

	g.inv <- function(y) {
	   return(exp(y) / (1 + exp(y)))
	}

	p = seq(g(0.01), g(0.99), length=200);
	plot(p, g.inv(p), lwd=2, type='l', col='red');

	# The logistic transform, or logit is the quantile function of this logistic distribution.
	x = seq(0.01,0.99,length=200)
	plot(x, g(x), lwd=2, type='l', col='red')

	# fit a logistic regression model to it using the R function glm.
	
	url = 'http://stats191.stanford.edu/data/flu.table';
	flu.table = read.table(url, header=T);

	pairs(flu.table, cex.labels=3, pch=23, bg='orange', cex=2);

	# Fitting the model involving to specify the family.
	flu.glm = glm(Shot ~ Age + Health.Aware, data=flu.table, family=binomial())
	summary(flu.glm);

	# Diagnostics can be plotted just as for lm. The residuals used here are what are called deviance residuals
	plot(flu.glm);


	# Influence measures can also be investigated
	
	library(car);
	influence.measures(flu.glm);

	# The function predict works just as for lm but it does not always give the output on the scale of the response, which is 0-1 for binary responses.

	a = predict(flu.glm, list(Age=c(35,45),Health.Aware=c(50,50)));
	b = predict(flu.glm, list(Age=c(35,45),Health.Aware=c(50,50)), type="response")
	c = exp(a)/(1+exp(a))
	print(c == b);

	# Comparing models is done with deviance instead of sums of squares. The function anova will do this automatically for us. Note that the p-value is different 
        # using anova and the summary. This is because the p-value in the summary table is based on a Normal approximation and not a difference of deviances. In the lm case,
        # these two things are identical, but not necessarily in the glm case. The test using the summary table is called a Wald test, whereas the deviance one is called a
        # likelihood ratio test.

	reduced.glm = glm(Shot ~ Health.Aware, family=binomial(), data=flu.table);
	anova(reduced.glm, flu.glm);
	summary(flu.glm);

	# Confidence intervals are also slightly different. They are constructed using the likelihood, not the summary table (which would be Wald confidence intervals)
	confint(flu.glm);

	# Model selection 
	step.glm = step(flu.glm, scope=list(upper=~.^2), direction='both');
	summary(step.glm);


	# probit and cloglog models. This is specified using the link argument to binomial, which defaults to logit.
	summary(glm(Shot ~ Age + Health.Aware, data=flu.table, family=binomial(link='probit')));
	summary(glm(Shot ~ Age + Health.Aware, data=flu.table, family=binomial(link='cloglog')));

	# Logistic (binary regression models) can be fit using Iteratively Reweighted Least Squares (IRLS). 
        # For logistic regression, this algorithm is an implementation of the Newton-Raphson algorithm used to minimize smooth functions.
	# In R, all of this is done for by glm.


	g <- function(x) {
		return(log(x / (1 - x)))
	}

	g.inv <- function(y) {
	   return(exp(y) / (1 + exp(y)))
	}

	# We need the derivative of g
	g.prime = function(x) {
		return(1/(x*(1-x)))
	}

	# And the "variance function"

	V = function(mu) {
		return(mu*(1-mu));
	}

	# Iteratively reweighted least squares
	niter = 10;
	mu = rep(mean(flu.table$Shot), length(flu.table$Shot));

	for (i in 1:niter) {
		Z = g(mu) + g.prime(mu) * (flu.table$Shot - mu);
		W = 1 / (g.prime(mu)^2 * V(mu));
		Z.lm = lm(Z ~ Age + Health.Aware, weights=W, data=flu.table);
		eta = predict(Z.lm);
		mu = g.inv(eta);
		beta = Z.lm$coef;
	}

	X = model.matrix(Z.lm);
	SE.beta = sqrt(diag(solve(t(X) %*% diag(W) %*% X)));
	T.beta = beta / SE.beta;

	# compare with the ones output by glm
	summary(flu.glm);


