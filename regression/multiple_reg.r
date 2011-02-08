# load in the data
	data31 = read.table('ex31.csv', sep=',', header=F);

# Put x and y in R's toplevel namespace
	names(data31) = c('Time','Cases', 'Distance');
	attach(data31);
	pairs(data31, pch=23, bg='orange', cex.labels=4, cex=2);
	X = cbind(1,Cases, Distance);
	y = Time;
	n = length(y);

% use lm function to compute regression coefficients
	model31= lm(Time ~ Cases + Distance);
	summary(model31);

% Use analytical formula to estimate beta
	beta = solve(t(X)%*%X) %*% t(X) %*% y;

% Use QR decomposition formula to estimate beta
	qr.coef(qr(X),y);

% Confidence intervals for the coefficients
	confint(model31, level = 0.9);

# confidence and prediction intervals for regression function at new values of the covariates.
	x0 = list(Cases=8,Distance=275);
	predict(model31, x0, interval='confidence', level = 0.95);
	predict(model31, x0, interval='prediction', level = 0.95);

# The sums of squares and R.square
	n = length(y);
	SST = sum((y - mean(y))^2);
	MST = SST / ( n - 1);
	SSE = sum(resid(model31)^2);
	MSE = SSE / model31$df.residual;

	R2 = 1 - SSE/SST;
	R2.adj = 1 - MSE / MST;
	data.frame(R2,R2.adj);


# Estimated coefficients are called partial regression coefficients. 
# The coefficient for Distance can be thought of as the simple linear regression 
# coefficient of y on Distance after regressing out other regressors.

# the coefficient for Distance is what we get if we first regress y and Distance onto
# Cases, and take the residuals, creating two new variables, Z and W and then fit a simple linear regression model with Z as the outcome and W as the covariates.
	model31$coef;

# regress Distance onto Cases
# and call the residuals W
# this is the part of Distance that is
# not explained by any of the other variables
	W = resid(lm(Distance~ Cases));

# do the same for Y
	Z = resid(lm(y ~ Cases));
	lm(Z~W)$coef["W"];

# construct Least squares estimates by hand
	colnames(X)[1] = '(Intercept)';

# solve for beta.hat
	beta.hat = solve(t(X) %*% X) %*% t(X) %*% y;
	y.hat = X %*% beta.hat;

# covariance matrix
	sigma.hat = sqrt(sum((y - y.hat)^2) / (n - ncol(X)))
	cov.beta = sigma.hat^2 * solve(t(X) %*% X);
	sqrt(diag(cov.beta));

# get the covariance matrix from the lm model 
	vcov(model31);

# compute confidence interval (can be used for predicition intervals with “extra” argument)
	CI.lm = function(model, x, level=0.95, extra=0)
	{
		# the center of the confidence interval
		center = sum(x*model$coef);

		# the estimate of sigma^2
		sigma.sq = sum(resid(model)^2) / model$df.resid;

		# the standard error of sum(x*cur.lm$coef)
		se = sqrt(extra * sigma.sq + sum((x %*% vcov(model)) * x));

		# the degrees of freedom for the t-statistic
		df = model$df;

		# the quantile used in the confidence interval
		q = qt((1 - level)/2, df, lower.tail=F);

		# upper, lower limits
		U = center + se * q;
		L = center - se * q;
		return(data.frame(center, L, U));
	}

	CI.lm(model31, c(1,8,275));
	predict(model31,	list(Cases=8,Distance=275), interval='confidence');

	CI.lm(model31, c(1,8,275), extra=1);
	predict(model31,	list(Cases=8,Distance=275), interval='prediction');

# F test based on comparing the models
	f.test = function(R.lm, F.lm) {
		SSE.R = sum(resid(R.lm)^2);
		SSE.F = sum(resid(F.lm)^2);
		df.num = R.lm$df - F.lm$df;
		df.den = F.lm$df;
		F = ((SSE.R - SSE.F) / df.num) / (SSE.F / df.den);
		p.value = 1 - pf(F, df.num, df.den);
		return(data.frame(F, df.num, df.den, p.value));
	}

	reduced.model = lm(Time ~ Cases);
	f.test(reduced.model, model31);

# Imposing constraints on parameters
# Test IF the coefficients for Cases (beta1) and Distance (beta2) are the same 
# Ho: beta1 = beta2 vs H1: beta1 <> beta2 

	F.lm = lm(Time~Cases+Distance)
	Z = Cases+Distance;
	equal.lm = lm(Time ~ Z);
	anova(equal.lm,F.lm); # p.value = 5.192e-09, thus reject H0

# Ho: beta1 + beta2 = 1
 
# F.lm: y    = beta0 + beta1*X1 + beta2*X2
# R.lm: y    = beta0 + beta1*X1 + (1-beta1)*X2 
# R.lm: y-X2 = beta0 + beta1(X1 - X2)

	Y = Time - Distance;
	X1 = Cases - Distance;
	R.lm = lm(Y ~ X1);
	f.test(R.lm, F.lm); #  p.value < 0.01, reject H0 

# Ho: beta1 + beta2 = 1.6

# F.lm: y    = beta0 + beta1*X1 + beta2*X2
# R.lm: y    = beta0 + beta1*X1 + (1.63-beta1)*X2 
# R.lm: y-1.6*X2 = beta0 + beta1(X1 - X2)

	Y = Time - 1.6*Distance;
	X1 = Cases - Distance;
	R.lm = lm(Y ~ X1);
	f.test(R.lm, F.lm); # p.value = 0.858, no evidence to reject H0


# General linear hypothesis, pp. 92, Montgomery, 4th edition 
# Ho: T*beta = c vs H1: T*beta <> c

# test statistic: F = ((T*beta -c)' * [T * inv(X'X) * T'] * (T*beta -c)/r) / SSE(F)/(n-p)


# Ho: beta1 + beta2 = 1	
	T = rbind(c(0, 1, 1));
	c = 1;
	beta = cbind(F.lm$coef);
	X = model.matrix(F.lm);
	df.num = qr(T)$rank;

	F_num = t(T%*%beta - c) %*% solve(T %*% solve(t(X)%*%X) %*% t(T)) %*% (T%*%beta - c)/df.num;
	SSE = sum(resid(F.lm)^2);
	MSE = SSE / F.lm$df.residual;
	F_den = MSE;
	F = F_num/F_den;
	p.value = 1 - pf(F, df.num, F.lm$df.residual);
	print(data.frame(F, df.num, F.lm$df.residual, p.value));

# Ho: beta1 + beta2 = 1.6	
	T = rbind(c(0, 1, 1));
	c = 1.6;

	beta = cbind(F.lm$coef);
	X = model.matrix(F.lm);
	df.num = qr(T)$rank;

	F_num = t(T%*%beta - c) %*% solve(T %*% solve(t(X)%*%X) %*% t(T)) %*% (T%*%beta - c)/df.num;
	SSE = sum(resid(F.lm)^2);
	MSE = SSE / F.lm$df.residual;
	F_den = MSE;
	F = F_num/F_den;
	p.value = 1 - pf(F, df.num, F.lm$df.residual);
	print(data.frame(F, df.num, F.lm$df.residual, p.value));

# Ho: beta1 - beta2 = 2	

	compute_f = function(F.lm, T, c) {
		beta = cbind(F.lm$coef);
		X = model.matrix(F.lm);
		df.num = qr(T)$rank;

		F_num = t(T%*%beta - c) %*% solve(T %*% solve(t(X)%*%X) %*% t(T)) %*% (T%*%beta - c)/df.num;
		SSE = sum(resid(F.lm)^2);
		F_den = SSE / F.lm$df.residual;
		F = F_num/F_den;
		p.value = 1 - pf(F, df.num, F.lm$df.residual);
		return(data.frame(F, df.num, F.lm$df.residual, p.value));
	}

	T = rbind(c(0, 1, -1));
	c = 2;
	print(compute_f(F.lm, T,c));

# Ho: beta1 - beta2 = 1.5
	T = rbind(c(0, 1, -1));
	c = 1.5;
	print(compute_f(F.lm, T,c));


