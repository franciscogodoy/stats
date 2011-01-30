# load in the data
	mdata = read.table('example21.csv', sep=',', header=F);

# Put x and y in R's toplevel namespace
	names(mdata) = c('y','x');
	attach(mdata);

# simple linear reg model
	model1= lm(y ~ x);
	plot(x, y, pch=23, bg='red', cex=2);

# Add line to the plot indicating the best fitting line
	abline(model1, lwd=3, col='blue');

# Compute reg coefficients by hand
	beta.1.hat <- cov(x, y) / var(x) ;
	beta.0.hat <- mean(y) - beta.1.hat * mean(x);
	print(c(beta.1.hat, beta.0.hat));
	print(coef(model1));

# Estimate sigma squared 
	MSE <- sum(resid(model1)^2) / model1$df.resid;
	print(MSE);

# print all info at once 
	summary(model1);

#confidence intervals for the coefficients in the regression model
	confint(model1, level=0.95);


plot the density of chisquare with 18 df. 
	xseq = seq(0,30,0.01);
	plot(xseq, dchisq(xseq, 18), xlab='s', ylab='Density -- f(s)', type='l', lwd=3, col='red');

#confidence intervals for the sigma square
	upr = (model1$df.resid * MSE)/ qchisq(0.025, model1$df.resid);
	lwr = (model1$df.resid * MSE)/ qchisq(0.975, model1$df.resid);
	sigma.square.CI = c(lwr, MSE, upr);
	print(sigma.square.CI);



