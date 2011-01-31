# load in the data
	mdata = read.table('example21.csv', sep=',', header=F);

# Put x and y in R's toplevel namespace
	names(mdata) = c('y','x');
	attach(mdata);

# simple linear reg model
	model1= lm(y ~ x);
	plot(x, y, pch=23, bg='red', cex=2);

	# Add line to the plot indicating the best fitting line
	# abline(model1, lwd=3, col='blue'); 
	lines(x,predict(model1), lwd=3, col='blue');

# Compute reg coefficients by hand
	beta.1.hat <- cov(x, y) / var(x) ;
	beta.0.hat <- mean(y) - beta.1.hat * mean(x);
	print(c(beta.1.hat, beta.0.hat));
	print(coef(model1));

# Estimate sigma squared 
	Sxx = sum((x - mean(x))^2);
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

# Plot the density of T distribution 
	xseq <- seq(-3,3,0.01);
	plot(xseq, dt(xseq, 18), xlab='s', ylab='Density -- f(s)', type='l', lwd=3, col='red');	
	lines(xseq, dnorm(xseq), lty=2, lwd=3, col='blue');
	abline(v=qt(0.975,18), lty=1, col='red');
	abline(v=qt(0.025,18), lty=1, col='red');
	legend(-1,0.2, c('t, 18 df', 'Normal'), lty=c(1,2), col=c('red', 'blue'));

# Manually compute CI on the mean response at point x = x0 
	x0 = 13.3625;
	y0.hat = beta.0.hat + beta.1.hat * x0;
	lwr = y0.hat  - qt(0.975,18) * sqrt(MSE * (1/20 + ((x0-13.3625)^2)/Sxx));
	upr = y0.hat  + qt(0.975,18) * sqrt(MSE * (1/20 + ((x0-13.3625)^2)/Sxx));
	conf_ints = data.frame(lwr, y0.hat, upr);
	names(conf_ints) = list("lwr", "fit", "upr");
	print(round(conf_ints,2));

# Use predict function to compute CI on the mean response at point x = x0 
	predict(model1, list(x=13.3625), interval='confidence', level=0.95);

# Manually compute prediction Interval for the future observation y0
	x0 = 10;
	y0.hat = beta.0.hat + beta.1.hat * x0;
	lwr = y0.hat  - qt(0.975,18) * sqrt(MSE * (1+1/20 + ((x0-13.3625)^2)/Sxx));
	upr = y0.hat  + qt(0.975,18) * sqrt(MSE * (1+ 1/20 + ((x0-13.3625)^2)/Sxx));
	pred_ints = data.frame(lwr, y0.hat, upr);
	names(pred_ints) = list("lwr", "fit", "upr");
	print(round(pred_ints,2));

# Use predict function
	predict(model1, list(x=10), interval='prediction', level=0.95);

# Plot the confidence and prediction bands 
	new <- data.frame(x = seq(-, 28, 0.5))
	#new <- data.frame(x = x)
	predict(model1, new, se.fit = TRUE)
	pred.w.plim <- predict(model1, new, interval="prediction")
	pred.w.clim <- predict(model1, new, interval="confidence")
	matplot(new$x,cbind(pred.w.clim, pred.w.plim[,-1]), lty=c(1,2,2,3,3), lwd=2, type="l", ylab="predicted y")     
	points(x, y, pch=23, bg='red', cex=1);


