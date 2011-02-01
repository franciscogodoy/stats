	detach(mdata);
	rm(list=ls(all=TRUE))

# load in the data
	mdata = read.table('example29.csv', sep=',', header=F);

# Put x and y in R's toplevel namespace
	names(mdata) = c('y','x');
	attach(mdata);

	# scatter plot 
	plot(x, y, pch=23, bg='red', cex=2);

	# fit a simple linear model, omitting intercept
	model1= lm(y ~ x);

	# Add line to the plot indicating the best fitting line
	# abline(model1, lwd=3, col='blue'); 
	lines(x,predict(model1), lwd=3, col='blue');

	summary(model1);

# confidence intervals for the coefficients in the regression model
	confint(model1, level=0.95);

# Test Ho: p = 0 vs H1: p != 0
	n = nrow(mdata);
	r = cor(x,y);
	t = r * sqrt(n-2) / sqrt(1-r^2);
	t.quant =  qt(0.975,n-2);
	if (t > t.quant) {
		print('reject Ho: p = 0 ');
	}

# Construct 95% confedince interval on p
	lwr = tanh(atanh(r) - qnorm(0.975)/sqrt(n-3));
	upr = tanh(atanh(r) + qnorm(0.975)/sqrt(n-3));
	mdat = matrix(c(lwr , upr), nrow = 1, ncol=2, byrow=TRUE,
          dimnames = list(c("*"), c("lwr", "upr")));

	print(mdat);
	









