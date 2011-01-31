
	rm(list=ls(all=TRUE))

# load in the data
	mdata = read.table('example28.csv', sep=',', header=F);

# Put x and y in R's toplevel namespace
	names(mdata) = c('y','x');
	attach(mdata);

	# scatter plot 
	plot(x, y, pch=23, bg='red', cex=2);

	# fit a simple linear model, omitting intercept
	model1= lm(y ~ x - 1);

	# Add line to the plot indicating the best fitting line
	# abline(model1, lwd=3, col='blue'); 
	lines(x,predict(model1), lwd=3, col='blue');

	summary(model1);

# confidence intervals for the coefficients in the regression model
	confint(model1, level=0.95);










