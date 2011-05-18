	install.packages('car');
	install.packages('calibrate');

	library(car);
	library(calibrate);

	##########################
	#x <- runif(50)
	#y <- runif(50)
	#plot(x,y, pch=23, bg='orange', cex=2)
	#textxy(x,y,c(5,10,20),m=c(mean(x),mean(y)))
	##########################

	b14 = read.table('b14.csv', sep=',', header=T);
	row.names(b14) <- seq(1,25);

	attach(b14);
	pairs(b14, pch=23, bg='orange', cex.labels=4, cex=2);
	plot(b14[,c(1,2,6)], pch=23, bg='orange', cex=2);
	plot(b14[,c(3,4,6)], pch=23, bg='orange', cex=2);

	n = nrow(b14);

	m422= lm(y ~ x1+x2+x3+x4, data=b14);
	summary(m422);

	plot(m422, pch=23, bg='orange',cex=2);
	influencePlot(m422, main="Influence Plot", sub="Circle size is proportial to Cook's Distance", id.method="identify", col='red')
	plot(cooks.distance(m422), pch=23,bg='orange', cex=2, ylab="Cook's distance", id.method="identify");
	cutoff <- 4/((nrow(m422)-length(m422$coefficients)-2)) 
	plot(m422, which=4, cook.levels=cutoff)

	par(mfrow=c(2,2));
	layout(matrix(c(1,2,3,4),2,2))
	avPlots(m422, 'x1', pch=23,bg='orange', cex=2, id.method="identify");
	avPlots(m422, 'x2', pch=23,bg='orange', cex=2, id.method="identify");
	avPlots(m422, 'x3', pch=23,bg='orange', cex=2, id.method="identify");
	avPlots(m422, 'x4', pch=23,bg='orange', cex=2, id.method="identify");

	e = resid(m422);
	r = rstandard(m422);
	t = rstudent(m422);
	hii = hatvalues(m422);
	cbind(e,r,t,hii);

	influence.measures(m422);
	
	# Bonferroni outlier test
	outlierTest(m422);

	# delete onservation 2
	remove2 = c(1,seq(3,25));
	b14b = b14[remove2,];
	m422b=lm(y ~ x1+x2+x3+x4, data=b14b);
	# compare old model with new model 
	m422;
	m422b;

	plot(m422b, pch=23 ,bg='orange',cex=2);

	influencePlot(m422b, main="Influence Plot", sub="Circle size is proportial to Cook's Distance", id.method="identify", col='red')
	plot(cooks.distance(m422b), pch=23,bg='orange', cex=2, ylab="Cook's distance", id.method="identify");
	cutoff <- 4/((nrow(m422b)-length(m422$coefficients)-2)) ;
	plot(m422b, which=4, cook.levels=cutoff);


	# Bonferroni outlier test
	outlierTest(m422b);

	#All the influence measures
	influence.measures(m422b);

	e = resid(m422b);
	r = rstandard(m422b);
	t = rstudent(m422b);
	hii = hatvalues(m422b);
	cbind(e,r,t,hii);


	#### ex 5.16
	m516a=lm(y ~ x1+x2+x3+x4, data=b14b);
	plot(m516a, pch=23 ,bg='orange',cex=2);

	e = resid(m516a);
	r = rstandard(m516a);
	t = rstudent(m516a);
	pred = predict(m516a)

	#All the influence measures
	influence.measures(m516a);

	plot(pred, e, pch=23 ,bg='orange',cex=2, main="residuals vs predicted values", xlab="predicted values", ylab="residuals");
	#textxy(pred,e,c(2,4,8),m=c(mean(pred),mean(e))+4);

	plot(pred, r, pch=23 ,bg='orange',cex=2, main="studentized residuals vs predicted values", xlab="predicted values", ylab="studentized residuals");
	plot(pred, t, pch=23 ,bg='orange',cex=2, main="R-student vs predicted values", xlab="predicted values", ylab="R-student");

	# ex 5.16b 
	m516b=lm(log(y) ~ x1+x2+x3+x4, data=b14);
	m516b

	plot(m516b, pch=23 ,bg='orange',cex=2);

	e = resid(m516b);
	r = rstandard(m516b);
	t = rstudent(m516b);
	pred = predict(m516b)

	#All the influence measures
	influence.measures(m516b);
	

	# ex 5.16c
	#Component+residual plots
	par(mfrow=c(1,2));
	avPlots(m516a, 'x1', pch=2,col='blue', main="partial regression plot");
	crPlots(m516a, 'x1', pch=2,col='blue', main="partial residual plot");

	par(mfrow=c(1,2));
	avPlots(m516a, 'x2', pch=2,col='blue', main="partial regression plot");
	crPlots(m516a, 'x2', pch=2,col='blue', main="partial residual plot");

	par(mfrow=c(1,2));
	avPlots(m516a, 'x3', pch=2,col='blue', main="partial regression plot");
	crPlots(m516a, 'x3', pch=2,col='blue', main="partial residual plot");

	par(mfrow=c(1,2));
	avPlots(m516a, 'x4', pch=2,col='blue', main="partial regression plot");
	crPlots(m516a, 'x4', pch=2,col='blue', main="partial residual plot");

	ceres.plots(m516a, one.page=TRUE, pch=2,col='blue', ask=FALSE);

	# non-constant error variance test
	ncvTest(m516a);

	# Evaluate Collinearity
	vif(m516a) ;
	sqrt(vif(m516a)) > 2 # problem?

	# Test for Autocorrelated Errors
	durbin.watson(m516a);

	# Now using log transformation both repressors and response variables.
	m516c=lm(log(y) ~ log(x1)+ log(x2) + log(x3) + log(x4), data=b14b);
	m516c
	plot(m516c, pch=23 ,bg='orange',cex=2);
	qqPlot(m516c, main="QQ Plot", pch=2,col='blue', id.method="identify")
	ceres.plots(m516a, one.page=TRUE, pch=2,col='blue', ask=FALSE);

	e = resid(m516c);
	r = rstandard(m516c);
	t = rstudent(m516c);
	cbind(e,r,t);

	#All the influence measures
	influence.measures(m516c);

	# Bonferroni outlier test
	outlierTest(m516c);




