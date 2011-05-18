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


	deli_data = read.table('delivery.data', sep=',', header=F);
	names(deli_data) = c('Time','Cases', 'Distance');
	attach(deli_data);

	n = nrow(deli_data);

	deli.lm= lm(Time ~ Cases + Distance, data=deli_data);	
	
	influence(deli.lm);

	infl = influence.measures(deli.lm);

	# residuals e = y^ - y
	e = resid(deli.lm);
	
	# studentized residuals: ri = ei / (sqrt(MSE*(1-hii))
	r = rstandard(deli.lm);

	# R-student ti = ei / (sqrt(S(i)^2*(1-hii))
	t = rstudent(deli.lm);

	# diagonal values of the hat matrix H
	hii = hatvalues(deli.lm);

	# 3 residuals matrix
	res = cbind(e,r,t);
	cbind(infl$infmat, res);
	
	# fig 4.2, 
	qqnorm(e, main = "Q-Q Plot of ord least square residuals"); qqline(e, col = 'red');
	qqnorm(r, main = "Q-Q Plot of studentized      residuals"); qqline(r, col = 'red');

	# fig 4.4
	plot(fitted(deli.lm), resid(deli.lm), pch=23, bg='orange', cex=2);
	abline(h=0,lty=2,col="red")

	plot(fitted(deli.lm), rstandard(deli.lm), pch=23, bg='orange', cex=2);
	abline(h=0,lty=2,col="red")


	# Fig 4.7 - Partial regression plots (i.e. added variable plots)
	par(mfrow=c(1,2));
	lm1 = lm(Time  ~  Distance);
	lm2 = lm(Cases ~  Distance);

	## the two plots below must be identical 
	avPlots(deli.lm, 'Cases', pch=23,bg='orange', cex=2);
	plot(resid(lm2), resid(lm1), pch=23,bg='orange', cex=2);
	par(mfrow=c(1,1));

	avPlots(deli.lm, pch=23,bg='orange', cex=2);

	par(mfrow=c(1,2));
	avPlots(deli.lm, 'Cases', pch=23, bg='orange', cex=2);
	crPlots(deli.lm, 'Cases', pch=23, bg='orange', cex=2);
	
	par(mfrow=c(1,2));
	avPlots(deli.lm, 'Distance', pch=2, col='blue', lwd=2);
	crPlots(deli.lm, 'Distance', pch=2, col='blue', lwd=2);




	plot(dfbetas(m422)[,'x1'], pch=23, bg='orange', cex=2, ylab="DFBETA (x1)");
	plot(dfbetas(m422)[,'x3'], pch=23, bg='orange', cex=2, ylab="DFBETA (x3)");
	par(mfrow=c(1,2));
	plot(dfbetas(m422)[,'x2'], pch=23, bg='orange', cex=2, ylab="DFBETA (x2)");
	plot(dfbetas(m422)[,'x4'], pch=23, bg='orange', cex=2, ylab="DFBETA (x4)");

	plot(dffits(m422), pch=23, bg='orange', cex=2, ylab="DFFITS");

	plot(cooks.distance(m422), pch=23,bg='orange', cex=2, ylab="Cook's distance");

	# Bonferroni outlier test
	outlierTest(m422);
	influence.measures(m422);

	subs = c(1,seq(3,25));
	b14b = b14[subs,];
	m422b=lm(y ~ x1+x2+x3+x4, data=b14b);
	# old model 
	m422;
	# new model - without observation 2

	m422b;
	plot(m422b, pch=23 ,bg='orange',cex=2);

	par(mfrow=c(1,2));
	plot(dfbetas(m422b)[,'x2'], pch=23, bg='orange', cex=2, ylab="DFBETA (x2)");
	plot(dfbetas(m422b)[,'x4'], pch=23, bg='orange', cex=2, ylab="DFBETA (x4)");

	par(mfrow=c(1,1));
	plot(dffits(m422b), pch=23, bg='orange', cex=2, ylab="DFFITS");
	plot(cooks.distance(m422b), pch=23,bg='orange', cex=2, ylab="Cook's distance");

	# Bonferroni outlier test
	outlierTest(m422b);

	#All the influence measures
	influence.measures(m422b);

	res = resid(m422b);

	plot(predict(m422b), resid(m422b), pch=23,bg='orange', cex=2);
	par(mfrow=c(1,2));
	plot(predict(m422b), rstandard(m422b), pch=23,bg='orange', cex=1);
	plot(predict(m422b), rstudent(m422b), pch=23,bg='orange', cex=1);
	

	m516b=lm(log(y) ~ x1+x2+x3+x4, data=b14b);
	# model m516b- without obs 2 and use log transformation on y
	m516b
	plot(m516b, pch=23 ,bg='orange',cex=2);

	par(mfrow=c(1,2));
	plot(dfbetas(m516b)[,'x2'], pch=23, bg='orange', cex=2, ylab="DFBETA (x2)");
	plot(dfbetas(m516b)[,'x4'], pch=23, bg='orange', cex=2, ylab="DFBETA (x4)");

	par(mfrow=c(1,2));
	plot(dfbetas(m516b)[,'x1'], pch=23, bg='orange', cex=2, ylab="DFBETA (x1)");
	plot(dfbetas(m516b)[,'x3'], pch=23, bg='orange', cex=2, ylab="DFBETA (x3)");

	par(mfrow=c(1,1));
	plot(dffits(m516b), pch=23, bg='orange', cex=2, ylab="DFFITS");
	plot(cooks.distance(m516b), pch=23,bg='orange', cex=2, ylab="Cook's distance");

	# Bonferroni outlier test
	outlierTest(m516b);

	#All the influence measures
	influence.measures(m516b);

	res = resid(m516b);

	par(mfrow=c(1,2));
	avPlots(m516b, 'x3', pch=23,bg='orange', cex=2);
	avPlots(m516b, 'x4', pch=23,bg='orange', cex=2);

	#Hat values
	plot(hatvalues(m516b), pch=23, bg='orange', cex=2, ylab='Hat values');

	#Component+residual plots
	par(mfrow=c(1,2));
	crPlots(m516b, 'x1', pch=19, cex=2);
	crPlots(m516b, 'x2', pch=19, cex=2);

	par(mfrow=c(1,2));
	crPlots(m516b, 'x3', pch=19, cex=2);
	crPlots(m516b, 'x4', pch=19, cex=2);


	# Now using log transformation both repressors and response variables.
	m516c=lm(log(y) ~ log(x1)+log(x2)+log(x3)+log(x4), data=b14b);
	m516c
	plot(m516c, pch=23 ,bg='orange',cex=2);

	# Bonferroni outlier test
	outlierTest(m516c);

	#All the influence measures
	influence.measures(m516c);




