  install.packages('car');
	library(car);

  B14.table <- read.csv("C:/Users/th/git/mva/regression/B14.table.csv");


	n = nrow(b14);

	m422= lm(y ~ x1+x2+x3+x4, data=b14);
	summary(m422);
	par(mfrow=c(1,2));
	plot(m422, pch=23 ,bg='orange',cex=1);
	par(mfrow=c(1,1));

	e1 = rstandard(m422);
	qqnorm(e1, main = "Q-Q Plot for the residuals"); qqline(e1, col = 'red');
	identify(rep(1, length(y)), y, labels = seq_along(y));

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

	# Added variable plots
	par(mfrow=c(1,2));
	avPlots(m516b, 'x1', pch=23,bg='orange', cex=2);
	avPlots(m516b, 'x2', pch=23,bg='orange', cex=2);

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




