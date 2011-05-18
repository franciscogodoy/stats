	install.packages('car');
	install.packages('MPV');

	library(car);
	library(MPV);


	PressureDrop= read.table('PressureDrop.csv', sep=',', header=T);
	n = nrow(PressureDrop);
	
	initial.lm= lm(y ~ x1+x2+x3+x4, data=PressureDrop);
	summary(initial.lm);
	par(mfrow=c(1,2));
	plot(initial.lm, pch=23 ,bg='orange',cex=1);
	par(mfrow=c(1,1));

	par(mfrow=c(1,2));
	std_res = rstudent(initial.lm);
	qqnorm(std_res, main = "Q-Q Plot of the studentized residuals"); qqline(std_res, col = 'red');
	plot(fitted(initial.lm), std_res, pch=23,bg='orange', cex=1, main ='Sudentized residuals vs fitted values');
	par(mfrow=c(1,1));

	X = model.matrix(initial.lm)[,-1];
	x2 = X[,2]; 
	x3 = X[,3]; 
	par(mfrow=c(1,2));
	plot(x2, std_res, pch=23,bg='orange', cex=1, main ='Sudentized residuals vs x2');
	plot(x3, std_res, pch=23,bg='orange', cex=1, main ='Sudentized residuals vs x3');
	par(mfrow=c(1,1));

	x1 = X[,1]; 
	x4 = X[,4]; 
	par(mfrow=c(1,2));
	plot(x1, std_res, pch=23,bg='orange', cex=1, main ='Sudentized residuals vs x1');
	plot(x4, std_res, pch=23,bg='orange', cex=1, main ='Sudentized residuals vs x4');
	par(mfrow=c(1,1));

	x2x3x4.lm = lm(y ~ x2+x3+x4, data=PressureDrop);	
	x2x3.lm = lm(y ~ x2+x3, data=PressureDrop);
	initial.lm;
	PRESS(initial.lm);

	x2x3x4.lm;
	PRESS(x2x3x4.lm);

	x2x3.lm;
	PRESS(x2x3.lm);

	par(mfrow=c(1,2));
	std_res = rstudent(x2x3.lm);
	qqnorm(std_res, main = "Q-Q Plot of the studentized residuals"); qqline(std_res, col = 'red');
	plot(fitted(x2x3.lm), std_res, pch=23,bg='orange', cex=1, main ='Sudentized residuals vs fitted values');
	par(mfrow=c(1,1));

	par(mfrow=c(1,2));
	plot(x2, std_res, pch=23,bg='orange', cex=1, main ='Sudentized residuals vs x2');
	plot(x3, std_res, pch=23,bg='orange', cex=1, main ='Sudentized residuals vs x3');
	par(mfrow=c(1,1));


	par(mfrow=c(2,2));
	plot(dfbetas(x2x3.lm)[,'x2'], pch=23, bg='orange', cex=2, ylab="DFBETA (x2)", main = "DFBETA (x2)");
	plot(dfbetas(x2x3.lm)[,'x3'], pch=23, bg='orange', cex=2, ylab="DFBETA (x3)", main = "DFBETA (x3)");
	plot(cooks.distance(x2x3.lm), pch=23,bg='orange', cex=2, ylab="Cook's distance", main="Cook's distance plot")
	par(mfrow=c(1,2));

	# influence measures
	influence.measures(x2x3.lm);

	# Bonferroni outlier test
	outlierTest(x2x3.lm);

	# Added variable plots
	par(mfrow=c(1,2));
	avPlots(x2x3.lm, 'x2', pch=23,bg='orange', cex=2);
	avPlots(x2x3.lm, 'x3', pch=23,bg='orange', cex=2);

	par(mfrow=c(1,2));
	avPlots(x2x3.lm, 'x2', pch=23,bg='orange', cex=2);
	avPlots(x2x3.lm, 'x3', pch=23,bg='orange', cex=2);

	#Hat values
	plot(hatvalues(x2x3.lm), pch=23, bg='orange', cex=2, ylab='Hat values');

	#Component+residual plots
	par(mfrow=c(1,2));
	crPlots(x2x3.lm, 'x2', pch=19, cex=2, main="Component+residual plots");
	crPlots(x2x3.lm, 'x3', pch=19, cex=2,main="Component+residual plots");

	# Now using log transformation both repressors and response variables.
	# m516c=lm(log(y) ~ log(x1)+log(x2)+log(x3)+log(x4), data=b14b);

	x2x3.log.lm = lm(log(y) ~ x2+x3, data=PressureDrop);
	summary(x2x3.log.lm);
	summary(x2x3.lm);

	par(mfrow=c(1,2));
	std_res = rstudent(x2x3.log.lm);
	qqnorm(std_res, main = "Q-Q Plot of the studentized residuals"); qqline(std_res, col = 'red');
	plot(fitted(x2x3.log.lm), std_res, pch=23,bg='orange', cex=1, main ='Sudentized residuals vs fitted values');
	par(mfrow=c(1,1));

	PressureDrop$x3f = factor(PressureDrop$x3);
	x2x3.factor.lm = lm(y ~ x2+x3f, data=PressureDrop);
	par(mfrow=c(1,2));
	plot(x2x3.factor.lm);
	boxplot(rstudent(x2x3.factor.lm) ~  PressureDrop$x3f);
