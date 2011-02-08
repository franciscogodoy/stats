install.packages('car');
library(car);
# load in the data
	data51 = read.table('electric_utility.csv', sep=',', header=F);

# Put x and y in R's toplevel namespace
	names(data51) = c('x','y');
	attach(data51);
	pairs(data51, pch=23, bg='orange', cex.labels=4, cex=2);
	#plot(data51[,1:5], pch=23, bg='orange', cex=2);

	X = cbind(1,x);
	n = length(y);

% use lm function to compute regression coefficients
	model51= lm(y ~ x);
	summary(model51);

	n = length(y);

	SST = sum((y-mean(y))^2);
	MST = SST/ (n-1);
	SSE = sum(resid(model51)^2);
	MSE = SSE / model51$df.residual;
	R2 = 1 - SSE/SST;
	R2.adj = 1 - MSE/MST;
	data.frame(R2,R2.adj);

# Plot R-student values ti vs fitted values 
	plot(model51, pch=23 ,bg='orange',cex=2);

	residual = rstudent(model51);
	y.hat = fitted(model51);

# Plot R-student values ti vs fitted values 
	plot(y.hat, residual, pch=23, bg='orange', cex=2);


	z = sqrt(y);
	model51b = lm(z ~ x);
	plot(model51b, pch=23 ,bg='orange',cex=2);

# Plot R-student values ti vs fitted values 
	plot(fitted(model51b), rstudent(model51b), pch=23, bg='orange', cex=2);


	

	
 