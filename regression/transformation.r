install.packages('car');
library(car);

############### Transform to stablize variance ##########################
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
	
	## since residuals form an outward-opening funnel 
	## indicates the error variance is increasing as energy consumpsion encreases
	## Here the response y may be view as the "count" of the number of kilowatts used 
	## by a customer during the peak-hour
	## Here sigma^2 is proportional to E(y) 
	## this transformation z = sqrt(y) might be appropriate. 
	
	z = sqrt(y);
	model51b = lm(z ~ x);
	plot(model51b, pch=23 ,bg='orange',cex=2);

# Plot R-student values ti vs fitted values 
	plot(fitted(model51b), rstudent(model51b), pch=23, bg='orange', cex=2);


	#plot(tanh, seq(-1,10,0.1));
	x = seq(-10,50);
	plot (x, 0.5*(x-0)^4, pch=23 ,bg='orange',cex=2);

	detach(data51);
################ Log linear ###############
	url = 'http://stats191.stanford.edu/data/bacteria.table'

	bacteria.table <- read.table(url, header=T)
	attach(bacteria.table)

	plot(bacteria.table, pch=23, cex=2, bg='orange')
	bacteria.lm <- lm(N_t ~ t)
	abline(bacteria.lm$coef)
	par(mfrow=c(2,2))
	plot(bacteria.lm, cex=2, pch=23, bg='orange')
	bacteria.log.lm <- lm(log(N_t) ~ t)
	par(mfrow=c(2,2))
	plot(bacteria.log.lm, cex=2, pch=23, bg='orange')
	par(mfrow=c(1,1))
	plot(bacteria.table, pch=23, cex=2, bg='orange')
	lines(t, fitted(bacteria.lm), lwd=2, col='red')
	lines(t, exp(fitted(bacteria.log.lm)), lwd=2, col='green')
	detach(bacteria.table)


############ Excercise 5.5, log tranformation ############ 
	p55.data = read.table('prob55.csv', sep=',', header=T);
	attach(p55.data);
 	p55.lm= lm(defects ~ weeks);
	plot(weeks, defects, pch=23, cex=2, bg='orange');
	abline(p55.lm$coef);
	summary(p55.lm);

	plot(p55.lm, pch=23 ,bg='orange',cex=2);
	## Here sigma^2 is proportional to [E(y)]^2 
	## transformation z = log(y) might be appropriate. 

	p55.log.lm = lm(log(defects) ~ weeks);
	plot(p55.log.lm, pch=23, bg='orange',cex=2);

	plot(weeks, defects, pch=23, cex=2, bg='orange');
	lines(weeks, fitted(p55.lm), lwd=2, col='red');
	lines(weeks, exp(fitted(p55.log.lm)), lwd=2, col='green');
	detach(p55.data);

############ reciprocal tranformation ############ 
	e52.data = read.table('ex52.csv', sep=',', header=F);
	names(e52.data) = c('x','y');
	attach(e52.data);	
 	e52.lm= lm(y~ x, data = e52.data);
	summary(e52.lm);		
	fitted = fitted(e52.lm);
	err = resid(e52.lm);
	data1 = data.frame(e52.data, fitted, err);

	# sort data1 by x column
	data2  = data1[order(x),]; 
	data3  = data1[order(-x),];
	
	plot(e52.lm) # residuals vs fitted y.hat indicates model inadequacy
	# imply the linear relationship has not captured all of information in the wind speed variable 
 
	# might attempt to use quadratic model Y = beta0 + beta1*X + beta2*X^2 + e
	
	# The scatter plot suggest that as the wind speed increases, DC output approaches an upper limmit of 
	# approximately 2.5. Thus the quadratic model is not appropriate!!

	# A better model that incoporates an upper asymtote would be
	# y = beta0 + beta1*(1/x) + e
	x = 1/x;
	e52.lm2= lm(y~x);
	plot(e52.lm2, pch=23, bg='orange',cex=2);
	summary(e52.lm2);
	
	detach(e52.data);

