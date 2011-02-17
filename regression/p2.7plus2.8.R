	mdata = read.table('p27.csv', sep=',', header=T);
	names(mdata) = c('y','x');
	attach(mdata);

	plot(x, y, pch=23, bg='orange', cex=2);

# Fit simple linear reg model by hand
# Compute reg coefficients by hand

	beta1.hat = cov(x, y) / var(x);
	beta0.hat = mean(y) - beta1.hat * mean(x);
	data.frame(beta0.hat, beta1.hat);

# Test the hypothesis Ho: ß1=0
	Sxx = sum((x - mean(x))^2);
	X = cbind(1, x);
	beta.hat = rbind(beta0.hat, beta1.hat);
	y.hat = X %*% beta.hat;
	SSE = sum((y-y.hat)^2) ;
	n = nrow(mdata);
	df.num = n-2;
	MSE = SSE / df.num;

# compute the test statistic t
	t = beta1.hat/sqrt(MSE/Sxx);
	p.value = 2*(1 - pt(t, df.num));
	data.frame(t, df.num, p.value);


# Calculate  R^2
	SST = sum((y - mean(y))^2) ;
	R.square = 1 - SSE/SST;
	data.frame(SSE, SST, R.square);
	
# 95% CI on the slope
	beta1.se = sqrt(MSE/Sxx);
	lwr = beta1.hat - qt(0.975, n-2) * beta1.se;
	upr = beta1.hat + qt(0.975, n-2) * beta1.se;
	data.frame(lwr, upr);

# 95% CI on E(y) when x0 = 1.00
	x0 = 1.00;
	y0.hat = c(1,1.00)%*%beta.hat;
	y0.lwr = y0.hat - qt(0.975, n-2) * sqrt(MSE*(1/n + (x0 - mean(x))^2/Sxx ));
	y0.upr = y0.hat + qt(0.975, n-2) * sqrt(MSE*(1/n + (x0 - mean(x))^2/Sxx ));
	data.frame(y0.lwr, y0.upr);

# output summary information about the model 
	p27.lm = lm(y ~ x);
	summary(p27.lm);

# 95% CI on the slope
	confint(p27.lm, level=0.95);

# 95% CI on E(y) when x0 = 1.00
	predict(p27.lm, list(x=1.00), interval='confidence', level=0.95);

# find correlation r between x and y.
	r = sqrt(R.square)

# Test the hypothesis Ho: ?=0
	t = r*sqrt(df.num)/sqrt(1-R.square);
	p.value = 2*(1 - pt(t, df.num));
	data.frame(t, df.num, p.value);

# 95% CI for ?
	lwr = tanh(atanh(r) - qnorm(0.975)/sqrt(n-3));
	upr = tanh(atanh(r) + qnorm(0.975)/sqrt(n-3));
	data.frame(lwr,  upr);


