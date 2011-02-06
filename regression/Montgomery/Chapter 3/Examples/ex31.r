# load in the data
	mdata = read.table('ex31.csv', sep=',', header=F);

# Put x and y in R's toplevel namespace
	names(mdata) = c('y','x1', 'x2');
	attach(mdata);
	X = cbind(1,x1,x2);

% use lm function to compute regression coefficients
	model31= lm(y ~ x1+x2);
	summary(model31);

% Use (Naive) analytical formula to estimate beta
	beta = solve(t(X)%*%X) %*% t(X) %*% y;

% Use QR decomposition formula to estimate beta
	qr.coef(qr(X),y);

