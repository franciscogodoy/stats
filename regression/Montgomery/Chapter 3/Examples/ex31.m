data = load('ex31.csv');

y = data(:,1);
X = data(:,2:3);
X = [ones(25,1), X];

% use regress to compute regression coefficients
b.hat = regress(y, X);

% Use (Naive) analytical formula to estimate beta
beta_hat = inv(X'*X)*X'*y;

% Use QR decomposition formular to estimate beta
[Q1,R] = qr(X,0);
beta_qr = inv(R)*Q1'*y;
