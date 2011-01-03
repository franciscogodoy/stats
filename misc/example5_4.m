

% Example 5.3 - Constructing a confidence ellipse for mean vector mu
clear all;
load example_5_3.dat;

% transform 1st and 2nd columns of example_5_3 matrix to near normality 
X1 = example_5_3(:,1).^(1/4);
X2 = example_5_3(:,2).^(1/4);

% put X1, X2 into matrix X
X = [X1, X2];

% n is number of observations,  p is number of features/variables
[n,p] = size(X);

% compute sample mean vector X_bar and sample covariance matrix S
X_bar = mean(X)';
S = cov(X);
S_inv = inv(S);

% Sii is list containing all variances Xi, i = 1,2,...,p. 
Sii = diag(S);

% compute simultanoous T_square intervals at the level of 95% confidence
t_square_int_weight = (p*(n-1)/(n-p)) * finv(.95,p,n-p);
t_square_intervals(:,1) = X_bar - sqrt(t_square_int_weight)*sqrt(Sii/n);
t_square_intervals(:,2) = X_bar + sqrt(t_square_int_weight)*sqrt(Sii/n);

% compute Bonfferoni intervals at the level of 95% confidence
bonfferoni_int_weight = tinv(1-0.05/(2*p), n-1);
bonfferoni_intervals(:,1) = X_bar - bonfferoni_int_weight*sqrt(Sii/n);
bonfferoni_intervals(:,2) = X_bar + bonfferoni_int_weight*sqrt(Sii/n);

t_square_intervals
bonfferoni_intervals
