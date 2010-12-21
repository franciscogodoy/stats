% Excercise 5.21- Constructing confidence intervals for data in table 1.8
clear all;
load T1-8.DAT;

X = T1_8;

% n is number of observations,  p is number of features/variables
[n,p] = size(X);

% compute sample mean vector X_bar and sample covariance matrix S
X_bar = mean(X)';
S = cov(X);

% Sii is list containing all variances Xi, i = 1,2,...,p. 
Sii = diag(S);

% compute simultanoous T_square intervals at the level of 95% confidence
% t_square_int_weight = 19.9199;
t_square_int_weight = (p*(n-1)/(n-p)) * finv(.95,p,n-p);
t_square_intervals(:,1) = X_bar - sqrt(t_square_int_weight)*sqrt(Sii/n);
t_square_intervals(:,2) = X_bar + sqrt(t_square_int_weight)*sqrt(Sii/n);

% compute Bonfferoni intervals at the level of 95% confidence
%bonfferoni_int_weight = 2.8751;

bonfferoni_int_weight = tinv(1-0.05/(2*p), n-1);
bonfferoni_intervals(:,1) = X_bar - bonfferoni_int_weight*sqrt(Sii/n);
bonfferoni_intervals(:,2) = X_bar + bonfferoni_int_weight*sqrt(Sii/n);

t_square_intervals
bonfferoni_intervals