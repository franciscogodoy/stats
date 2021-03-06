% Example 5.3 - Constructing a confidence ellipse for mean vector mu
clear;
mydata = load('example5_3.dat');
X = mydata .^(1/4);
[n,p] = size(X);

X_mean = mean(X)';
S = cov(X);
S_inv = inv(S);

% V store eigen-vectors , D are eigen values
[V D] = eig(S);
display(V); display(D);

% The direction and lengths of axes of ellipsoid
%   n * (x.mean - mu)'*inv(S)*(x.mean - mu) 
%       <= c^2 = [p(n-1)/(n-p)]*F(.95, p, n-p)     
% are:
ellipse_center = X_mean;
display(ellipse_center);
for i = 1:size(V,2)
    lambda = D(i,i);
    half_len = sqrt(lambda) * sqrt( (p*(n-1)/(n*(n-p))) * finv(.95,p,n-p) );
    fprintf('Axis %d, half-length = %10.4f, direction:  \n', i, half_len);
    disp(V(:,i));    
end

% compute 95% confidence region
critvalue = (p*(n-1)/(n-p)) * finv(.95,p,n-p);

% Calculate T_sq to see if [.562, .589 ]' is in the confidence region?
mu = [.562, .589 ]';
T_sq = n*(X_mean-mu)'*S_inv*(X_mean-mu);

disp(mu);
if T_sq <= critvalue  
    sprintf('IN confidence region. T square = %.2f <= %.2f', T_sq, critvalue)
else   
    sprintf('NOT in confidence region. T square = %.2f > %.2f', T_sq, critvalue)
end        