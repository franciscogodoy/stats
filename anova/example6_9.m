% Example 6.9
load Ex6_9.dat;
X = Ex6_9;
g = 3; p = 2;
nl_arr = [3 2 3];
n = sum(nl_arr);
X_mean = mean(X(:,2:3))';

% Compute B matrix;
B = [0 0; 0 0];
for l = 1:g
    nl = nl_arr(l);
    groupl_idx = find(X(:,1) == l);
    Xl_mean = mean(X(groupl_idx,2:3))';
    B = B + nl*(Xl_mean - X_mean)*(Xl_mean - X_mean)';
end; 

% Compute W matrix;
W = [0 0; 0 0];
for l = 1:g
    nl = nl_arr(l);
    groupl_idx = find(X(:,1) == l);
    Sl = cov(X(groupl_idx,2:3));
    W = W + (nl-1)*Sl;
end;

T = B + W;
lambda = det(W)/det(T);

% Using table 6.3, p.303, to compute the exact test statistics

% Can also use Barlette correction if n is large!
% barlet_stat = -(n-1-(p+g)/2) * log (det(W)/det(T));
% crit_value  = chi2inv(0.99,p*(g-1));

test_stat = ((sum(nl_arr) - g - 1)/(g-1))*((1-sqrt(lambda))/sqrt(lambda))
crit_value = finv(0.99, 2*(g-1), 2*(n-g-1))
if test_stat <= crit_value 
    sprintf('No treatment effects. test_stat = %.2f <= %.2f', test_stat, crit_value)
else   
    sprintf('Treatment difference exist! test_stat = %.2f > %.2f', test_stat, crit_value)    
end 

