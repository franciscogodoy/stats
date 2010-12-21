% Example 6.10

g = 3; p = 4;
nl_arr = [271 138 107];
n = sum(nl_arr);

X1_mean = [2.066 0.480 0.082 0.360]';
X2_mean = [2.167 0.596 0.124 0.418]';
X3_mean = [2.273 0.521 0.125 0.383]';
S1 = [0.291 -0.001 0.002 0.010; -0.001 0.011  0.000 0.003; 0.002 0.000 0.001 0.000; 0.010 0.003 0.000 0.010];
S2 = [0.561  0.011 0.001 0.037;  0.011 0.025  0.004 0.007; 0.001 0.004 0.005 0.002; 0.037 0.007 0.002 0.019];
S3 = [0.261  0.030 0.003 0.018;  0.030 0.017 -0.000 0.006; 0.003 0.000 0.004 0.001; 0.018 0.006 0.001 0.013];

X_mean = ((nl_arr(1)*X1_mean + nl_arr(2)*X2_mean + nl_arr(3)*X3_mean)/n);

% Compute B matrix;
B1 = (X1_mean-X_mean)*(X1_mean-X_mean)';
B2 = (X2_mean-X_mean)*(X2_mean-X_mean)';
B3 = (X3_mean-X_mean)*(X3_mean-X_mean)';
B = nl_arr(1)*B1 + nl_arr(2)*B2 + nl_arr(3)*B3

% Compute W matrix;
W = nl_arr(1)*S1 + nl_arr(2)*S2 + nl_arr(3)*S3

T = B + W;
lambda = det(W)/det(T)

% Using table 6.3, p.303, to compute the exact test statistics

% Can also use Barlette correction if n is large!
% barlet_stat = -(n-1-(p+g)/2) * log (det(W)/det(T));
% crit_value  = chi2inv(0.99,p*(g-1));

test_stat = ((sum(nl_arr) - p - 2)/p)*((1-sqrt(lambda))/sqrt(lambda))
crit_value = finv(0.99, 2*p, 2*(n-p-2))
if test_stat <= crit_value  
    sprintf('No treatment effects. test_stat = %.2f <= %.2f', test_stat, crit_value)
else   
    sprintf('Treatment difference exist! test_stat = %.2f > %.2f', test_stat, crit_value)    
end 

tau1_est = X1_mean - X_mean;
tau3_est = X3_mean - X_mean;

tau13_est = tau1_est(3);
tau33_est = tau3_est(3);

point133_est = tau13_est - tau33_est;
error = tinv(1-0.05/(p*g*(g-1)),n-g) * sqrt((1/nl_arr(1) + 1/nl_arr(3))*W(3,3)/(n-g));
sprintf('tau13-tau33 belongs to [ %.4f, %.4f ]', point133_est-error, point133_est+ error)
