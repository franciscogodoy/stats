%% Find integration of cos^2(x)dx from 0 to pi %%%%%%%%%%%%

clear;
clc;
n = 10^4;

% Plotting 
x = 0:0.01:pi;
y = cos(x) .^ 2;
plot(x,y)

% Method 1: Ef(w(Xi))
X = rand(n,1);
X = X .* pi;
w = (cos(X) .^ 2) .* pi;
I_est1 = sum(w)/n;
I_se = std(w)/sqrt(n);
I_delta = I_se * norminv(0.975, 0,1);
I_ci = I_est1 + [-1, 1] .* I_delta; 

% Method 2: 
m = 1000;
xmax = pi;
fmax = 1;
I_est = zeros(m,1);
for k=1:m
  n_accept = 0;
  for i=1:n
    r1 = rand(1,1);
    r2 = rand(1,1);
    xr = r1*xmax;
    fr = r2 * fmax;
    if (fr < (cos(xr) ^ 2))
      n_accept = n_accept +1;
    end
  end
  I_est(k) = (n_accept/n) * (fmax * xmax);
end

I_est2 = sum(I_est)/m;
I_se2 = std(I_est);
I_delta2 = I_se2 * norminv(0.975, 0,1);
I_ci2 = I_est2 + [-1, 1] .* I_delta2; 

fprintf('I.hat = %10.5f \n', I_est1);
fprintf('I.hat2 = %10.5f \n', I_est2);

disp('Method 1: 95 percent confidence inteval of I ');
disp(I_ci);

disp('Method 2: 95 percent confidence inteval of I ');
disp(I_ci2);
