%% Part 1: Find integration of cos^2(x)dx from 0 to pi %%%%%%%%%%%%

n = 10^4;

% Plotting 
x = 0:0.01:pi;
y = cos(x) .^ 2;
plot(x,y)


% Method 1: Based on simulation of n = 10,000 trials 
X = rand(n,1);
X = X .* pi;
w = (cos(X) .^ 2) .* pi;
I_est1 = sum(w)/n;
fprintf('Numerical integration (method 1). I = %10.5f \n', I_est1);


% Method 2: 
xmax = pi;
fmax = 1;
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
I_est2 = (n_accept/n) * (fmax * xmax);
fprintf('Numerical integration (method 2). I = %10.5f \n', I_est2);
