% %example 2.1
% Manual estimation of regression coefficients
y = data(:,2);
x = data(:,3);
n = size(x,1);

Sxx = sum(x.^2) - (sum(x)^2)/n;
Sxy = dot(x,y) - (sum(x) * sum(y))/n;
beta.hat1 = Sxy/Sxx;
beta.hat0 = mean(y) - beta.hat1 * mean(x);

%another way to estimate beta.hat1
covxy =  cov(x,y);
beta.hat1 = covxy(1,2)/var(x);

% compute the residuals
res = y - (beta.hat0 + beta.hat1 * x);

SST = sum(y.^2) - (sum(y)^2)/n;
SSE = SST - beta.hat1 * Sxy;
sigma.hat.square = SSE/(n-2);



% Use matlab stat toolbox
X = [ones(n,1), x];
[b,bint,r,rint,stats] = regress(y,X);
%regstats(y,x)


