% Excercise 14.3
disp(data);
x = data(:,1);
n = data(:,2);
r = data(:,3);
plot(x,r./n,'x','LineWidth',2);
grid on;
xlabel('Load, x');
ylabel('Proportion of fasteners failing');

% MATLAB: Use the glmfit function to carry out the regression:
[b,dev,stats] = glmfit(x,[r n],'binomial','link','logit');

% Excercise 14.12
dev_resids = stats.residd;
normplot(dev_resids);


% b = glmfit(x,[y n],'binomial','link','probit');
yfit = glmval(b, x,'logit','size', n);
phat = yfit./n; 

pred_vals = 2*asin(sqrt(phat));
plot(pred_vals , phat,'o');

% print estimated coeeficients
disp(stats.beta)

%print deviance
disp(dev);

% print the t statistics
disp(stats.t);

% print the p-values
disp(stats.p);

% Fit a quadratic 
x2 = x.^2;
X = cat(2,x,x2);
[b2,dev2,stats2] = glmfit(X,[r n],'binomial','link','logit');

% print dev2
disp(dev2);

% print difference in deviance 
disp(dev-dev2);

%Ho: beta1=0. Get Wald statistic 
Z1 = stats2.beta(2)/stats2.se(2);
disp(Z1);
    
%Ho: beta2=0. Get Wald statistic 
Z2 = stats2.beta(3)/stats2.se(3);
disp(Z2);

%  get CI for beta1
ci.low = stats2.beta(2) - 1.96*stats2.se(2);
ci.high = stats2.beta(2) + 1.96*stats2.se(2);
fprintf('95 percent confidence interval for beta1: (%6.4f, %6.4f) \n',ci.low, ci.high);

%  get CI for beta2
beta2.ci.low = stats2.beta(3) - 1.96*stats2.se(3);
beta2.ci.high = stats2.beta(3) + 1.96*stats2.se(3);
fprintf('95 percent confidence interval for beta1: (%10.8f, %10.8f) \n',beta2.ci.low, beta2.ci.high);

[b3,dev3,stats3] = glmfit(X,y,'poisson','link','log');
