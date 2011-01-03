% Final exam - problem 1 and 5
% Thanh Doan 

p1data = dataset('file','final1.data', 'delimiter',',');             
X  = [p1data.X1, p1data.X2];
[n,p] = size(X);

xbar = mean(X)';
S = cov(X);
S_inv = inv(S);

mu_prime = [6;12 ];
T_sq = n*(xbar-mu_prime)'*S_inv*(xbar-mu_prime);

critvalue = (p*(n-1)/(n-p)) * finv(.95,p,n-p);

% The direction and lengths of axes of ellipsoid
%   n * (xbar - mu)'*inv(S)*(xbar - mu) 
%       <= c^2 = [p(n-1)/(n-p)]*F(.95, p, n-p)     
% are:
[v d] = eig(S);
ellipse_center = xbar;
disp(ellipse_center);
for i = 1:size(d,2)
    lambda = d(i,i);
    half_len = sqrt(lambda) * sqrt( (p*(n-1)/(n*(n-p))) * finv(.95,p,n-p) );
    fprintf('Axis %d, half-length = %10.4f, direction:  \n', i, half_len);
    disp(v(:,i));    
end

theta = [0:0.1:360]';
theta = theta .* (pi/180);
a = sqrt(d(1,1))*sqrt(((p*(n-1))/(n*(n-p)))*finv(0.95,p,n-p)) .* v(:,1);
b = sqrt(d(2,2))*sqrt(((p*(n-1))/(n*(n-p)))*finv(0.95,p,n-p)) .* v(:,2);
for i = 1:length(theta)
    x(i) = xbar(1) + a(1)*sin(theta(i)) + b(1)*cos(theta(i));
    y(i) = xbar(2) + a(2)*sin(theta(i)) + b(2)*cos(theta(i));
end

plot(x,y);
hold on
axis equal
plot(xbar(1),xbar(2),'r*');
hold off


% problem 5.a
pollution_data = load('final5.data');

% Compute principal components for the data, 
%           and the variance accounted for by each component
% pc:       contains the coefficients of the linear combinations of the 
%           original variables that generate the principal components. 
% scores:   contains the coordinates of the original data in the new
%           coordinate system defined by the principal components. 
% variances:  is a vector containing the variance explained by the 
%             corresponding principal component.
% t2:       Hotelling's T2, a statistical measure of the multivariate 
%           distance of each observation from the center of the data set.
[pc,scores,variances,t2] = princomp(pollution_data);
disp(pc);

% plot first two columns of scores shows the data projected onto 
% the first two principal components. 
% princomp computes the scores to have mean zero.
plot(scores(:,1),scores(:,2),'+')
xlabel('1st Principal Component')
ylabel('2nd Principal Component')
gname();

% compute the percent of the total variability explained by each 
% principal component.
percent_explained = 100*variances/sum(variances);
disp(percent_explained);

% Use the pareto function to make a scree plot of the percent variability 
% explained by each principal component.

pareto(percent_explained)
xlabel('Principal Component')
ylabel('Variance Explained (%)')

% use Hotelling's T2, a statistical measure, as an analytical way to 
% find the most extreme points in the data.

[st2, index] = sort(t2,'descend'); % Sort in descending order.
extreme = index(1);
disp(extreme);

% biplot visualize both the principal component coefficients 
% for each variable and the principal component scores 
% for each observation in a single plot. 
biplot(pc(:,1:2), 'scores',scores(:,1:2));
axis([-1 1.1 -.4 1 1 1]);

%biplot in three dimensions
biplot(pc(:,1:3), 'scores',scores(:,1:3));
axis([-1 1.1 -0.4 1 -1 1]);
view([30 40]);

% problem 5b - factor analysis
[Loadings,specificVar,T,stats] = factoran(pollution_data,2,'rotate','none');

%Ref: http://www.mathworks.com/help/toolbox/stats/brkgqnt.html
