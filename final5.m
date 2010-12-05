% STAT 5531 - Multivariate statistical analysis
% Final exam - problem 5.a
% Thanh Doan 

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

