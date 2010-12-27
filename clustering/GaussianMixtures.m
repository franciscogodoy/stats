% From: http://www.mathworks.com/help/toolbox/stats/bq_679x-24.html
%
% Specifying a mixture Model
% gmdistribution create Gaussian mixture models with specified means, 
% covariances, and mixture proportions
MU = [1 2;-3 -5]; % Means
SIGMA = cat(3,[2 0;0 .5],[1 0;0 1]); % Covariances
p = ones(1,2)/2; % Mixing proportions
obj = gmdistribution(MU,SIGMA,p);

% Use the methods pdf and cdf to compute values and visualize the object:
ezsurf(@(x,y)pdf(obj,[x y]),[-10 10],[-10 10]);
ezsurf(@(x,y)cdf(obj,[x y]),[-10 10],[-10 10]);

% Fitting a Model to Data.  
% Create Gaussian mixture models by fitting a parametric model with a
% specified number of components to data.

% The fit method of the gmdistribution class uses the syntax obj = gmdistribution.fit(X,k), 
% where X is a data matrix and k is the specified number of components. 
% Choosing a suitable number of components k is essential for creating a useful model of the data—
% too few components fails to model the data accurately; 
% too many components leads to an over-fit model with singular covariance matrices.

% to illustrates this approach.
% create some data from a mixture of two bivariate Gaussian distributions
% using the mvnrnd function
MU1 = [1 2];
SIGMA1 = [2 0; 0 .5];
MU2 = [-3 -5];
SIGMA2 = [1 0; 0 1];
X = [mvnrnd(MU1,SIGMA1,1000);
mvnrnd(MU2,SIGMA2,1000)];
scatter(X(:,1),X(:,2),10,'.');

% fit a two-component Gaussian mixture model:
options = statset('Display','final');
obj = gmdistribution.fit(X,2,'Options',options);

hold on
h = ezcontour(@(x,y)pdf(obj,[x y]),[-8 6],[-8 6]);
hold off
disp(obj.mu);
disp(obj.Sigma);
disp(obj.PComponents);

% The two-component model minimizes the Akaike information:
AIC = zeros(1,4);
obj = cell(1,4);
for k = 1:4
    obj{k} = gmdistribution.fit(X,k);
    AIC(k)= obj{k}.AIC;
end
[minAIC,numComponents] = min(AIC);

% Use the method random of the gmdistribution class to generate random data 
% from a Gaussian mixture model created with gmdistribution or fit.

% the following specifies a gmdistribution object consisting of a 
% two-component mixture of bivariate Gaussian distributions:
MU = [1 2;-3 -5];
SIGMA = cat(3,[2 0;0 .5],[1 0;0 1]);
p = ones(1,2)/2;
obj = gmdistribution(MU,SIGMA,p);

ezcontour(@(x,y)pdf(obj,[x y]),[-10 10],[-10 10])
hold on


% Use random (gmdistribution) to generate 1000 random values:
Y = random(obj,1000);
scatter(Y(:,1),Y(:,2),10,'.');


% Clustering with Gaussian Mixtures
% Gaussian mixture distributions can be used for clustering data.
% multivariate normal components of the fitted model can represent clusters.

% 1. To demonstrate the process, first generate some simulated data from a 
% mixture of two bivariate Gaussian distributions using the mvnrnd function:
mu1 = [1 2];
sigma1 = [3 .2; .2 2];
mu2 = [-1 -2];
sigma2 = [2 0; 0 1];
X = [mvnrnd(mu1,sigma1,200); mvnrnd(mu2,sigma2,100)];
scatter(X(:,1),X(:,2),10,'ko')

% 2. Fit a two-component Gaussian mixture distribution. 
% Here, we know the correct number of components to use. 
% In practice, with real data, this decision would require comparing models 
% with different numbers of components.
options = statset('Display','final');
gm = gmdistribution.fit(X,2,'Options',options);

% 3. Plot the estimated probability density contours for the two-component mixture 
% distribution. The two bivariate normal components overlap, but their peaks are distinct. 
% This suggests that the data could reasonably be divided into two clusters:
hold on
ezcontour(@(x,y)pdf(gm,[x y]),[-8 6],[-8 6]);
hold off

% 4. Partition the data into clusters using the cluster method for the
% fitted mixture distribution. The cluster method assigns each point 
% to one of the two components in the mixture distribution.
idx = cluster(gm,X);
cluster1 = (idx == 1);
cluster2 = (idx == 2);
scatter(X(cluster1,1),X(cluster1,2),10,'r+');
hold on
scatter(X(cluster2,1),X(cluster2,2),10,'bo');
hold off
legend('Cluster 1','Cluster 2','Location','NW')

P = posterior(gm,X);
scatter(X(cluster1,1),X(cluster1,2),10,P(cluster1,1),'+')
hold on
scatter(X(cluster2,1),X(cluster2,2),10,P(cluster2,1),'o')
hold off
legend('Cluster 1','Cluster 2','Location','NW')
clrmap = jet(80); colormap(clrmap(9:72,:))
ylabel(colorbar,'Component 1 Posterior Probability')

% An alternative is to use the posterior probabilities for "soft clustering". 
% Each point is assigned a membership score to each cluster. 
% Membership scores are the posterior probabilities, and describe 
% how similar each point is to each cluster's archetype, 
% The points can be ranked by their membership score in a given cluster:
[tmp,order] = sort(P(:,1));
plot(1:size(X,1),P(order,1),'r-',1:size(X,1),P(order,2),'b-');
legend({'Cluster 1 Score' 'Cluster 2 Score'},'location','NW');
ylabel('Cluster Membership Score');
xlabel('Point Ranking');


% Although a clear separation of the data is hard to see in a scatter plot 
% plotting the membership scores indicates that the fitted distribution 
% does a good job of separating the data into groups. 
% Very few points have scores close to 0.5.



% Soft clustering using a Gaussian mixture is similar to fuzzy K-means clustering, 
% which also assigns each point to each cluster a membership score. 
% Fuzzy K-means assumes that clusters are roughly spherical in shape, 
% and all of roughly equal size. 
% This is comparable to a Gaussian mixture distribution with a single covariance 
% matrix that is shared across all components, and is a multiple of the identity matrix. 
% In contrast, gmdistribution allows different covariance options. 
% The default is to estimate a separate, unconstrained covariance matrix for each component. 
% A more restricted option, closer to K-means, would be to estimate a shared, diagonal covariance matrix:
gm2 = gmdistribution.fit(X,2,'CovType','Diagonal', 'SharedCov',true);

% This covariance option is similar to fuzzy K-means clustering, 
% but provides more flexibility by allowing unequal variances for different variables

% compute the soft cluster membership scores without computing hard cluster assignments, 
% either using posterior, or as part of hard clustering, as the second output from cluster:

P2 = posterior(gm2,X); % equivalently [idx,P2] = cluster(gm2,X)
[tmp,order] = sort(P2(:,1));
plot(1:size(X,1),P2(order,1),'r-',1:size(X,1),P2(order,2),'b-');
legend({'Cluster 1 Score' 'Cluster 2 Score'},'location','NW');
ylabel('Cluster Membership Score');
xlabel('Point Ranking');

% Assigning New Data to Clusters
% In the previous example, fitting the mixture distribution to data using fit, 
% and clustering those data using cluster, are separate steps. 
% However, the same data are used in both steps. 
% we can use the cluster method to assign new data points to the clusters 
% found in the original data.

mu1 = [1 2];
sigma1 = [3 .2; .2 2];
mu2 = [-1 -2];
sigma2 = [2 0; 0 1];
X = [mvnrnd(mu1,sigma1,200); mvnrnd(mu2,sigma2,100)];
options = statset('Display','final');
gm = gmdistribution.fit(X,2,'Options',options);

% Y = random(gm,1000);
% or
Y = [mvnrnd(mu1,sigma1,50);mvnrnd(mu2,sigma2,25)];
idx = cluster(gm,Y);
cluster1 = (idx == 1);
cluster2 = (idx == 2);
scatter(Y(cluster1,1),Y(cluster1,2),10,'r+');
hold on
scatter(Y(cluster2,1),Y(cluster2,2),10,'bo');
hold off
legend('Cluster 1','Cluster 2','Location','NW')

% IRIS dataset 
load fisheriris;

% which model minimizes the Akaike information:
AIC = zeros(1,4);
obj = cell(1,4);
for k = 1:4
    obj{k} = gmdistribution.fit(meas,k);
    AIC(k)= obj{k}.AIC;
end
% the 3-cluster model minimizes  the Akaike information:
[minAIC,numComponents] = min(AIC); %

options = statset('Display','final');
gm3 = gmdistribution.fit(meas,3,'Options',options);
disp(gm3.mu);
disp(gm3.Sigma);
disp(gm3.PComponents);
[idx,tmp] = cluster(gm3,meas);
P3 = posterior(gm3,meas); 
disp(idx);
disp(crosstab(idx, species));

% Ref: http://www.mathworks.com/help/toolbox/stats/bq_679x-24.html

