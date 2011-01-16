% From http://www.mathworks.com/help/toolbox/stats/bsehyju-1.html
% Classifying query points based on their distance to points in a training
% data set. List of distance metrics are explained at
% http://www.mathworks.com/help/toolbox/stats/bsehyju-1.html

% k-Nearest Neighbor Search:

% Given a set X of n points and a distance function d, kNN search find the
% k closest points in X to a query point or set of points. 

% The kNN search technique and kNN-based algorithms are widely used as
% benchmark learning rules — the simplicity of the kNN search
% makes it easy to compare the results from other classification techniques
% to kNN results.

% Can use kNN search for machine learning algorithms, such as kNN
% classification, local weighted regression, missing data imputation and
% interpolation, and density estimation.

% Can use kNN search with many distance-based learning functions, such
% as K-means clustering.

% kNN Search use 2 Search objects; ExhaustiveSearcher and KDTreeSearcher
% object.

% kd-trees divide data into nodes with at most BucketSize (default is 50)
% points per node, based on coordinates.

% When we find the k-nearest neighbors to a given query point, knnsearch
% performs the following steps:

% (1) Determine the node to which the query point belongs. 
% (2) Find the closest k points within that node and its distance to the
%       query point. 
% (3) Choose all other nodes having any area that is within the same
%       distance, in any direction, from the query point to the kth closest
%       point. 
% (4) Search nodes within that range for any points closer to the query
%       point. 

% All search objects (KDTreeSearcher, ExhaustiveSearcher) have a knnsearch
% method specific to that class. This method performs a k-nearest neighbors
% search on your object in the most efficient way for that specific object type. 
% There is also a generic knnsearch function which performs the search
% without creating or using an object.

% To decide which type of search object is best for the data,
% consider:

% (1) If data have many (i.e. > 10) columns, then ExhaustiveSearcher may
%       perform better.
% (2) Is the dataset is  sparse? Use the ExhaustiveSearcher object.
% (3) If one of the following distance measures used, Use the
%       ExhaustiveSearcher object..
%         'seuclidean'
%         'mahalanobis'
%         'cosine'
%         'correlation'
%         'spearman'
%         'hamming'
%         'jaccard'
%         A custom distance function

% (4) if dataset is very large (but with fewer than 10 columns), use the
%       KDTreeSearcher object.
%
% (5) If searching for the nearest neighbors for a large number of query
%       points? Use the KDTreeSearcher object.

% Example: Classifying Query Data Using knnsearch

% Classify a new point based on the last two columns of the Fisher iris
% data. 

load fisheriris;
X = meas(:,3:4);
gscatter(X(:,1),X(:,2),species)
set(legend,'location','best')

% Plot a new point p:
p = [5 1.45];
line(p(1),p(2),'marker','x','color','k', 'markersize',10,'linewidth',2);

% find 10 points closest to p:
[n,d] = knnsearch(X,p,'k',10);
line(X(n,1),X(n,2),'color',[.5 .5 .5],'marker','o', 'linestyle','none','markersize',10);

% display 10 closest points
disp(X(n,:));

% To make duplicate values visible on the plot, use the following code:

% jitter to make repeated points visible
xj = x + .05*(rand(150,2)-.5); 
gscatter(xj(:,1),xj(:,2),species);

% The jittered points do not affect any analysis of the data, only the
% visualization. This example does not jitter the points.

% Make the axes equal so the calculated distances correspond to the
% apparent distances on the plot axis equal and zoom in to see the
% neighbors better:

set(gca,'xlim',[4.5 5.5],'ylim',[1 2]); axis square

% Now find the species of the 10 neighbors:
tabulate(species(n));

% Using majority vote of the 10 nearest neighbors, 
% can classify this new point as a versicolor.

% visually identify the neighbors by drawing a circle around the group of
% them:

% Define the center and diameter of a circle, based on the 
% location of the new point: 

ctr = p - d(end);
diameter = 2*d(end);
% Draw a circle around the 10 nearest neighbors:
h = rectangle('position',[ctr,diameter,diameter],...
   'curvature',[1 1]);
set(h,'linestyle',':')


Using the same dataset, find the 10 nearest neighbors to three new points:

newpoint2 = [5 1.45;6 2;2.75 .75];
gscatter(x(:,1),x(:,2),species);
legend('location','best');
[n,dd] = knnsearch(x,newpoint2,'k',10);
line(x(n,1),x(n,2),'color',[.5 .5 .5],'marker','o', 'linestyle','none','markersize',10);
line(newpoint2(:,1),newpoint2(:,2),'marker','x','color','k', 'markersize',10,'linewidth',2,'linestyle','none');

% Find the species of the 10 nearest neighbors for each new point:
tabulate(species(n(1,:)));
tabulate(species(n(2,:)));
tabulate(species(n(3,:)));


















    

