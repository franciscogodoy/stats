% compute distance between pairs of objects
% see http://www.mathworks.com/help/toolbox/stats/pdist.html
% for different metric options 

% Compute the ordinary Euclidean distance.
X = randn(100, 5);
D = pdist(X,'euclidean');  % euclidean distance

% reformat the distance vector into a matrix using the squareform function
squareform(D);

% Compute the Euclidean distance with each coordinate
% difference scaled by the standard deviation.
Dstd = pdist(X,'seuclidean');
 
% Use a function handle to compute a distance that weights
% each coordinate contribution differently.
Wgts = [.1 .3 .3 .2 .1];     % coordinate weights
weuc = @(XI,XJ,W)(sqrt(bsxfun(@minus,XI,XJ).^2 * W'));
Dwgt = pdist(X, @(Xi,Xj) weuc(Xi,Xj,Wgts));