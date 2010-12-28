% MDS produce a representation of data in a small number of dimensions. 
% MDS visualize how near points are to each other for many kinds of distance 
% or dissimilarity metrics 
% MDS does not require raw data, but only a matrix of pairwise distances 
% or dissimilarities.

% function cmdscale performs metric multidimensional scaling, 
% aka principal coordinates analysis. 
% cmdscale takes as an input a matrix of inter-point distances and 
% creates a configuration of points. 

% we try to reduce to new points in two or three dimensions, 
% and hope the Euclidean distances between new points reproduce the original 
% distance matrix. 

% Thus, a scatter plot of the points created by cmdscale provides a visual 
% representation of the original distances.

% To demo reconstructing a set of points from only their inter-point distances.
 % First, create some four dimensional points with a small component in their fourth coordinate, 
 % and reduce them to distances.
X = [ normrnd(0,1,10,3), normrnd(0,.1,10,1) ];
D = pdist(X,'euclidean');
%Format distance matrix
%D = squareform(D);

% cmdscale finds a configuration with those inter-point distances. 
% cmdscale accepts distances as either a square matrix, or, as a vector produced by pdist.
% Y is a matrix containing the reconstructed points. eigvals is a vector 
% containing the sorted eigenvalues of what is often referred to as 
% the "scalar product matrix," which, in the simplest case, is equal to Y*Y'. 
% The relative magnitudes of those eigenvalues indicate the relative contribution 
% of the corresponding columns of Y in reproducing the original distance 
[Y,eigvals] = cmdscale(D);
[eigvals (eigvals*100)/sum(abs(eigvals))];
disp(sum(eigvals(1:2))/sum(eigvals));
disp(sum(eigvals(1:3))/sum(eigvals));

% If eigvals contains only positive and zero eigenvalues, the columns of Y 
% corresponding to the positive eigenvalues provide an exact reconstruction 
% of D, in the sense that their inter-point Euclidean distances, 
% computed using pdist, for example, are identical to the values in D.

maxerr4 = max(abs(D - pdist(Y))) % exact reconstruction

% If two or three of the eigenvalues in eigvals are much larger than the rest, 
% then the distance matrix based on the corresponding columns of Y 
% nearly reproduces the original distance matrix D. In this sense, 
% those columns form a lower-dimensional representation that adequately 
% describes the data. 
% But Is not always possible to find a good low-dimensional reconstruction.

% Below the reconstruction in 3 dimensions reproduces D very well, but 
% the reconstruction in 2 dimensions has errors that are of the same order 
% of magnitude as the largest values in D.
maxerr3 = max(abs(D - pdist(Y(:,1:3))));
maxerr2 = max(abs(D - pdist(Y(:,1:2)))); 
max(max(D));

% negative eigenvalues indicate that the distances in D cannot be reproduced exactly. 
% there might not be any configuration of points whose inter-point 
% Euclidean distances are given by D. 
% If the largest negative eigenvalue is small in magnitude relative to the largest positive eigenvalues, 
% then the configuration returned by cmdscale might still reproduce D well.

% Nonclassical  Multidimensional Scaling
% mdscale performs nonclassical multidimensional scaling. 
% use mdscale either to visualize dissimilarity data for which no "locations" exist, 
% or to visualize high-dimensional data by reducing its dimensionality. 
% Take a matrix of dissimilarities as an input and produce a configuration of points. 
% mdscale offers a choice of different criteria to construct the configuration, 
% and allows missing data and weights.

% example: the cereal data include measurements on 10 variables describing 
% breakfast cereals. 
% use mdscale to visualize these data in two dimensions. 

load cereal.mat
X = [Calories Protein Fat Sodium Fiber ... 
    Carbo Sugars Shelf Potass Vitamins];

% Take a subset from a single manufacturer
mfg1 = strcmp('G',cellstr(Mfg));
X = X(mfg1,:);
size(X);

% use pdist to transform the 10-dimensional data into dissimilarities.  
% This code first standardizes the cereal data, and then uses city block 
% distance as a dissimilarity. The choice of transformation to dissimilarities 
% is application-dependent, and the choice here is only for simplicity. 
% In some applications, the original data are already in the form of dissimilarities.
dissimilarities = pdist(zscore(X),'cityblock');
disp(size(dissimilarities));

% mdscale perform metric MDS. Unlike cmdscale, we must specify the number 
% of dimensions, and the method to use to construct the output configuration. 
% Here use two dimensions. The metric STRESS criterion is a common method 
% for computing the output; 
% The second output from mdscale is the value of that criterion evaluated 
% for the output configuration. 
% It measures the how well the inter-point distances of the output configuration 
% approximate the original input dissimilarities:
[Y,stress2] = mdscale(dissimilarities,2,'criterion','metricstress');
disp(stress2);

% A scatterplot of the output represents the original 
% 10-dimensional data in 2 dimensions, 
% use the gname function to label selected points:
plot(Y(:,1),Y(:,2),'o','LineWidth',2);
gname(Name(mfg1));

% Nonmetric Multidimensional Scaling
% Metric scaling creates a configuration of points whose inter-point distances 
% approximate the given dissimilarities. This is sometimes too strict a
% requirement.

% non-metric scaling is designed to relax it a bit. Instead of trying to 
% approximate the dissimilarities themselves, non-metric scaling approximates 
% a nonlinear, but monotonic, transformation of them. 
% Because of the monotonicity, larger or smaller distances on a plot of 
% the output will correspond to larger or smaller dissimilarities, respectively. 
% However, the nonlinearity implies that mdscale only attempts to preserve 
% the ordering of dissimilarities. 
% Thus, there may be contractions or expansions of distances at different scales.

% use mdscale to perform nonmetric MDS in the same way as for metric scaling. 
% The nonmetric STRESS criterion is a common method for computing the output; 
% As with metric scaling, the second output from mdscale is the value of that 
% criterion evaluated for the output configuration. 
% For nonmetric scaling, however, it measures the how well the inter-point distances 
% of the output configuration approximate the disparities. 
% The disparities are returned in the third output. 
% They are the transformed values of the original dissimilarities:
 [Y,stress,disparities] = mdscale(dissimilarities,2,'criterion','stress');  
disp( size(disparities));

% check the fit of the output configuration to the dissimilarities, 
% and to understand the disparities, it helps to make a Shepard plot:

%This plot shows that mdscale has found a configuration of points in 2 dimensions 
%whose inter-point distances approximates the disparities, 
%which in turn are a nonlinear transformation of the original dissimilarities. 
%The concave shape of the disparities as a function of the dissimilarities 
%indicates that fit tends to contract small distances relative to the 
%corresponding dissimilarities. This may be perfectly acceptable in practice.
distances = pdist(Y);
[dum,ord] = sortrows([disparities(:) dissimilarities(:)]);
plot(dissimilarities,distances,'bo', ...
     dissimilarities(ord),disparities(ord),'r.-', ...
     [0 25],[0 25],'k-');
xlabel('Dissimilarities');
ylabel('Distances/Disparities');
legend({'Distances' 'Disparities' '1:1 Line'},...
       'Location','NorthWest');
   
% mdscale uses an iterative algorithm to find the output configuration, 
% the results can often depend on the starting point. 
% By default, mdscale uses cmdscale to construct an initial configuration, 
% and this choice often leads to a globally best solution. 
% However, it is possible for mdscale to stop at a configuration that is a 
% local minimum of the criterion. Such cases can be diagnosed and often 
% overcome by running mdscale multiple times with different starting points
% by using the 'start' and 'replicates' parameters. 
% The following code runs five replicates of MDS, each starting at a 
% different randomly-chosen initial configuration. 
% The criterion value is printed out for each replication; 
% mdscale returns the configuration with the best fit.

opts = statset('Display','final');
[Y,stress] =... 
mdscale(dissimilarities,2,'criterion','stress',... 
'start','random','replicates',5,'Options',opts);


    