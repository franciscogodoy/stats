% from
% http://www.mathworks.com/products/statistics/demos.html?file=/products/de
% mos/shipping/stats/clusterdemo.html
% make sure to replicate the result;

% load iris dataset 
seed = 931316785; rand('seed',seed); randn('seed',seed);
load fisheriris;

% K-Means is an iterative algorithm that assigns objects to clusters 
% so that the sum of distances from each object to its cluster centroid,  
% over all clusters, is a minimum.
[cidx2,cmeans2] = kmeans(meas,2,'display','iter');

% silhouette plot displays a measure of how close each point in one cluster 
% is to points in the neighboring clusters.
[cidx2,cmeans2] = kmeans(meas,2,'dist','sqeuclidean');
[silh2,h] = silhouette(meas,cidx2,'sqeuclidean');
cc cx7 6ty mn xf zMM >>

% Since the fourth measurement in these data, the petal width, is highly 
% correlated with the third measurement, the petal length,
% and so a 3-D plot of the first three measurements gives 
% a good representation of the data, without resorting to four dimensions. 
ptsymb = {'bs','r^','md','go','c+'};
for i = 1:2
    clust = find(cidx2==i);
    plot3(meas(clust,1),meas(clust,2),meas(clust,3),ptsymb{i});
    hold on
end
plot3(cmeans2(:,1),cmeans2(:,2),cmeans2(:,3),'ko');
plot3(cmeans2(:,1),cmeans2(:,2),cmeans2(:,3),'kx');
hold off
xlabel('Sepal Length'); ylabel('Sepal Width'); zlabel('Petal Length');
view(-137,10);
grid on

% we can increase the number of clusters to see if kmeans can find further 
% grouping structure in the data. 
% 'display' parameter to print out information about each iteration in the clustering algorithm.
[cidx3,cmeans3] = kmeans(meas,3,'display','iter');


% it is possible to reach a local minimum, 
% can use the optional 'replicates' parameter to overcome that problem. 
% if specify more than 1 replicate, kmeans repeats the clustering process 
% starting from different randomly selected centroids for each replicate.

[cidx3,cmeans3,sumd3] = kmeans(meas,3,'replicates',5,'display','final');

% silhouette plot for this three-cluster solution indicates that there is one cluster 
% that is well-separated, but that the other two clusters are not very
% distinct.
[silh3,h] = silhouette(meas,cidx3,'sqeuclidean');

ptsymb = {'bs','r^','md','go','c+'};
for i = 1:3
    clust = find(cidx3==i);
    plot3(meas(clust,1),meas(clust,2),meas(clust,3),ptsymb{i});
    hold on
end
plot3(cmeans3(:,1),cmeans3(:,2),cmeans3(:,3),'ko');
plot3(cmeans3(:,1),cmeans3(:,2),cmeans3(:,3),'kx');
hold off
xlabel('Sepal Length'); ylabel('Sepal Width'); zlabel('Petal Length');
view(-137,10);
grid on


% use a different distance. The cosine distance might make sense for our data 
% because it would ignore absolute sizes of the measurements, and only consider 
% their relative sizes. Two flowers that were different sizes, but which had similarly 
% shaped petals and sepals, might not be close with respect to squared Euclidean distance,
% but would be close with respect to cosine distance.

[cidxCos,cmeansCos] = kmeans(meas,3,'dist','cos');

% From the silhouette plot, these clusters appear to be only slightly
% better separated than those found using squared Euclidean distance.
[silhCos,h] = silhouette(meas,cidxCos,'cos');
[mean(silh2) mean(silh3) mean(silhCos)];


% the order of the clusters is different than in the previous silhouette plot. 
% This is because kmeans chooses initial cluster assignments at random.

for i = 1:3
    clust = find(cidxCos==i);
    plot3(meas(clust,1),meas(clust,2),meas(clust,3),ptsymb{i});
    hold on
end
hold off
xlabel('Sepal Length'); ylabel('Sepal Width'); zlabel('Petal Length');
view(-137,10);
grid on


% we know the species (the label) of each observation in the data, 
% we can compare the clusters discovered by kmeans to the actual species, 
% to see if the three species have discernibly different physical characteristics. 
% the below plot shows, the clusters created using cosine distance differ 
% from the species groups for only five of the flowers. 
% Those five points, plotted with stars, are all near the boundary of the upper two clusters.

subplot(1,1,1);
for i = 1:3
    clust = find(cidxCos==i);
    plot3(meas(clust,1),meas(clust,2),meas(clust,3),ptsymb{i});
    hold on
end
xlabel('Sepal Length'); ylabel('Sepal Width'); zlabel('Petal Length');
view(-137,10);
grid on
sidx = grp2idx(species);
miss = find(cidxCos ~= sidx);
plot3(meas(miss,1),meas(miss,2),meas(miss,3),'k*');
legend({'setosa','versicolor','virginica'},1);
hold off

% Ref:
% http://www.mathworks.com/products/statistics/demos.html?file=/products/demos/shipping/stats/clusterdemo.html#4
