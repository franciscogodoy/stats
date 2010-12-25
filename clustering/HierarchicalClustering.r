# Fisher Iris Data
	data(iris);

	# listwise deletion of missing
		data <- na.omit(data);

	#standardize variables
	#	mydata <- scale(data[,1:4]);
		mydata = data[,1:4];


	# Ward Hierarchical Clustering

		# distance matrix
		d <- dist(mydata, method = "euclidean") 
		d <- dist(mydata, method = "maximum");
		fit <- hclust(d, method="ward") 
	
	# display dendogram	
		plot(fit) 

	# draw dendogram with red borders around the 5 clusters 
		groups <- cutree(fit, k=3); 

	# Cluster Plot against 1st 2 principal components
	# vary parameters for most readable graph
		library(cluster); 
		clusplot(mydata, groups, color=TRUE, shade=TRUE, labels=2, lines=0);
		clusplot(mydata, groups, color=TRUE, shade=TRUE, labels=1, lines=0);
		clusplot(mydata, groups, color=TRUE, shade=FALSE, labels=1, lines=0);

# The pvclust( ) function in the pvclust package provides p-values for hierarchical 
# clustering based on multiscale bootstrap resampling. 
# Clusters that are highly supported by the data will have large p values. 
# Interpretation details are provided Suzuki. 
# pvclust clusters columns, not rows. Transpose your data before using.
	mydata = t(mydata);

	# Ward Hierarchical Clustering with Bootstrapped p values
		utils:::menuInstallPkgs();
		library(pvclust);
		fit <- pvclust(mydata, method.hclust="ward", method.dist="euclidean");

	# dendogram with p values
		plot(fit); 

	# add rectangles around groups highly supported by the data
		pvrect(fit, alpha=.95);

# PLOTTING CLUSTER SOLUTIONS
	# K-Means Clustering with 3 clusters
		mydata <- scale(data[,1:4]);
		fit <- kmeans(mydata, 3);

	# Cluster Plot against 1st 2 principal components
	# vary parameters for most readable graph
		library(cluster); 
		clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0);

	# Centroid Plot against 1st 2 discriminant functions
		library(fpc);
		plotcluster(mydata, fit$cluster);

# VALIDATING CLUSTER SOLUTIONS
	# function cluster.stats() in the fpc package provides a mechanism for comparing the similarity 
	# of two cluster solutions using a variety of validation criteria (Hubert's gamma coefficient, 
	# the Dunn index and the corrected rand index)

	# comparing 2 cluster solutions
		library(fpc);
		cluster.stats(d, fit1$cluster, fit2$cluster);

		# where d is a distance matrix among objects, and fit1$cluster and fit$cluster are integer vectors 
		# containing classification results from two different clusterings of the same data.


# REF
# http://www.statmethods.net/advstats/cluster.html

