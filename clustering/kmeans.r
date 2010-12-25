
# Fisher Iris Data
	data(iris);

	# listwise deletion of missing
		data <- na.omit(data);

	#standardize variables
		mydata <- scale(data[,1:4]);

	# K-means clustering requires the analyst to specify the number of clusters
 	# A plot of the within groups sum of squares by number of clusters extracted 
	# can help determine the appropriate number of clusters. 
	# The analyst looks for a bend in the plot similar to a scree test in factor analysis.

	# Determine number of clusters
		wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var));
		for (i in 2:15) wss[i] <- sum(kmeans(mydata, centers=i)$withinss);
		plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares");


	# K-Means Cluster Analysis. 3 cluster solution
		fit <- kmeans(mydata, 3);

	# get cluster means 
		aggregate(mydata,by=list(fit$cluster),FUN=mean)
	# append cluster assignment
		mydata <- data.frame(mydata, fit$cluster)

	# cross table;
		table(fit$cluster, data$species);

	# Cluster Plot against 1st 2 principal components
	# vary parameters for most readable graph
		library(cluster); 
		clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0);

	# Centroid Plot against 1st 2 discriminant functions
		library(fpc);
		plotcluster(mydata, fit$cluster);

# a 2-dimensional example
	x <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2),
                 matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2));

		colnames(x) <- c("x", "y");
		(cl <- kmeans(x, 2));
		plot(x, col = cl$cluster);
		points(cl$centers, col = 1:2, pch = 8, cex=2);

	## random starts do help here with too many clusters
		(cl <- kmeans(x, 5, nstart = 25));
		plot(x, col = cl$cluster);
		points(cl$centers, col = 1:5, pch = 8);

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


# A robust version of K-means based on mediods can be invoked by using pam( ) instead of kmeans( ). 
# The function pamk( ) in the fpc package is a wrapper for pam that also prints 
# the suggested number of clusters based on optimum average silhouette width.
	http://cran.r-project.org/web/packages/fpc/fpc.pdf


# REF
# http://www.statmethods.net/advstats/cluster.html
