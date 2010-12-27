% from: http://www.statmethods.net/advstats/cluster.html

# Model based clustering assume a variety of data models 
% apply maximum likelihood estimation and Bayes criteria to identify 
% the most likely model and number of clusters. 
% the Mclust( ) function in the mclust package selects the optimal model 
% according to BIC for EM initialized by hierarchical clustering 
% for parameterized Gaussian mixture models. 
% One chooses the model and number of clusters with the largest BIC. 
% See help(mclustModelNames) to details on the model chosen as best.


# MATLAB code
	# MU1 = [1 2];
	# SIGMA1 = [2 0; 0 .5];
	# MU2 = [-3 -5];
	# SIGMA2 = [1 0; 0 1];
	# X = [mvnrnd(MU1,SIGMA1,1000); mvnrnd(MU2,SIGMA2,1000)];

# Equavalent R code:
	Mu1 = c(1,2);
	Sigma1 <- matrix(c(2,0,0,.5),2,2);
	Mu2 = c(-3,-5);
	Sigma2 <- matrix(c(1,0,0,1),2,2);
	X = rbind(mvrnorm(n=1000, Mu1, Sigma1), mvrnorm(n=1000, Mu2, Sigma2));

# MATLAB code
	# fit a two-component Gaussian mixture model:
	# options = statset('Display','final');
	# gm = gmdistribution.fit(X,2,'Options',options);

	# display the best model
	# AIC = zeros(1,4);
	# obj = cell(1,4);
	# for k = 1:4
	#     obj{k} = gmdistribution.fit(X,k);
	#     AIC(k)= obj{k}.AIC;
	# end
	# [minAIC,numComponents] = min(AIC);


# Equavalent R code:
	install.packages("mclust");
	library(mclust);
	fit <- Mclust(X);
	
	# plot results 
	plot(fit, X);

	# display the best model
	print(fit); 
	print(fit$classification);
	print(fit$parameters);

	data(iris);
	mydata = data[,1:4];
	fit2 <- Mclust(mydata)
	plot(fit2, mydata) # plot results 
	print(fit2$classification);
	print(fit2$parameters);



