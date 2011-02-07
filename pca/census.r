#################### 5a ############################################
##		i. Construct the sample principal components	      ##
##		ii. Determine the proportion of the total sample      ##
##		    variance explained by the first                   ##
##		    few principal components.                         ##
####################################################################
	# clear workspace variables 
	# to avoid potential problems of refering to wrong dataset 
		rm(list=ls(all=TRUE));  

	# if the dataset contains variables names, use header = TRUE
	# data = read.csv('T8-5.DAT', header = TRUE, sep = ",");
		cencusdata = read.table('T8-5.DAT');
		cencus.prcomp = prcomp(cencusdata );
		print(cencus.prcomp);		
		summary(cencus.prcomp);
		print(round(cencus.prcomp$sdev^2,2));

	% manual spectral decomposition 
		covmat = cov(cencusdata);
		eigen(covmat)	

	# produces a screeplot
	# plot(pollution.prcomp);
		plot(cencus.prcomp, type="lines");
	
	# produces a biplot
		biplot(cencus.prcomp);

