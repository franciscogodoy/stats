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
	# data = read.csv(mydata.data', header = TRUE, sep = ",");

		houston = read.table('airpollution.data', sep = ",");
		pollution.prcomp = prcomp(houston);
		print(pollution.prcomp);		
		summary(pollution.prcomp);

		print(round(pollution.prcomp$sdev^2,2));

	# produces a screeplot
	# plot(pollution.prcomp);
		plot(pollution.prcomp, type="lines");
	
	# produces a biplot
		biplot(pollution.prcomp);

