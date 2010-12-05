
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
		# data5 = read.csv('final5.data', header = TRUE, sep = ",");

		data5 = read.table('final5.data', sep = ",");
		pollution.prcomp = prcomp(data5);
		print(pollution.prcomp);		
		summary(pollution.prcomp);
		print(round(pollution.prcomp$sdev^2,2));

	# produces a screeplot
	# plot(pollution.prcomp);
		plot(pollution.prcomp, type="lines");
	
	# produces a screeplot
		biplot(pollution.prcomp);


