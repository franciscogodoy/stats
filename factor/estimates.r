	# clear workspace variables 
		rm(list=ls(all=TRUE));  

	# data = read.csv('a.data', header = TRUE, sep = ",");
		data = read.table('stockprice.data');
		names(data) = c('X1', 'X2', 'X3', 'X4', 'X5');
	
	# define a function to compute the est. factor loadings, 
	# communalities, uniqueness and total variance explained by factors
		pc.estimate = function(data, p, m) {
			cor.r = cor(data);
			eigen.pair = eigen(cor.r);
			row.names = row.names(cor.r);
			col.names = paste(c("F"), 1:m, sep="");
			dim.names = list(row.names, col.names);			
			loadings = matrix(0,p,m, dimnames = dim.names);			
			for (i in 1:m){
				loadings[,i] = sqrt(eigen.pair$values[i]) * eigen.pair$vectors[,i];
			}

			eigen.values = eigen.pair$values;
			eigen.values.proportion = eigen.pair$values / p;
			eigen.values.cum.proportion = cumsum(eigen.values.proportion);

			eigen.values.matrix = rbind(eigen.values, eigen.values.proportion, eigen.values.cum.proportion);
			eigen.row.names = c('Eigenvalues', 'propotion.of.total.variance', 'cummulitive.propotion');
			eigen.col.names = paste(c("F"), 1:p, sep="");
			dimnames(eigen.values.matrix) = list(eigen.row.names, eigen.col.names);

			LLt = loadings %*% t(loadings);
			specific.vars.vector = diag(cor.r - LLt);
			specific.vars.matrix = matrix(0, p, p);
			diag(specific.vars.matrix) = diag(cor.r - LLt);
			communalities = apply(loadings^2, 1, sum);

			ls = list(est.factor.loadings = round(loadings,3),  
					communalities = round(communalities,3), 
					specific.variances = round(specific.vars.vector,3), 
					specific.variances.matrix = round(specific.vars.matrix,3),
					sample.variance.explained = round(eigen.values.matrix[,1:m],3));
			ls;
		}

	# use pc.estimate() function to obtain the principal component solution to a factor model 
		pc.estimate(data, 5, 2); # 2 factor solution;
		pc.estimate(data, 5, 1); # 1 factor solution;


	# EXTRA-WORK: DETERMINING THE NUMBER OF FACTORS TO EXTRACT
	# A crucial decision in factor analysis is how many factors to extract. 
	# The nFactors package offer a suite of functions to aid in this decision.
		install.packages('nFactors');
		library(nFactors);
		ev = eigen(cor(data5)); # get eigenvalues;
		ap = parallel(subject=nrow(data5),var=ncol(data5),rep=100,cent=.05);
		nS = nScree(ev$values, ap$eigen$qevpea);
		plotnScree(nS);


	# Find the maximum likelihood estimates of L and Psi	
		mle.estimate = function(data, p, m) {
			estimate = factanal(data, factors = m, rotation = "none");
			loadml = loadings(estimate);
			class(loadml) = "matrix";
			uniqueml = estimate$uniquenesses
			specific.vars.matrix = matrix(0, p, p);
			diag(specific.vars.matrix) = uniqueml;
			ls = list(Loadings = round(loadml,3), Uniquenesses=round(uniqueml,3), 
					Specific.Variances= round(specific.vars.matrix,3));
			ls;
		}
  
	# use mle.estimate() function to obtain the MLE solution to a factor model 
		mle.estimate(data5, 7, 2); # 2 factor solution;

		# use standard R output which gives more information.
		print(factanal(data5, factors = 2, rotation = "none"));

		mle.estimate(data5, 7, 3); # 3 factor solution;

		# use standard R output which gives more information.
		print(factanal(data5, factors = 3, rotation = "none"));



