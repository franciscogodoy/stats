
#################### problem 4  ####################################
##	In a regression study, there are three dependent variables  ##
##	and four independent variables.. 				      ##
####################################################################

	data4 = read.table('final4.data', header = TRUE, sep = ",");
	
	## Part 4a-i ##################################################

		#### fit linear model for y1
		m1y1 = lm(Y1 ~ Z1 + Z2 + Z3 + Z4,  data = data4);
		summary(m1y1);
		
		# stepwise suggest drop Z1 from the model  			
		step(m1y1);			# stepwise using AIC criteria
		step(m1y1, k=log(42));	# stepwise using BIC criteria 

		m2y1 = lm(Y1 ~ Z2 + Z3 + Z4,  data = data4);
		summary(m2y1);

		# compare model m1y1 with simpler model m2y1.	
		# anova also suggest the same thing as setpwise does	
		anova(m1y1, m2y1);

		#### fit linear model for y2
		m1y2 = lm(Y2 ~ Z1 + Z2 + Z3 + Z4,  data = data4);
		summary(m1y2);
		
		# stepwise (using AIC) suggest keeping Z1 and Z4 in the model  			
		# stepwise (using BIC) suggest keeping only Z4 in the model  			
		step(m1y2);			# stepwise using AIC criteria
		step(m1y2, k=log(42));	# stepwise using BIC criteria 

		m2y2 = lm(Y2 ~ Z1 + Z4,  data = data4);
		summary(m2y2);

		m3y2 = lm(Y2 ~ Z4,  data = data4);
		summary(m3y2);

		# compare model m1y2 with simpler model m2y2.	
		# anova also suggest the same thing as setpwise does	
		anova(m1y2, m2y2);
		anova(m2y2, m3y2);


		#### fit linear model for y3
		m1y3 = lm(Y3 ~ Z1 + Z2 + Z3 + Z4,  data = data4);
		summary(m1y3);
		
		# stepwise (using AIC) suggest dropping Z1 from model
		# stepwise (using BIC) suggest keeping Z2,Z3,Z4 in the model 
		step(m1y3);			# stepwise using AIC criteria
		step(m1y3, k=log(42));	# stepwise using BIC criteria 

		m2y3 = lm(Y3 ~ Z2 + Z3 + Z4,  data = data4);
		summary(m2y3);

		# compare model m1y3 with simpler model m2y3.	
		# anova also suggest the same thing as setpwise does	
		anova(m1y3, m2y3);


		# analyse the residuals 
		par(mfrow=c(2,1));
		plot (m4a2);

		# 95% prediction interval for Y1
		newobs = list(Z1 = 0.5, Z2 = 40, Z3 = 20, Z4 = 1);
		predict(m4a2, newobs, interval='prediction', level=0.95);


##########################
#	Part c
##########################
	y = as.matrix(data725[1:2]);
	summary(mc1 <- lm(y ~ z1 + z2 + z3 + z4 + z5, data = data725));
	summary(mc2 <- lm(y ~ z1 + z2 , data = data725));
	anova(mc1, mc2); # p = 0.198 so choose mc2, a simpler model.

	# plot residuals to check normality 
	e1 = rstudent(mc2)[,1];
	e2 = rstudent(mc2)[,2];
	par(mfrow=c(2,1));
	qqnorm(e1, main = "Q-Q Plot for y1 residuals"); qqline(e1, col = 'red');
	qqnorm(e2, main = "Q-Q Plot for y2 residuals"); qqline(e2, col = 'red');

	#################################################
	# construct prediction ellipse with (7-42, p. 399) 
	##################################################
	m = qr(y)$rank; 
	n = nrow(data725);
	r = mc2$rank-1;

	z = cbind(1, data725$z1, data725$z2);
	z0 = c(1, 1, 1200);

	y.hat = z0 %*% coef(mc2); print(round(y.hat,2));

	qf.z = 1+ t(z0) %*% solve(t(z) %*% z) %*% z0; 
	print(round(qf.z,2));

	error.mat = SSD(mc2)$SSD;  print(round(error.mat,2));
	F.quant = qf(.95, m, n-r-m);  print(round(F.quant,2));

	# compute the eigen values and eigen vectors of error.mat
	# to determine the orientation and lengths of major and 
	# minor axes of the prediction ellipse 
	eigen(error.mat);

	# Now we have all information needed to calculate a 95% prediction
	# ellipse for y=(y1,y2) at (z1,z2)=(1,1200) using equation (7-42):
	# (y-y.hat) %*% ((n-r-1) * solve(error.mat)) %*% t(y-y.hat)
	#     <= qf.z * (m*(n-r-1)/(n-r-m)) * F.quant

	#################################################
	# print simultanous prediction intervals at z0 
	##################################################

	sqrt1 = sqrt(  (m*(n-r-1)/(n-r-m)) * F.quant );
	sqrt2 = sqrt(  qf.z * diag(error.mat) / (n-r-1) );
	int_err =  sqrt1 * sqrt2;

	lwr.bound = y.hat - int_err;
	upr.bound = y.hat + int_err;

	pred_ints = data.frame(t(lwr.bound), t(y.hat), t(upr.bound));
	names(pred_ints) = list("lwr", "center", "upr");
	print(round(pred_ints,2));




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

#################### 5b ############################################
##      Factor Analysis from the sample correlation matrix R      ##
##		i. Obtain the principal component solution            ##
##		ii. Find the maximum likelihood estimates of L and 	##
####################################################################
	# clear workspace variables 
	# to avoid potential problems of refering to wrong dataset 
		rm(list=ls(all=TRUE));  

	# if the dataset contains variables names, use header = TRUE
	# data5 = read.csv('final5.data', header = TRUE, sep = ",");
		data5 = read.table('final5.data', sep = ",");
		names(data5) = c('X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7');
	
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
		pc.estimate(data5, 7, 2); # 2 factor solution;
		pc.estimate(data5, 7, 3); # 3 factor solution;


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



