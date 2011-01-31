
#################### problem 2  #####################################
## a clinical study involves three characteristic measurements   #### 
##  in two groups                                                ####
#####################################################################

	rm(list=ls(all=TRUE));
	data2 = read.table('final2.data', header = TRUE, sep = ",");
	groups = factor(data2$Groups);
	Y = as.matrix(data2[,2:4]);

	# print Wilks statistics, F-value and p-value for each factor and interation	
	fit = manova(Y ~ groups );	
	summary(fit, test = c("Wilks"));

	# print matrix of sum of squares and cross products SSP
	summary(fit, test = c("Wilks"))$SS;

	xbar = by(Y, groups, mean);
	print(xbar);

	# 95% simultaneous confidence intervals 
	# for the differences in the mean components.
	n = 20;
	p = 3;
	g = 2;
	m = p*g*(g-1)/2;
	w =  summary(fit)$SS$Residuals;
	alpha = 0.05;
	n.control = 10;
	n.test = 10;
	this.qt = qt(1 - (alpha/(p*g*(g-1))), df = n-g);
	
	mean.control  = xbar$Control ;
	mean.test = xbar$Test;

	# Sim CI for differences between conrtol and test groups
	groups.diff = mean.control - mean.test;
	est.err = this.qt * sqrt((diag(w)/(n-g)) * (1/n.control + 1/n.test));
	sim.int.lwr = groups.diff - est.err;
	sim.int.upr = groups.diff + est.err;
	sim.intervals = list(difference.between.groups = 'Control and Test',
					interval.type = '95% simultaneous', 
					lower=sim.int.lwr,
					upper=sim.int.upr);
	print(sim.intervals);

#################### problem 3  #####################################
## Comparing the loss in weights of male and female mice         ####	
## under three different treatments.                             ####
## Four mice of each sex were randomly assigned                  ####
## to each of three treatments and weight losses were measured   ####
## at the end of the first and second weeks.                     ####
#####################################################################

	rm(list=ls(all=TRUE));
	data3 = read.table('final3.data', header = TRUE, sep = ",");
	sex = factor(data3$Sex);
	treatments = factor(data3$Treatments);
	Y = as.matrix(data3[,3:4]);

	# print Wilks statistics, F-value and p-value for each factor and interation	
	fit = manova(Y ~ sex * treatments);
	summary(fit, test = c("Wilks"));

	# the interaction term is not signicant. 
	# We can fit the model without interactions
	fit2 = manova(Y ~ sex + treatments);
	summary(fit2, test = c("Wilks"));

	# factor 1 (sex) is not signicant. 
	# We can fit the model without sex factor
	fit3 = manova(Y ~ treatments);
	summary(fit3, test = c("Wilks"));

	xbar = by(Y, treatments, mean);
	print(xbar);

	# Bonferroni simultaneous confidence intervals 
	n = 24;
	p = 2;
	g = 3;
	m = p*g*(g-1)/2;
	w =  summary(fit3)$SS$Residuals;
	alpha = 0.05;
	n.A = 8;
	n.B = 8;
	n.C = 8;
	this.qt = qt(1 - (alpha/(p*g*(g-1))), df = n-g);
	print(this.qt);
	
	mean.A = xbar$A;
	mean.B = xbar$B;
	mean.C = xbar$C;

	# Sim CI for differences between treatment B - A	
	diff.BA = mean.B - mean.A;
	err.BA = this.qt * sqrt((diag(w)/(n-g)) * (1/n.A + 1/n.B));
	sim.int.lwr.BA = diff.BA - err.BA;
	sim.int.upr.BA = diff.BA + err.BA;
	sim.intervals.BA = list(difference.between.treaments = 'B and A',
					interval.type = 'Bonferroni', 
					lower=sim.int.lwr.BA,
					upper=sim.int.upr.BA);
	print(sim.intervals.BA);

	# Sim CI for differences between treatment C - A	
	diff.CA = mean.C - mean.A;
	err.CA = this.qt * sqrt((diag(w)/(n-g)) * (1/n.A + 1/n.C));
	sim.int.lwr.CA = diff.CA - err.CA;
	sim.int.upr.CA = diff.CA + err.CA;
	sim.intervals.CA = list(difference.between.treaments = 'C and A',
					interval.type = 'Bonferroni', 
					lower=sim.int.lwr.CA,
					upper=sim.int.upr.CA);
	print(sim.intervals.CA);


	# Sim CI for differences between treatment C - B	
	diff.CB = mean.C - mean.B;
	err.CB = this.qt * sqrt((diag(w)/(n-g)) * (1/n.A + 1/n.B));
	sim.int.lwr.CB = diff.CB - err.CB;
	sim.int.upr.CB = diff.CB + err.CB;
	sim.intervals.CB = list(difference.between.treaments = 'C and B',
					interval.type = 'Bonferroni', 
					lower=sim.int.lwr.CB,
					upper=sim.int.upr.CB);
	print(sim.intervals.CB);





#################### problem 4  ####################################
##	In a regression study, there are three dependent variables  ##
##	and four independent variables.. 				      ##
####################################################################

	rm(list=ls(all=TRUE));
	data4 = read.table('final4.data', header = TRUE, sep = ",");
	
	##########################
	#	Part a
	##########################
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
		plot (m2y1);
		plot (m3y2);
		plot (m2y3);


		newobs = list(Z1 = 0.5, Z2 = 40, Z3 = 20, Z4 = 1);
		# 95% prediction interval for Y1
		predict(m2y1, newobs, interval='prediction', level=0.95);

		# 95% prediction interval for Y2
		predict(m3y2, newobs, interval='prediction', level=0.95);

		# 95% prediction interval for Y3
		predict(m2y3, newobs, interval='prediction', level=0.95);

	##########################
	#	Part b
	##########################
	Y = as.matrix(data4[1:3]);
	m1y = lm(Y ~ Z1 + Z2 + Z3 + Z4 , data = data4);	
	m2y = lm(Y ~ Z2 + Z3 + Z4 , data = data4);
	m3y = lm(Y ~ Z4 , data = data4);

	anova(m1y, m2y); 
	anova(m2y, m3y); 

	# plot residuals to check normality 
	e1 = rstudent(m2y)[,1];
	e2 = rstudent(m2y)[,2];
	e3 = rstudent(m2y)[,3];

	qqnorm(e1, main = "Q-Q Plot for y1 residuals"); qqline(e1, col = 'red');
	qqnorm(e2, main = "Q-Q Plot for y2 residuals"); qqline(e2, col = 'red');
	qqnorm(e3, main = "Q-Q Plot for y3 residuals"); qqline(e3, col = 'red');

	#################################################
	# simultaneous 95% prediction intervals for each 
	#	individual response Yi 
	##################################################
	m = qr(Y)$rank; 
	n = nrow(data4);
	r = m2y$rank-1;
	z = cbind(1, data4$Z2, data4$Z3, data4$Z4);
	z0 = c(1,  40, 20, 1);
	y.hat = z0 %*% coef(m2y); print(round(y.hat,2));
	qf.z = 1+ t(z0) %*% solve(t(z) %*% z) %*% z0; 
	error.mat = SSD(m2y)$SSD;  
	F.quant = qf(.95, m, n-r-m);  

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



