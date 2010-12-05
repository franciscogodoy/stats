# STAT 5531 Project
# Excercise 7.25 
# Thanh Doan - 0159701

	data725 = read.table('T7-6.DAT');
	names(data725) = list("y1", "y2", "z1", "z2", "z3", "z4", "z5");

##########################
#	Part a
##########################
	summary(ma1 <- lm(y1 ~ z1 + z2 + z3 + z4 + z5, data = data725));
	summary(ma2 <- lm(y1 ~ z1 + z2, data = data725));

	# compare model ma1 with simpler model ma2.
	step(ma1, k=log(100));
	anova(ma1, ma2);

	# p-value=0.04. This time I choose model 1.
	# Last time I choose ma2 because i was liberal. 

	# analyse the residuals 
	par(mfrow=c(2,1));
	plot (ma1);

	# 95% prediction interval for y1 for z0 = (1,1200,140,70,85)
	z0 = list(z1=1, z2=1200, z3=140, z4=70, z5=85);
	predict(ma2, z0, interval='prediction', level=0.95);

##########################
#	Part b
##########################
	summary(mb1 <- lm(y2 ~ z1 + z2 + z3 + z4 + z5, data = data725))
	step(mb1, k=log(100));
	summary(mb2 <- lm(y2 ~ z1 + z2 , data = data725));

	# compare model mb1 with simpler model mb2.
	anova(mb1, mb2);
	# p-value=0.1. Choose model mb2 because it is simpler. 

	# analyse the residuals 
	par(mfrow=c(2,1));
	plot (mb2);

	# 95% prediction interval for y2 for z0 = (1,1200,140,70,85);
	z0= list(z1=1, z2=1200, z3=140, z4=70, z5=85);
	predict(mb2, z0, interval='prediction', level=0.95);

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
