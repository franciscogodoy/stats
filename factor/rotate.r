# A little demonstration, x2 is just x1 with noise,
# and same for x4 vs. x3 and x6 vs. x5
# Last four cases are there to add noise

	x1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
	x2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
	x3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
	x4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
	x5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
	x6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
	x <- cbind(x1,x2,x3,x4,x5,x6)

#factanal(x, factors, data = NULL, covmat = NULL, n.obs = NA,
#         subset, na.action, start = NULL,
#         scores = c("none", "regression", "Bartlett"),
#         rotation = "varimax", control = NULL, ...)

	cor(x)
	factanal(x, factors=3) # varimax is the default
	factanal(x, factors=3, rotation="promax")

	# The following shows the g factor as PC1
	prcomp(m1)

## formula interface
	factanal(~x1+x2+x3+x4+x5+x6, factors = 3,
      	   scores = "Bartlett")$scores


# example 9.9 
# rotated loadings for the consumer-preference data
	c1 = c(1, 0.02, 0.96, 0.42, 0.01)
	c2 = c(0.02, 1, 0.13, 0.71, 0.85);
	c3 = c(0.96, 0.13, 1, 0.50, 0.11);
	c4 = c(0.42, 0.71, 0.50, 1, 0.79);
	c5 = c(0.01, 0.85, 0.11, 0.79, 1);
	cor.mat = cbind(c1,c2,c3,c4,c5);
	factanal(x=NULL, data=NULL, covmat = cor.mat, factors=2, rotation="varimax");


