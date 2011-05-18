# Read in the data, and run "attach" to make the variables
# in the table available by name.

heights=read.table("Data\\Heights.txt",header=T)
attach(heights)

plot(Wife,Husband)

# fit the linear model of Husband~Wife.
heights.lm = lm(Husband~Wife)
abline(heights.lm)

# look at what's contained in a linear model.
names(heights.lm)

# what is the degree of freedom?  
heights.lm$df.residual
heights.lm$rank
length(Wife)

# Estimate regression coefficients

beta.1.hat <- cov(Husband, Wife) / var(Wife)
beta.0.hat <- mean(Husband) - beta.1.hat * mean(Wife)
print(c(beta.1.hat, beta.0.hat))

# Estimate sigma squared 

sigma.hat=sqrt(sum(resid(heights.lm)^2) / heights.lm$df.resid)
print(sigma.hat)

# plot the residuals.
plot(Wife, heights.lm$residuals)

# get a summary of the regression fit.
summary(heights.lm)


# ----- Calcium dataset -----#
calcium=read.table("CalciumBloodPressure.txt",header=T)
attach(calcium)
Treatment=(Treatment=="Calcium")
calcium.lm=lm(Decrease~Treatment);

plot(Treatment,Decrease)
abline(calcium.lm)


