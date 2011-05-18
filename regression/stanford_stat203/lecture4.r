# --------------#
#   LECTURE 4   #
# --------------#


rivers<-read.table("Rivers.txt",header=T)
attach(rivers)


# Fit the linear model of Nitrogen on all of the predictors.
# The optional argument "data=rivers" tells R that all of the variables "Nitrogen",
# "Agr", etc.. come from the rivers data-frame, which avoids searching.
# The arument "subset=subset" means to use only a subset of the data in the regression.

rivers.lm1=lm(Nitrogen~Agr+Forest+Rsdntial+ComIndl)
summary(rivers.lm1)

influence.measures(rivers.lm1)

# This is a subset that excludes Neversink and Hackensack, which 
# are data points 4 and 5.  
subset=c(1:3,6:20)

# This is a way you can produce jpeg plots for your homeworks.
# You can also experiment with the "png" command.
#jpeg("rivers_del45_pairs.jpg",height=600,width=600);
pairs(Nitrogen~Agr+Forest+Rsdntial+ComIndl,subset=subset)
#dev.off()

# What if we took out the outliers?
rivers.lm1=update(rivers.lm1, subset=subset);

summary(rivers.lm1)

# Since the variables Agr and Forest are highly correlated, 
# and Rsdntial and ComIndl are highly correlated, try
# fitting the linear model on only Agr and Rsdntial.

rivers.lm2=lm(Nitrogen~Agr+Rsdntial, data=rivers,subset=subset)
summary(rivers.lm2)

# Now try updating the model to include the other variables.

rivers.lm3=update(rivers.lm2, .~.+Forest);
summary(rivers.lm3)

rivers.lm4=update(rivers.lm2, .~.+ComIndl);
summary(rivers.lm4)


rivers.Agr.lm=lm(Nitrogen~Agr,data=rivers,subset=subset)
anova(rivers.Agr.lm,rivers.lm2)
anova(rivers.lm2,rivers.lm3)
anova(rivers.Agr.lm,rivers.lm2,rivers.lm3,rivers.lm1)



# ------ Predictions -------
# You use the predict(...) function.
# This is a good example from the R manual.  It also has
# a good example of the function matplot, which can be useful.

x <- rnorm(15)
y <- x + rnorm(15)

# without new observations, these two are the same.
predict(lm(y ~ x))
fitted(lm(y~x))

plot(x,y)
lines(x,predict(lm(y~x)))


new <- data.frame(x = seq(-3, 3, 0.5))
predict(lm(y ~ x), new, se.fit = TRUE)
pred.w.plim <- predict(lm(y ~ x), new, interval="prediction")
pred.w.clim <- predict(lm(y ~ x), new, interval="confidence")
matplot(new$x,cbind(pred.w.clim, pred.w.plim[,-1]),
        lty=c(1,2,2,3,3), type="l", ylab="predicted y")     
points(x,y)


# Try using the function sink() to direct output to file.
sink("file.txt")
# to stop the sinking: 
sink()

