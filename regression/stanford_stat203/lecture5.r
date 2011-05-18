
# ------ Predictions -------
# You use the predict(...) function.
# This is a good example from the R manual.  It also has
# a good example of the function matplot, which can be useful.

x <- rnorm(15)
y <- x + rnorm(15)

# without new observations, these two are the same.
predict(lm(y ~ x))
fitted(lm(y~x))

new <- data.frame(x = seq(-3, 3, 0.5))
predict(lm(y ~ x), new, se.fit = TRUE)
pred.w.plim <- predict(lm(y ~ x), new, interval="prediction")
pred.w.clim <- predict(lm(y ~ x), new, interval="confidence")
matplot(new$x,cbind(pred.w.clim, pred.w.plim[,-1]),
        lty=c(1,2,2,3,3), type="l", ylab="predicted y")     


# ------ Diagnostics for multiple regression -------

# ------ Diagnostic Plots -----
 install.packages('car')
#
# you have to install this package above to use this example.
# this can be done with the GUI in R for Windows.
#
library(car)  # this library contains useful functions for plotting.

# Data example 1

X1=rnorm(100,0,1)
X3=rnorm(100,0,1)
X2=10*X1-X3+rnorm(100,0,0.25)
Y1=10*X1+X3+rnorm(100,0,0.25)
pairs(Y1~X1+X2)
Y1.lm=lm(Y1~X1+X2)

av.plot(Y1.lm,X1)
av.plot(Y1.lm,X3)

# Data example 2.
X1 = rnorm(100,0,1)
X2 = rnorm(100,2,1)
Y2=10*X1+X2^2+rnorm(100,0,0.25)
pairs(Y2~X1+X2)
Y2.lm=lm(Y2~X1+X2)

av.plot(Y2.lm,X1)
av.plot(Y2.lm,X2)

cr.plot(Y2.lm,X1)
cr.plot(Y2.lm,X2)

# A real data set.

races=read.table("Races.txt",header=T)
attach(races)

pairs(Time~Distance+Climb)
races.lm3=lm(Time~Distance+Climb)

plot(races.lm3)
av.plot(races.lm3,Distance)
av.plot(races.lm3,Climb)
cr.plot(races.lm3,Distance)
cr.plot(races.lm3,Climb)



